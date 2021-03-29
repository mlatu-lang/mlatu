{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Mlatu.Parse
-- Description : Parsing from tokens to terms
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Parse
  ( generalName,
    fragment,
  )
where

import Data.List (findIndex)
import Data.Map.Strict qualified as Map
import Mlatu.Class (Class (..), Method (..))
import Mlatu.Class qualified as Class
import Mlatu.DataConstructor (DataConstructor (DataConstructor))
import Mlatu.DataConstructor qualified as DataConstructor
import Mlatu.Definition (PermissionDefinition (..), WordDefinition (..))
import Mlatu.Definition qualified as Definition
import Mlatu.Desugar.Data qualified as Data
import Mlatu.Element (Element)
import Mlatu.Element qualified as Element
import Mlatu.Entry.Merge qualified as Merge
import Mlatu.Entry.Parameter (Parameter (Parameter))
import Mlatu.Fragment (Fragment)
import Mlatu.Fragment qualified as Fragment
import Mlatu.Ice (ice)
import Mlatu.Informer (Informer (..), errorCheckpoint)
import Mlatu.Instance (Instance (..))
import Mlatu.Instance qualified as Instance
import Mlatu.Intrinsic (Intrinsic (..))
import Mlatu.Intrinsic qualified as Intrinsic
import Mlatu.Kind (Kind (..))
import Mlatu.Located (Located)
import Mlatu.Located qualified as Located
import Mlatu.Metadata (Metadata (Metadata))
import Mlatu.Metadata qualified as Metadata
import Mlatu.Monad (M)
import Mlatu.Name
  ( GeneralName (..),
    Qualified (Qualified),
    Qualifier (Qualifier),
    Root (Absolute, Relative),
    Unqualified (..),
  )
import Mlatu.Operator qualified as Operator
import Mlatu.Origin (Origin)
import Mlatu.Origin qualified as Origin
import Mlatu.Parser (Parser, getTokenOrigin, parserMatch, parserMatch_)
import Mlatu.Report qualified as Report
import Mlatu.Signature (Signature)
import Mlatu.Signature qualified as Signature
import Mlatu.Term (Case (..), Else (..), MatchHint (..), Term (..), Value (..), compose)
import Mlatu.Term qualified as Term
import Mlatu.Token (Token)
import Mlatu.Token qualified as Token
import Mlatu.Tokenize (tokenize)
import Mlatu.TypeDefinition (TypeDefinition (TypeDefinition))
import Mlatu.TypeDefinition qualified as TypeDefinition
import Mlatu.Vocabulary qualified as Vocabulary
import Optics
import Relude hiding (Compose, Constraint)
import Relude.Unsafe qualified as Unsafe
import Text.Parsec ((<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos (SourcePos)

-- | Parses a program fragment.
fragment ::
  -- | Initial source line (e.g. for REPL offset).
  Int ->
  -- | Source file path.
  FilePath ->
  -- | List of permissions granted to @main@.
  [GeneralName] ->
  -- | Override name of @main@.
  Maybe Qualified ->
  -- | Input tokens.
  [Located Token] ->
  -- | Parsed program fragment.
  M (Fragment ())
fragment line path mainPermissions mainName tokens =
  let parsed =
        Parsec.runParser
          (fragmentParser mainPermissions mainName)
          Vocabulary.global
          path
          tokens
   in case parsed of
        Left parseError -> do
          report $ Report.parseError parseError
          halt
        Right result -> pure (Data.desugar (insertMain result))
  where
    isMain = (fromMaybe Definition.mainName mainName ==) . view Definition.wordName
    insertMain f = case find isMain $ view Fragment.wordDefinitions f of
      Just {} -> f
      Nothing ->
        over
          Fragment.wordDefinitions
          ( Definition.main
              mainPermissions
              mainName
              (Term.identityCoercion () (Origin.point path line 1))
              :
          )
          f

-- | Parses only a name.
generalName :: (Informer m) => Int -> FilePath -> Text -> m GeneralName
generalName line path text = do
  tokens <- tokenize line path text
  errorCheckpoint
  let parsed = Parsec.runParser nameParser Vocabulary.global path tokens
  case parsed of
    Left parseError -> do
      report $ Report.parseError parseError
      halt
    Right (name, _) -> pure name

fragmentParser ::
  [GeneralName] -> Maybe Qualified -> Parser (Fragment ())
fragmentParser mainPermissions mainName =
  partitionElements mainPermissions mainName
    <$> elementsParser <* Parsec.eof

elementsParser :: Parser [Element ()]
elementsParser = asum <$> many (vocabularyParser <|> one <$> elementParser)

partitionElements ::
  [GeneralName] ->
  Maybe Qualified ->
  [Element ()] ->
  Fragment ()
partitionElements mainPermissions mainName = rev . foldr go mempty
  where
    rev :: Fragment () -> Fragment ()
    rev f =
      over Fragment.intrinsics reverse $
        over Fragment.instances reverse $
          over Fragment.classes reverse $
            over Fragment.wordDefinitions reverse $
              over Fragment.permissionDefinitions reverse $
                over Fragment.metadata reverse $
                  over Fragment.types reverse f

    go :: Element () -> Fragment () -> Fragment ()
    go e acc = case e of
      Element.Intrinsic x -> over Fragment.intrinsics (x :) acc
      Element.Instance x -> over Fragment.instances (x :) acc
      Element.Class x -> over Fragment.classes (x :) acc
      Element.WordDefinition x -> over Fragment.wordDefinitions (x :) acc
      Element.PermissionDefinition x -> over Fragment.permissionDefinitions (x :) acc
      Element.Metadata x -> over Fragment.metadata (x :) acc
      Element.TypeDefinition x -> over Fragment.types (x :) acc
      Element.Term x ->
        over
          Fragment.wordDefinitions
          ( \defs ->
              case findIndex
                ((== fromMaybe Definition.mainName mainName) . view Definition.wordName)
                defs of
                Just index -> case splitAt index defs of
                  (a, existing : b) ->
                    a
                      ++ over Definition.wordBody (`composeUnderLambda` x) existing :
                    b
                  _nonMain -> ice "Mlatu.Parse.partitionElements - cannot find main definition"
                Nothing ->
                  Definition.main mainPermissions mainName x : defs
          )
          acc
        where
          -- In top-level code, we want local parameteriable bindings to remain in scope even
          -- when separated by other top-level program elements, e.g.:
          --
          --     1 -> x;
          --     define f (int -> int) { (+ 1) }
          --     x say  // should work
          --
          -- As such, when composing top-level code, we extend the scope of lambdas to
          -- include subsequent expressions.

          composeUnderLambda :: Term () -> Term () -> Term ()
          composeUnderLambda (Lambda typ name parameterType body origin) term =
            Lambda typ name parameterType (composeUnderLambda body term) origin
          composeUnderLambda a b = Compose () a b

vocabularyParser :: Parser [Element ()]
vocabularyParser = (<?> "vocabulary definition") $ do
  parserMatch_ Token.Vocab
  original@(Qualifier _ outer) <- Parsec.getState
  (vocabularyName, _) <- nameParser <?> "vocabulary name"
  let (inner, name) = case vocabularyName of
        QualifiedName
          (Qualified (Qualifier _root qualifier) (Unqualified unqualified)) ->
            (qualifier, unqualified)
        UnqualifiedName (Unqualified unqualified) -> ([], unqualified)
        LocalName {} -> ice "Mlatu.Parse.vocabularyParser - local name should not appear as vocabulary name"
  Parsec.putState (Qualifier Absolute (outer ++ inner ++ [name]))
  Parsec.choice
    [ [] <$ parserMatchOperator ";",
      do
        es <- blockedParser elementsParser
        Parsec.putState original
        pure es
    ]

blockedParser :: Parser a -> Parser a
blockedParser =
  Parsec.between
    (parserMatch Token.BlockBegin)
    (parserMatch Token.BlockEnd)

groupedParser :: Parser a -> Parser a
groupedParser =
  Parsec.between
    (parserMatch Token.GroupBegin)
    (parserMatch Token.GroupEnd)

groupParser :: Parser (Term ())
groupParser = do
  origin <- getTokenOrigin
  groupedParser $ Group . compose () origin <$> Parsec.many1 termParser

bracketedParser :: Parser a -> Parser a
bracketedParser =
  Parsec.between
    (parserMatch Token.VectorBegin)
    (parserMatch Token.VectorEnd)

nameParser :: Parser (GeneralName, Operator.Fixity)
nameParser = (<?> "name") $ do
  global <-
    isJust
      <$> Parsec.optionMaybe
        (parserMatch Token.Ignore <* parserMatch Token.VocabLookup)
  parts <-
    Parsec.choice
      [ (,) Operator.Postfix <$> wordNameParser,
        (,) Operator.Infix <$> operatorNameParser
      ]
      `Parsec.sepBy1` parserMatch Token.VocabLookup
  pure $ case parts of
    [(fixity, unqualified)] ->
      ( ( if global
            then QualifiedName . Qualified Vocabulary.global
            else UnqualifiedName
        )
          unqualified,
        fixity
      )
    _list ->
      let parts' = (\(Unqualified part) -> part) . snd <$> parts
          qualifier = Unsafe.fromJust (viaNonEmpty init parts')
          (fixity, unqualified) = Unsafe.fromJust (viaNonEmpty last parts)
       in ( QualifiedName
              ( Qualified
                  (Qualifier (if global then Absolute else Relative) qualifier)
                  unqualified
              ),
            fixity
          )

unqualifiedNameParser :: Parser Unqualified
unqualifiedNameParser =
  (<?> "unqualified name") $
    wordNameParser <|> operatorNameParser

wordNameParser :: Parser Unqualified
wordNameParser = (<?> "word name") $
  parseOne $
    \token -> case Located.item token of
      Token.Word name -> Just name
      _nonWord -> Nothing

operatorNameParser :: Parser Unqualified
operatorNameParser = (<?> "operator name") $ do
  name <- parseOne $ \token -> case Located.item token of
    Token.Operator (Unqualified name) -> Just name
    _nonUnqualifiedOperator -> Nothing
  pure $ Unqualified name

parseOne :: (Located Token -> Maybe a) -> Parser a
parseOne = Parsec.tokenPrim show advance
  where
    advance :: SourcePos -> t -> [Located Token] -> SourcePos
    advance _ _ (token : _) = Origin.begin $ Located.origin token
    advance sourcePos _ _ = sourcePos

elementParser :: Parser (Element ())
elementParser =
  (<?> "top-level program element") $
    Parsec.choice
      [ Element.WordDefinition <$> basicDefinitionParser,
        Element.PermissionDefinition <$> permissionParser,
        Element.Instance <$> instanceParser,
        Element.Class <$> classParser,
        Element.Intrinsic <$> intrinsicParser,
        Element.Metadata <$> metadataParser,
        Element.TypeDefinition <$> typeDefinitionParser,
        do
          origin <- getTokenOrigin
          Element.Term . compose () origin <$> Parsec.many1 termParser
      ]

metadataParser :: Parser Metadata
metadataParser = (<?> "metadata block") $ do
  origin <- getTokenOrigin <* parserMatch Token.About
  -- FIXME: This only allows metadata to be defined for elements within the
  -- current vocabulary.
  name <-
    Qualified <$> Parsec.getState
      <*> Parsec.choice
        [ unqualifiedNameParser <?> "word identifier",
          (parserMatch Token.Type *> wordNameParser)
            <?> "'type' and type identifier"
        ]
  fields <-
    blockedParser $
      many $
        (,)
          <$> (wordNameParser <?> "metadata key identifier")
          <*> (blockParser <?> "metadata value block")
  pure
    Metadata
      { Metadata._fields = Map.fromList fields,
        Metadata._name = QualifiedName name,
        Metadata._origin = origin
      }

typeDefinitionParser :: Parser TypeDefinition
typeDefinitionParser = (<?> "type definition") $ do
  origin <- getTokenOrigin <* parserMatch Token.Type
  (name, fixity) <- qualifiedNameParser <?> "type definition name"
  case fixity of
    Operator.Infix -> Parsec.unexpected "type-level operator"
    Operator.Postfix -> pass
  parameters <- Parsec.option [] quantifierParser
  constructors <- blockedParser $ many constructorParser
  pure
    TypeDefinition
      { TypeDefinition._constructors = constructors,
        TypeDefinition._name = name,
        TypeDefinition._origin = origin,
        TypeDefinition._parameters = parameters
      }

constructorParser :: Parser DataConstructor
constructorParser = (<?> "constructor definition") $ do
  origin <- getTokenOrigin <* parserMatch Token.Case
  name <- wordNameParser <?> "constructor name"
  fields <-
    (<?> "constructor fields") $
      Parsec.option [] $
        groupedParser constructorFieldsParser
  pure
    DataConstructor
      { DataConstructor._fields = fields,
        DataConstructor._name = name,
        DataConstructor._origin = origin
      }

constructorFieldsParser :: Parser [Signature]
constructorFieldsParser = typeParser `Parsec.sepEndBy` commaParser

typeParser :: Parser Signature
typeParser = Parsec.try functionTypeParser <|> basicTypeParser <?> "type"

functionTypeParser :: Parser Signature
functionTypeParser = (<?> "function type") $ do
  (effect, origin) <-
    Parsec.choice
      [ stackSignature,
        arrowSignature
      ]
  perms <- permissions
  pure (effect perms origin)
  where
    stackSignature :: Parser ([GeneralName] -> Origin -> Signature, Origin)
    stackSignature = (<?> "stack function type") $ do
      leftparameter <- UnqualifiedName <$> stack
      leftTypes <- Parsec.option [] (commaParser *> left)
      origin <- arrow
      rightparameter <- UnqualifiedName <$> stack
      rightTypes <- Parsec.option [] (commaParser *> right)
      pure
        ( Signature.StackFunction
            (Signature.Variable leftparameter origin)
            leftTypes
            (Signature.Variable rightparameter origin)
            rightTypes,
          origin
        )
      where
        stack :: Parser Unqualified
        stack = Parsec.try $ wordNameParser <* parserMatch Token.Ellipsis

    arrowSignature :: Parser ([GeneralName] -> Origin -> Signature, Origin)
    arrowSignature = (<?> "arrow function type") $ do
      leftTypes <- left
      origin <- arrow
      rightTypes <- right
      pure (Signature.Function leftTypes rightTypes, origin)

    permissions :: Parser [GeneralName]
    permissions =
      (<?> "permission labels") $
        many $ parserMatchOperator "+" *> (fst <$> nameParser)

    left, right :: Parser [Signature]
    left = basicTypeParser `Parsec.sepEndBy` commaParser
    right = typeParser `Parsec.sepEndBy` commaParser

    arrow :: Parser Origin
    arrow = getTokenOrigin <* parserMatch Token.Arrow

commaParser :: Parser ()
commaParser = void $ parserMatch Token.Comma

basicTypeParser :: Parser Signature
basicTypeParser = (<?> "basic type") $ do
  prefix <-
    Parsec.choice
      [ quantifiedParser $ groupedParser typeParser,
        Parsec.try $ do
          origin <- getTokenOrigin
          (name, fixity) <- nameParser
          -- Must be a word, not an operator, but may be qualified.
          guard $ fixity == Operator.Postfix
          pure $ Signature.Variable name origin,
        groupedParser typeParser
      ]
  let apply a b = Signature.Application a b $ Signature.origin prefix
  mSuffix <-
    Parsec.optionMaybe $
      asum
        <$> Parsec.many1
          ( typeListParser basicTypeParser
          )
  pure $ case mSuffix of
    Just suffix -> foldl' apply prefix suffix
    Nothing -> prefix

quantifierParser :: Parser [Parameter]
quantifierParser = typeListParser parameter

parameter :: Parser Parameter
parameter = do
  origin <- getTokenOrigin
  Parsec.choice
    [ (\unqualified -> Parameter origin unqualified Permission Nothing)
        <$> (parserMatchOperator "+" *> wordNameParser),
      do
        name <- wordNameParser
        Parameter origin name
          <$> Parsec.option Value (Stack <$ parserMatch Token.Ellipsis) <*> pure Nothing
    ]

typeListParser :: Parser a -> Parser [a]
typeListParser e =
  bracketedParser
    (e `Parsec.sepEndBy1` commaParser)

quantifiedParser :: Parser Signature -> Parser Signature
quantifiedParser thing = do
  origin <- getTokenOrigin
  params <- quantifierParser
  Signature.Quantified params <$> thing <*> pure origin

classParser :: Parser Class
classParser =
  (<?> "type class declaration") $ do
    origin <- getTokenOrigin <* parserMatch Token.Class
    suffix <- unqualifiedNameParser <?> "type class name"
    name <- Qualified <$> Parsec.getState <*> pure suffix
    parameters <- Parsec.option [] quantifierParser
    methods <- blockedParser $ many methodParser
    pure
      Class
        { Class._className = name,
          Class._classOrigin = origin,
          Class._parameters = parameters,
          Class._methods = methods
        }

methodParser :: Parser Method
methodParser = (<?> "method") $ do
  origin <- getTokenOrigin <* parserMatch Token.Method
  suffix <- unqualifiedNameParser <?> "method name"
  name <- Qualified <$> Parsec.getState <*> pure suffix
  sig <- signatureParser
  pure
    Method
      { Class._signature = sig,
        Class._name = name,
        Class._origin = origin
      }

intrinsicParser :: Parser Intrinsic
intrinsicParser =
  (<?> "intrinsic declaration") $ do
    origin <- getTokenOrigin <* parserMatch Token.Intrinsic
    suffix <- unqualifiedNameParser <?> "intrinsic name"
    name <- Qualified <$> Parsec.getState <*> pure suffix
    sig <- signatureParser <?> "declaration signature"
    pure
      Intrinsic
        { Intrinsic._name = name,
          Intrinsic._origin = origin,
          Intrinsic._signature = sig
        }

basicDefinitionParser :: Parser (WordDefinition ())
basicDefinitionParser =
  (<?> "word definition") $ do
    origin <- getTokenOrigin <* parserMatch Token.Define
    (name, fixity) <- qualifiedNameParser <?> "word definition name"
    sig <- signatureParser <?> "word definition signature"
    body <- blockLikeParser <?> "word definition body"
    pure
      WordDefinition
        { Definition._wordBody = body,
          Definition._wordFixity = fixity,
          Definition._wordInferSignature = False,
          Definition._wordMerge = Merge.Deny,
          Definition._wordName = name,
          Definition._wordOrigin = origin,
          Definition._wordSignature = sig
        }

instanceParser :: Parser (Instance ())
instanceParser = (<?> "class instance") $ do
  origin <- getTokenOrigin <* parserMatch Token.Instance
  suffix <- unqualifiedNameParser <?> "class name"
  name <- Qualified <$> Parsec.getState <*> pure suffix
  parserMatch Token.For
  target <- basicTypeParser
  methods <- blockedParser $ many basicDefinitionParser
  pure
    Instance
      { Instance._name = name,
        Instance._origin = origin,
        Instance._target = target,
        Instance._methods = methods
      }

permissionParser :: Parser (PermissionDefinition ())
permissionParser =
  (<?> "permission definition") $ do
    origin <- getTokenOrigin <* parserMatch Token.Permission
    (name, fixity) <- qualifiedNameParser <?> "definition name"
    sig <- signatureParser
    body <- blockLikeParser <?> "definition body"
    pure
      PermissionDefinition
        { Definition._permissionBody = body,
          Definition._permissionFixity = fixity,
          Definition._permissionName = name,
          Definition._permissionOrigin = origin,
          Definition._permissionSignature = sig
        }

-- | Unqualified or partially qualified name, implicitly qualified by the
-- current vocabulary, or fully qualified (global) name.
qualifiedNameParser :: Parser (Qualified, Operator.Fixity)
qualifiedNameParser = (<?> "optionally qualified name") $ do
  (suffix, fixity) <- nameParser
  name <- case suffix of
    QualifiedName qualified@(Qualified (Qualifier root parts) unqualified) ->
      case root of
        -- Fully qualified name: pure it as-is.
        Absolute -> pure qualified
        -- Partially qualified name: add current vocab prefix to qualifier.
        Relative -> do
          Qualifier root' prefixParts <- Parsec.getState
          pure (Qualified (Qualifier root' (prefixParts ++ parts)) unqualified)
    -- Unqualified name: use current vocab prefix as qualifier.
    UnqualifiedName unqualified ->
      Qualified <$> Parsec.getState <*> pure unqualified
    LocalName _ -> ice "Mlatu.Parse.qualifiedNameParser - name parser should only pure qualified or unqualified name"
  pure (name, fixity)

signatureParser :: Parser Signature
signatureParser = quantifiedParser signature <|> signature <?> "type signature"

signature :: Parser Signature
signature = groupedParser functionTypeParser

blockParser :: Parser (Term ())
blockParser =
  (blockedParser blockContentsParser <|> reference)
    <?> "block or reference"

reference :: Parser (Term ())
reference =
  parserMatch_ Token.Reference
    *> Parsec.choice
      [ do
          origin <- getTokenOrigin
          Word () Operator.Postfix
            <$> (fst <$> nameParser) <*> pure [] <*> pure origin,
        termParser
      ]

blockContentsParser :: Parser (Term ())
blockContentsParser = do
  origin <- getTokenOrigin
  terms <- many termParser
  let origin' = case terms of
        x : _ -> Term.origin x
        _emptyList -> origin
  pure $ foldr (Compose ()) (Term.identityCoercion () origin') terms

termParser :: Parser (Term ())
termParser = (<?> "expression") $ do
  origin <- getTokenOrigin
  Parsec.choice
    [ Parsec.try (uncurry (Push ()) <$> parseOne toLiteral <?> "literal"),
      do
        (name, fixity) <- nameParser
        pure (Word () fixity name [] origin),
      Parsec.try sectionParser,
      Parsec.try groupParser <?> "parenthesized expression",
      vectorParser,
      lambdaParser,
      matchParser,
      ifParser,
      doParser,
      Push () <$> blockValue <*> pure origin,
      withParser,
      asParser
    ]

toLiteral :: Located Token -> Maybe (Value (), Origin)
toLiteral token = case Located.item token of
  Token.Character x -> Just (Character x, origin)
  Token.Float x -> Just (Float x, origin)
  Token.Integer x -> Just (Integer x, origin)
  Token.Text x -> Just (Text x, origin)
  _nonLiteral -> Nothing
  where
    origin :: Origin
    origin = Located.origin token

sectionParser :: Parser (Term ())
sectionParser =
  (<?> "operator section") $
    groupedParser $
      Parsec.choice
        [ do
            origin <- getTokenOrigin
            function <- operatorNameParser
            let call =
                  Word
                    ()
                    Operator.Postfix
                    (UnqualifiedName function)
                    []
                    origin
            Parsec.choice
              [ do
                  operandOrigin <- getTokenOrigin
                  operand <- Parsec.many1 termParser
                  pure $ compose () operandOrigin $ operand ++ [call],
                pure call
              ],
          do
            operandOrigin <- getTokenOrigin
            operand <-
              Parsec.many1 $
                Parsec.notFollowedBy operatorNameParser *> termParser
            origin <- getTokenOrigin
            function <- operatorNameParser
            pure $
              compose () operandOrigin $
                operand
                  ++ [ Word
                         ()
                         Operator.Postfix
                         (QualifiedName (Qualified Vocabulary.intrinsic "swap"))
                         []
                         origin,
                       Word () Operator.Postfix (UnqualifiedName function) [] origin
                     ]
        ]

vectorParser :: Parser (Term ())
vectorParser = (<?> "vector literal") $ do
  vectorOrigin <- getTokenOrigin
  es <-
    bracketedParser $
      (compose () vectorOrigin <$> Parsec.many1 termParser)
        `Parsec.sepEndBy` commaParser
  pure $
    compose () vectorOrigin $
      (Group <$> es)
        ++ [NewVector () (length es) () vectorOrigin]

lambdaParser :: Parser (Term ())
lambdaParser = (<?> "parameteriable introduction") $ do
  names <- parserMatch Token.Arrow *> lambdaNamesParser
  Parsec.choice
    [ parserMatchOperator ";" *> do
        origin <- getTokenOrigin
        body <- blockContentsParser
        pure $ makeLambda names body origin,
      do
        origin <- getTokenOrigin
        body <- blockParser
        pure $ Push () (Quotation $ makeLambda names body origin) origin
    ]

matchParser :: Parser (Term ())
matchParser = (<?> "match") $ do
  matchOrigin <- getTokenOrigin <* parserMatch Token.Match
  scrutineeOrigin <- getTokenOrigin
  mScrutinee <- Parsec.optionMaybe groupParser <?> "scrutinee"
  (cases, else_) <- do
    cases' <-
      many $
        (<?> "case") $
          parserMatch Token.Case *> do
            origin <- getTokenOrigin
            (name, _) <- nameParser
            body <- blockLikeParser
            pure $ Case name body origin
    mElse' <- Parsec.optionMaybe $ do
      origin <- getTokenOrigin <* parserMatch Token.Else
      body <- blockParser
      pure $ Else body origin
    pure $
      (,) cases' $
        fromMaybe
          (DefaultElse () matchOrigin)
          mElse'
  let match = Match AnyMatch () cases else_ matchOrigin
  pure $ case mScrutinee of
    Just scrutinee -> compose () scrutineeOrigin [scrutinee, match]
    Nothing -> match

ifParser :: Parser (Term ())
ifParser = (<?> "if-else expression") $ do
  ifOrigin <- getTokenOrigin <* parserMatch Token.If
  mCondition <- Parsec.optionMaybe groupParser <?> "condition"
  ifBody <- blockParser
  elseBody <-
    Parsec.option (Term.identityCoercion () ifOrigin) $
      parserMatch Token.Else *> blockParser
  pure $
    compose
      ()
      ifOrigin
      [ fromMaybe (Term.identityCoercion () ifOrigin) mCondition,
        Match
          BooleanMatch
          ()
          [ Case "true" ifBody ifOrigin,
            Case "false" elseBody (Term.origin elseBody)
          ]
          (DefaultElse () ifOrigin)
          ifOrigin
      ]

doParser :: Parser (Term ())
doParser = (<?> "do expression") $ do
  doOrigin <- getTokenOrigin <* parserMatch Token.Do
  term <- groupParser <?> "parenthesized expression"
  Parsec.choice
    -- do (f) { x y z } => { x y z } f
    [ do
        body <- blockLikeParser
        pure $
          compose
            ()
            doOrigin
            [Push () (Quotation body) (Term.origin body), term],
      -- do (f) [x, y, z] => [x, y, z] f
      do
        body <- vectorParser
        pure $ compose () doOrigin [body, term]
    ]

blockValue :: Parser (Value ())
blockValue = (<?> "quotation") $ Quotation <$> blockParser

asParser :: Parser (Term ())
asParser = (<?> "'as' expression") $ do
  origin <- getTokenOrigin <* parserMatch_ Token.As
  signatures <- groupedParser $ basicTypeParser `Parsec.sepEndBy` commaParser
  pure $ Term.asCoercion () origin signatures

-- A 'with' term is parsed as a coercion followed by a call.
withParser :: Parser (Term ())
withParser = (<?> "'with' expression") $ do
  origin <- getTokenOrigin <* parserMatch_ Token.With
  permits <- groupedParser $ Parsec.many1 permitParser
  pure $
    Term.compose
      ()
      origin
      [ Term.permissionCoercion permits () origin,
        Word
          ()
          Operator.Postfix
          (QualifiedName (Qualified Vocabulary.intrinsic "call"))
          []
          origin
      ]

permitParser :: Parser Term.Permit
permitParser =
  Term.Permit
    <$> Parsec.choice
      [ True <$ parserMatchOperator "+",
        False <$ parserMatchOperator "-"
      ]
    <*> (UnqualifiedName <$> wordNameParser)

parserMatchOperator :: Text -> Parser (Located Token)
parserMatchOperator = parserMatch . Token.Operator . Unqualified

lambdaNamesParser :: Parser [(Maybe Unqualified, Origin)]
lambdaNamesParser = lambdaName `Parsec.sepEndBy1` commaParser

lambdaName :: Parser (Maybe Unqualified, Origin)
lambdaName = do
  origin <- getTokenOrigin
  name <- Just <$> wordNameParser <|> Nothing <$ parserMatch Token.Ignore
  pure (name, origin)

blockLikeParser :: Parser (Term ())
blockLikeParser =
  Parsec.choice
    [ blockParser,
      parserMatch Token.Arrow *> do
        names <- lambdaNamesParser
        origin <- getTokenOrigin
        body <- blockParser
        pure $ makeLambda names body origin
    ]

makeLambda :: [(Maybe Unqualified, Origin)] -> Term () -> Origin -> Term ()
makeLambda parsed body origin =
  foldr
    ( \(nameMaybe, nameOrigin) acc ->
        maybe
          ( Compose
              ()
              ( Word
                  ()
                  Operator.Postfix
                  (QualifiedName (Qualified Vocabulary.intrinsic "drop"))
                  []
                  origin
              )
              acc
          )
          (\name -> Lambda () name () acc nameOrigin)
          nameMaybe
    )
    body
    (reverse parsed)
