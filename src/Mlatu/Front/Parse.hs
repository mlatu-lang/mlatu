{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Mlatu.Parse
-- Description : Parsing from tokens to terms
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Front.Parse
  ( fragment,
  )
where

import Data.List (findIndex, foldl1)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Mlatu.Base.Kind (Kind (..))
import Mlatu.Base.Located (Located)
import Mlatu.Base.Located qualified as Located
import Mlatu.Base.Name
  ( GeneralName (..),
    Qualified (Qualified),
    Qualifier (Qualifier),
    Root (Absolute, Relative),
    Unqualified (..),
  )
import Mlatu.Base.Origin (Origin)
import Mlatu.Base.Origin qualified as Origin
import Mlatu.Front.CodataDefinition (CodataDefinition (..))
import Mlatu.Front.CodataDefinition qualified as CodataDefinition
import Mlatu.Front.DataDefinition (DataDefinition (..))
import Mlatu.Front.DataDefinition qualified as DataDefinition
import Mlatu.Front.Definition (Category (..), Definition (..), Merge (..), Parent (..))
import Mlatu.Front.Definition qualified as Definition
import Mlatu.Front.DesugarTypes qualified as Data
import Mlatu.Front.Fragment (Fragment)
import Mlatu.Front.Fragment qualified as Fragment
import Mlatu.Front.Metadata (Metadata (Metadata))
import Mlatu.Front.Metadata qualified as Metadata
import Mlatu.Front.Parameter (Parameter (Parameter))
import Mlatu.Front.Parser (Parser, getTokenOrigin, parserMatch, parserMatch_)
import Mlatu.Front.Signature (Signature)
import Mlatu.Front.Signature qualified as Signature
import Mlatu.Front.Term (Term (..), Value (..), compose)
import Mlatu.Front.Term qualified as Term
import Mlatu.Front.Token (Token)
import Mlatu.Front.Token qualified as Token
import Mlatu.Front.Trait (Trait (..))
import Mlatu.Front.Trait qualified as Trait
import Mlatu.Informer (M, halt, ice, reportParseError)
import Relude.Unsafe qualified as Unsafe
import Text.Parsec ((<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos (SourcePos)

-- | A top-level program element.
data Element a
  = -- | @intrinsic@, @trait@
    TraitElement !Trait
  | -- | @define@, @instance@
    DefinitionElement !(Definition a)
  | -- | @about@
    MetadataElement !Metadata
  | -- | Top-level (@main@) code.
    TermElement !(Term a)
  | -- | @type@
    DataElement !DataDefinition
  | CodataElement !CodataDefinition

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
          (Qualifier Absolute [])
          path
          tokens
   in case parsed of
        Left parseError -> do
          reportParseError parseError
          halt
        Right result -> pure (Data.desugar (insertMain result))
  where
    isMain = (fromMaybe Definition.mainName mainName ==) . view Definition.name
    insertMain f = case find isMain $ view Fragment.definitions f of
      Just {} -> f
      Nothing ->
        over
          Fragment.definitions
          ( Definition.main
              mainPermissions
              mainName
              (Term.identityCoercion (Origin.point path line 1) ())
              :
          )
          f

fragmentParser ::
  [GeneralName] -> Maybe Qualified -> Parser (Fragment ())
fragmentParser mainPermissions mainName =
  partitionElements mainPermissions mainName
    <$> elementsParser <* Parsec.eof

elementsParser :: Parser [Element ()]
elementsParser = many elementParser <|> moduleParser

partitionElements ::
  [GeneralName] ->
  Maybe Qualified ->
  [Element ()] ->
  Fragment ()
partitionElements mainPermissions mainName = foldr go mempty
  where
    go :: Element () -> Fragment () -> Fragment ()
    go = \case
      CodataElement x -> over Fragment.codataDefinitions (x :)
      TraitElement x -> over Fragment.traits (x :)
      DefinitionElement x -> over Fragment.definitions (x :)
      MetadataElement x -> over Fragment.metadata (x :)
      DataElement x -> over Fragment.dataDefinitions (x :)
      TermElement x ->
        over
          Fragment.definitions
          ( \defs ->
              case findIndex
                ((== fromMaybe Definition.mainName mainName) . view Definition.name)
                defs of
                Just index -> case splitAt index defs of
                  (a, existing : b) ->
                    a
                      ++ over Definition.body (`composeUnderLambda` x) existing :
                    b
                  _nonMain -> ice "cannot find main definition"
                Nothing ->
                  Definition.main mainPermissions mainName x : defs
          )
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
          composeUnderLambda (Lambda origin typ name parameterType body) term =
            Lambda origin typ name parameterType (composeUnderLambda body term)
          composeUnderLambda a b = Compose () a b

moduleParser :: Parser [Element ()]
moduleParser = (<?> "module definition") $ do
  parserMatch_ Token.Module
  original@(Qualifier _ outer) <- Parsec.getState
  vocabularyName <- nameParser <?> "module name"
  let (inner, name) = case vocabularyName of
        QualifiedName
          (Qualified (Qualifier _root qualifier) (Unqualified unqualified)) ->
            (qualifier, unqualified)
        UnqualifiedName (Unqualified unqualified) -> ([], unqualified)
        LocalName {} -> ice "local name should not appear as vocabulary name"
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
  groupedParser $ Group . compose origin () <$> Parsec.many1 termParser

bracketedParser :: Parser a -> Parser a
bracketedParser =
  Parsec.between
    (parserMatch Token.VectorBegin)
    (parserMatch Token.VectorEnd)

nameParser :: Parser GeneralName
nameParser = (<?> "name") $ do
  parts <- unqualifiedNameParser `Parsec.sepBy1` parserMatch Token.Dot
  pure $ case parts of
    [unqualified] -> UnqualifiedName unqualified
    _list ->
      let parts' = (\(Unqualified part) -> part) <$> parts
          qualifier = Unsafe.fromJust (viaNonEmpty init parts')
          unqualified = Unsafe.fromJust (viaNonEmpty last parts)
       in QualifiedName
            ( Qualified
                (Qualifier Relative qualifier)
                unqualified
            )

unqualifiedNameParser :: Parser Unqualified
unqualifiedNameParser =
  (<?> "unqualified name") $
    lowerNameParser <|> upperNameParser <|> operatorNameParser

lowerNameParser :: Parser Unqualified
lowerNameParser = (<?> "word name") $
  parseOne $
    \token -> case Located.item token of
      Token.LowerWord name -> Just name
      _nonWord -> Nothing

upperNameParser :: Parser Unqualified
upperNameParser = (<?> "initial-capital word name") $
  parseOne $
    \token -> case Located.item token of
      Token.UpperWord name -> Just name
      _nonWord -> Nothing

operatorNameParser :: Parser Unqualified
operatorNameParser = (<?> "operator name") $ do
  angles <- many $
    parseOne $ \token -> case Located.item token of
      Token.AngleBegin -> Just "<"
      Token.AngleEnd -> Just ">"
      _nonAngle -> Nothing
  rest <- parseOne $ \token -> case Located.item token of
    Token.Operator (Unqualified name) -> Just name
    _nonUnqualifiedOperator -> Nothing
  pure $ Unqualified $ Text.concat $ angles ++ [rest]

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
      [ DefinitionElement
          <$> Parsec.choice
            [ basicDefinitionParser,
              instanceParser,
              permissionParser
            ],
        TraitElement <$> traitParser,
        MetadataElement <$> metadataParser,
        DataElement <$> dataParser,
        CodataElement <$> codataParser,
        TermElement
          <$> ( do
                  origin <- getTokenOrigin
                  compose origin () <$> Parsec.many1 termParser
              )
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
          (parserMatch Token.Data *> lowerNameParser)
            <?> "'type' and type identifier"
        ]
  fields <-
    blockedParser $
      many $
        (,)
          <$> (lowerNameParser <?> "metadata key identifier")
          <*> (blockParser <?> "metadata value block")
  pure
    Metadata
      { Metadata._fields = Map.fromList fields,
        Metadata._name = QualifiedName name,
        Metadata._origin = origin
      }

dataParser :: Parser DataDefinition
dataParser = (<?> "data definition") $ do
  origin <- getTokenOrigin <* parserMatch Token.Data
  parameters <- Parsec.option [] $ groupedParser (Parsec.many parameter)
  dataName <- qualifiedNameParser <?> "data name"
  constructors <-
    blockedParser
      ( ( ( do
              origin <- getTokenOrigin
              name <- lowerNameParser <?> "constructor name"
              (lefts, rights) <-
                (<?> "constructor fields") $
                  groupedParser
                    ( do
                        leftTypes <- left
                        _ <- arrow
                        rightTypes <- right
                        pure (leftTypes, rightTypes)
                    )
              pure (name, lefts, rights, origin)
          )
            <?> "constructor definition"
        )
          `Parsec.sepBy` parserMatch Token.Case
      )
  pure
    DataDefinition
      { DataDefinition._constructors = constructors,
        DataDefinition._name = dataName,
        DataDefinition._origin = origin,
        DataDefinition._parameters = reverse parameters
      }

codataParser :: Parser CodataDefinition
codataParser = (<?> "codata definition") $ do
  origin <- getTokenOrigin <* parserMatch Token.Codata
  parameters <- Parsec.option [] $ groupedParser (Parsec.many parameter)
  codataName <- qualifiedNameParser <?> "codata name"
  deconstructors <-
    blockedParser
      ( ( ( do
              origin <- getTokenOrigin
              name <- lowerNameParser <?> "destructor name"
              (lefts, rights) <-
                (<?> "destructor signature") $
                  groupedParser
                    ( do
                        leftTypes <- left
                        _ <- arrow
                        rightTypes <- right
                        pure (leftTypes, rightTypes)
                    )
              pure (name, lefts, rights, origin)
          )
            <?> "destructor definition"
        )
          `Parsec.sepBy` parserMatchOperator "+"
      )
  pure $
    CodataDefinition
      { CodataDefinition._deconstructors = deconstructors,
        CodataDefinition._name = codataName,
        CodataDefinition._origin = origin,
        CodataDefinition._parameters = reverse parameters
      }

traitParser :: Parser Trait
traitParser = do
  origin <- getTokenOrigin <* parserMatch Token.Trait
  suffix <- unqualifiedNameParser <?> "trait name"
  name <- Qualified <$> Parsec.getState <*> pure suffix
  sig <- signatureParser <?> "trait signature"
  pure
    Trait
      { Trait._name = name,
        Trait._origin = origin,
        Trait._signature = sig
      }

typeParser :: Parser Signature
typeParser = Parsec.try functionTypeParser <|> basicTypeParser <?> "type"

functionTypeParser :: Parser Signature
functionTypeParser = (<?> "function type") $ do
  (effect, origin) <-
    Parsec.choice
      [ stackSignature,
        arrowSignature
      ]
  perms <- Parsec.option [] permissions
  pure (effect perms origin)

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
    stack = upperNameParser

arrowSignature :: Parser ([GeneralName] -> Origin -> Signature, Origin)
arrowSignature = (<?> "arrow function type") $ do
  leftTypes <- left
  origin <- arrow
  rightTypes <- right
  pure (Signature.Function leftTypes rightTypes, origin)

permissions :: Parser [GeneralName]
permissions = (<?> "permission labels") $ do
  parserMatch_ Token.AngleBegin
  ps <- nameParser `Parsec.sepBy1` parserMatchOperator "+"
  parserMatch_ Token.AngleEnd
  pure ps

left, right :: Parser [Signature]
left = basicTypeParser `Parsec.sepEndBy` commaParser
right = typeParser `Parsec.sepEndBy` commaParser

arrow :: Parser Origin
arrow = getTokenOrigin <* parserMatch Token.Arrow

commaParser :: Parser ()
commaParser = void $ parserMatch Token.Comma

basicTypeParser' :: Parser Signature
basicTypeParser' =
  Parsec.choice
    [ do
        sig <- groupedParser (quantifiedParser typeParser <|> typeParser)
        pure $ Signature.Grouped sig (Signature.origin sig),
      Parsec.try $ do
        origin <- getTokenOrigin
        name <- nameParser
        guard $ name /= "+"
        pure $ Signature.Variable name origin
    ]

basicTypeParser :: Parser Signature
basicTypeParser = (<?> "basic type") $ foldl1 (\a b -> Signature.Application a b (Signature.origin a)) . reverse <$> Parsec.many1 basicTypeParser'

parameter :: Parser Parameter
parameter = do
  origin <- getTokenOrigin
  (kind, name) <-
    Parsec.choice
      [ (Permission,) <$> (parserMatchOperator "+" *> lowerNameParser),
        (Stack,) <$> upperNameParser,
        (Value,) <$> lowerNameParser
      ]
  pure $ Parameter origin name kind

quantifiedParser :: Parser Signature -> Parser Signature
quantifiedParser thing = do
  origin <- getTokenOrigin <* parserMatch_ Token.For
  params <- Parsec.many1 parameter
  parserMatch_ Token.Dot
  Signature.Quantified params <$> thing <*> pure origin

basicDefinitionParser :: Parser (Definition ())
basicDefinitionParser =
  (<?> "word definition") $
    definitionParser Token.Define DefinedWord

instanceParser :: Parser (Definition ())
instanceParser =
  (<?> "instance definition") $
    definitionParser Token.Instance InstanceWord

permissionParser :: Parser (Definition ())
permissionParser =
  (<?> "permission definition") $
    definitionParser Token.Permission PermissionWord

-- | Unqualified or partially qualified name, implicitly qualified by the
-- current vocabulary, or fully qualified (global) name.
qualifiedNameParser :: Parser Qualified
qualifiedNameParser = (<?> "optionally qualified name") $ do
  suffix <- nameParser
  case suffix of
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
    LocalName _ -> ice "name parser should only return qualified or unqualified name"

definitionParser :: Token -> Category -> Parser (Definition ())
definitionParser keyword category = do
  origin <- getTokenOrigin <* parserMatch keyword
  name <- qualifiedNameParser <?> "definition name"
  sig <- signatureParser
  body <- blockLikeParser <?> "definition body"
  pure
    Definition
      { Definition._body = body,
        Definition._category = category,
        Definition._inferSignature = False,
        Definition._merge = DenyMerge,
        Definition._name = name,
        Definition._origin = origin,
        -- HACK: Should be passed in from outside?
        Definition._parent = case keyword of
          Token.Instance -> Just $ TraitParent name
          _nonInstance -> Nothing,
        Definition._signature = sig
      }

signatureParser :: Parser Signature
signatureParser = groupedParser (quantifiedParser functionTypeParser <|> functionTypeParser) <?> "type signature"

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
          Word origin () <$> nameParser <*> pure [],
        termParser
      ]

blockContentsParser :: Parser (Term ())
blockContentsParser = do
  origin <- getTokenOrigin
  terms <- many termParser
  let origin' = case terms of
        x : _ -> Term.origin x
        _emptyList -> origin
  pure $ foldr (Compose ()) (Term.identityCoercion origin' ()) terms

termParser :: Parser (Term ())
termParser = (<?> "expression") $ do
  origin <- getTokenOrigin
  Parsec.choice
    [ Parsec.try intParser,
      Parsec.try ((\(v, o) -> Push o () v) <$> parseOne toLiteral <?> "literal"),
      do
        name <- nameParser
        pure (Word origin () name []),
      Parsec.try sectionParser,
      Parsec.try groupParser <?> "parenthesized expression",
      vectorParser,
      lambdaParser,
      matchParser,
      Push origin () <$> blockValue,
      withParser,
      asParser
    ]

toLiteral :: Located Token -> Maybe (Value (), Origin)
toLiteral token = case Located.item token of
  Token.Character x -> Just (Character x, origin)
  Token.Text x -> Just (Text x, origin)
  _nonLiteral -> Nothing
  where
    origin :: Origin
    origin = Located.origin token

intParser :: Parser (Term ())
intParser = do
  (num, origin) <-
    parseOne
      ( \token -> case Located.item token of
          Token.Integer x -> Just (x, Located.origin token)
          _ -> Nothing
      )
  let go 0 = [Word origin () "zero" []]
      go n = go (n - 1) ++ [Word origin () "succ" []]

  pure $ compose origin () (go num)

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
                    origin
                    ()
                    (UnqualifiedName function)
                    []
            Parsec.choice
              [ do
                  operandOrigin <- getTokenOrigin
                  operand <- Parsec.many1 termParser
                  pure $ compose operandOrigin () $ operand ++ [call],
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
              compose operandOrigin () $
                operand
                  ++ [ Word origin () (UnqualifiedName "swap") [],
                       Word origin () (UnqualifiedName function) []
                     ]
        ]

vectorParser :: Parser (Term ())
vectorParser = (<?> "list literal") $ do
  origin <- getTokenOrigin
  es <- bracketedParser $ (compose origin () <$> Parsec.many1 termParser) `Parsec.sepEndBy` commaParser
  pure $ compose origin () ((Group <$> es) ++ [Word origin () "nil" []] ++ replicate (length es) (Word origin () "cons" []))

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
        pure $ Push origin () (Quotation $ makeLambda names body origin)
    ]

matchParser :: Parser (Term ())
matchParser = (<?> "match") $ do
  matchOrigin <- getTokenOrigin <* parserMatch Token.Match
  (cases, else_) <- do
    cases' <-
      many $
        (<?> "case") $
          Parsec.try $ do
            o1 <- getTokenOrigin <* parserMatch Token.Case
            name <- nameParser
            o2 <- getTokenOrigin
            body <- blockLikeParser
            pure (Origin.between o1 o2, name, body)
    mElse' <- Parsec.optionMaybe $ do
      o1 <- getTokenOrigin <* parserMatch Token.Case
      o2 <- getTokenOrigin <* parserMatch Token.Ignore
      body <- blockParser
      pure (Origin.between o1 o2, Right body)
    pure $
      (,) cases' $
        fromMaybe
          (matchOrigin, Left ())
          mElse'
  pure (Match matchOrigin () cases else_)

blockValue :: Parser (Value ())
blockValue = (<?> "quotation") $ Quotation <$> blockParser

asParser :: Parser (Term ())
asParser = (<?> "'as' expression") $ do
  origin <- getTokenOrigin <* parserMatch_ Token.As
  signatures <- groupedParser $ basicTypeParser `Parsec.sepEndBy` commaParser
  pure $ Term.asCoercion origin () signatures

-- A 'with' term is parsed as a coercion followed by a call.
withParser :: Parser (Term ())
withParser = (<?> "'with' expression") $ do
  origin <- getTokenOrigin <* parserMatch_ Token.With
  permits <- groupedParser $ Parsec.many1 permitParser
  pure $
    Term.compose
      origin
      ()
      [ Term.permissionCoercion origin () permits,
        Push origin () (Text "call"),
        Word origin () "extern" []
      ]

permitParser :: Parser Term.Permit
permitParser =
  Term.Permit
    <$> Parsec.choice
      [ True <$ parserMatchOperator "+",
        False <$ parserMatchOperator "-"
      ]
    <*> (UnqualifiedName <$> lowerNameParser)

parserMatchOperator :: Text -> Parser (Located Token)
parserMatchOperator = parserMatch . Token.Operator . Unqualified

lambdaNamesParser :: Parser [(Maybe Unqualified, Origin)]
lambdaNamesParser = lambdaName `Parsec.sepEndBy1` commaParser

lambdaName :: Parser (Maybe Unqualified, Origin)
lambdaName = do
  origin <- getTokenOrigin
  name <- Just <$> lowerNameParser <|> Nothing <$ parserMatch Token.Ignore
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
          ( compose
              origin
              ()
              [Push origin () (Text "drop"), Word origin () "extern" [], acc]
          )
          (\name -> Lambda nameOrigin () name () acc)
          nameMaybe
    )
    body
    (reverse parsed)
