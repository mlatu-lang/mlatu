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
import Mlatu.DataConstructor (DataConstructor (DataConstructor))
import Mlatu.DataConstructor qualified as DataConstructor
import Mlatu.Definition (WordDefinition (..))
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
import Mlatu.Origin (Origin)
import Mlatu.Origin qualified as Origin
import Mlatu.Parser (Parser, getTokenOrigin, parserMatch, parserMatch_)
import Mlatu.RecordDefinition (RecordDefinition (..))
import Mlatu.RecordDefinition qualified as RecordDefinition
import Mlatu.RecordField (RecordField (..))
import Mlatu.RecordField qualified as RecordField
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
  -- | Override name of @main@.
  Maybe Qualified ->
  -- | Input tokens.
  [Located Token] ->
  -- | Parsed program fragment.
  M (Fragment ())
fragment line path mainName tokens =
  let parsed =
        Parsec.runParser
          (fragmentParser mainName)
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
    Right name -> pure name

fragmentParser ::
  Maybe Qualified -> Parser (Fragment ())
fragmentParser mainName =
  partitionElements mainName
    <$> elementsParser <* Parsec.eof

elementsParser :: Parser [Element ()]
elementsParser = asum <$> many (vocabularyParser <|> one <$> elementParser)

partitionElements ::
  Maybe Qualified ->
  [Element ()] ->
  Fragment ()
partitionElements mainName = rev . foldr go mempty
  where
    rev :: Fragment () -> Fragment ()
    rev f =
      over Fragment.intrinsics reverse $
        over Fragment.wordDefinitions reverse $
          over Fragment.metadata reverse $
            over Fragment.types reverse f

    go :: Element () -> Fragment () -> Fragment ()
    go e acc = case e of
      Element.Intrinsic x -> over Fragment.intrinsics (x :) acc
      Element.WordDefinition x -> over Fragment.wordDefinitions (x :) acc
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
                  Definition.main mainName x : defs
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
  vocabularyName <- nameParser <?> "vocabulary name"
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

nameParser :: Parser GeneralName
nameParser = (<?> "name") $ do
  global <-
    isJust
      <$> Parsec.optionMaybe
        (parserMatch Token.Ignore <* parserMatch Token.VocabLookup)
  parts <-
    Parsec.choice
      [ wordNameParser,
        operatorNameParser
      ]
      `Parsec.sepBy1` parserMatch Token.VocabLookup
  pure $ case parts of
    [unqualified] ->
      ( if global
          then QualifiedName . Qualified Vocabulary.global
          else UnqualifiedName
      )
        unqualified
    _list ->
      let parts' = (\(Unqualified part) -> part) <$> parts
          qualifier = Unsafe.fromJust (viaNonEmpty init parts')
          unqualified = Unsafe.fromJust (viaNonEmpty last parts)
       in QualifiedName
            ( Qualified
                (Qualifier (if global then Absolute else Relative) qualifier)
                unqualified
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
        Element.Intrinsic <$> intrinsicParser,
        Element.Metadata <$> metadataParser,
        Element.TypeDefinition <$> typeDefinitionParser,
        Element.RecordDefinition <$> recordDefinitionParser,
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
  name <- qualifiedNameParser <?> "type definition name"
  parameters <-
    Parsec.many
      ( (<?> "parameter") $ do
          origin <- getTokenOrigin
          name <- wordNameParser <?> "parameter name"
          pure $ Parameter origin name Value Nothing
      )
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

recordDefinitionParser :: Parser RecordDefinition
recordDefinitionParser = (<?> "record definition") $ do
  origin <- getTokenOrigin <* parserMatch Token.Record
  name <- qualifiedNameParser <?> "record definition name"
  parameters <-
    Parsec.many
      ( (<?> "parameter") $ do
          origin <- getTokenOrigin
          name <- wordNameParser <?> "parameter name"
          pure $ Parameter origin name Value Nothing
      )
  fields <- blockedParser $ many fieldParser
  pure
    RecordDefinition
      { RecordDefinition._fields = fields,
        RecordDefinition._name = name,
        RecordDefinition._origin = origin,
        RecordDefinition._parameters = parameters
      }

fieldParser :: Parser RecordField
fieldParser = (<?> "field definition") $ do
  origin <- getTokenOrigin <* parserMatch Token.Field
  name <- wordNameParser <?> "field name"
  signature <- groupedParser typeParser
  pure
    RecordField
      { RecordField._signature = signature,
        RecordField._name = name,
        RecordField._origin = origin
      }

constructorFieldsParser :: Parser [Signature]
constructorFieldsParser = typeParser `Parsec.sepEndBy` commaParser

typeParser :: Parser Signature
typeParser = Parsec.try functionTypeParser <|> basicTypeParser <?> "type"

functionTypeParser :: Parser Signature
functionTypeParser = (<?> "function type") $ do
  leftTypes <- basicTypeParser `Parsec.sepEndBy` commaParser
  origin <- getTokenOrigin <* parserMatch Token.Arrow
  rightTypes <- typeParser `Parsec.sepEndBy` commaParser
  pure $ Signature.Function leftTypes rightTypes origin

commaParser :: Parser ()
commaParser = void $ parserMatch Token.Comma

quantifiedParser :: Parser Signature -> Parser Signature
quantifiedParser thing = do
  origin <- getTokenOrigin <* parserMatch Token.Forall
  params <-
    ( asum <$> groupedParser (Parsec.sepEndBy1 kindParameters commaParser)
        <|> Parsec.many1
          ( do
              origin <- getTokenOrigin
              name <- wordNameParser <?> "parameter name"
              pure (Parameter origin name Value Nothing)
          )
      )
      <?> "parameters"
  parserMatch_ Token.Dot
  Signature.Quantified params <$> thing <*> pure origin
  where
    kindParameters :: Parser [Parameter]
    kindParameters = do
      names <-
        Parsec.many1
          ( do
              origin <- getTokenOrigin
              name <- wordNameParser <?> "parameter name"
              pure (Parameter origin name)
          )
      parserMatch_ Token.Colon
      kind <- kindParser
      pure $ fmap (\name -> name kind Nothing) names

basicTypeParser :: Parser Signature
basicTypeParser = (<?> "basic type") $ do
  prefix <-
    Parsec.choice
      [ quantifiedParser typeParser,
        Parsec.try $ do
          origin <- getTokenOrigin
          name <- nameParser
          pure $ Signature.Variable name origin,
        groupedParser typeParser
      ]
  let apply a b = Signature.Application a b $ Signature.origin prefix
  mSuffix <- Parsec.optionMaybe (Parsec.many1 basicTypeParser)
  pure $ case mSuffix of
    Just suffix -> foldl' apply prefix suffix
    Nothing -> prefix

intrinsicParser :: Parser Intrinsic
intrinsicParser =
  (<?> "intrinsic") $ do
    origin <- getTokenOrigin <* parserMatch Token.Intrinsic
    suffix <- unqualifiedNameParser <?> "intrinsic name"
    name <- Qualified <$> Parsec.getState <*> pure suffix
    sig <- signatureParser <?> "intrinsic signature"
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
    name <- qualifiedNameParser <?> "word definition name"
    sig <- signatureParser <?> "word definition signature"
    body <- blockLikeParser <?> "word definition body"
    pure
      WordDefinition
        { Definition._wordBody = body,
          Definition._wordInferSignature = False,
          Definition._wordMerge = Merge.Deny,
          Definition._wordName = name,
          Definition._wordOrigin = origin,
          Definition._wordSignature = sig
        }

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
    LocalName _ -> ice "Mlatu.Parse.qualifiedNameParser - name parser should only pure qualified or unqualified name"

signatureParser :: Parser Signature
signatureParser =
  groupedParser
    ( quantifiedParser functionTypeParser
        <|> functionTypeParser
    )
    <?> "type signature"

kindParser :: Parser Kind
kindParser =
  ( ( do
        _ <- parserMatch_ Token.Value
        pure Value
    )
      <|> ( do
              _ <- parserMatch_ Token.Stack
              pure Stack
          )
  )
    <?> "kind"

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
          Word () <$> nameParser <*> pure [] <*> pure origin,
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
        name <- nameParser
        pure (Word () name [] origin),
      Parsec.try sectionParser,
      Parsec.try groupParser <?> "parenthesized expression",
      vectorParser,
      lambdaParser,
      matchParser,
      ifParser,
      Push () <$> blockValue <*> pure origin,
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
                         (QualifiedName (Qualified Vocabulary.intrinsic "swap"))
                         []
                         origin,
                       Word () (UnqualifiedName function) [] origin
                     ]
        ]

vectorParser :: Parser (Term ())
vectorParser = (<?> "vector literal") $ do
  vectorOrigin <- getTokenOrigin
  es <-
    bracketedParser $
      ((\ts -> compose () vectorOrigin (ts ++ [Word () "cons" [] vectorOrigin])) <$> Parsec.many1 termParser)
        `Parsec.sepEndBy` commaParser
  pure $ compose () vectorOrigin $ Word () "nil" [] vectorOrigin : es

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
            name <- nameParser
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

blockValue :: Parser (Value ())
blockValue = (<?> "quotation") $ Quotation <$> blockParser

asParser :: Parser (Term ())
asParser = (<?> "'as' expression") $ do
  origin <- getTokenOrigin <* parserMatch_ Token.As
  signatures <- groupedParser $ basicTypeParser `Parsec.sepEndBy` commaParser
  pure $ Term.asCoercion () origin signatures

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
