{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Mlatu.Pretty
-- Description : Pretty-printing utilities
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Pretty
  ( printConstructor,
    printGeneralName,
    printOrigin,
    printQualified,
    printSignature,
    printTerm,
    printType,
    printInstantiated,
    printKind,
    printEntry,
    printFragment,
    printQualifier,
  )
where

import Data.List (findIndex, groupBy)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Mlatu.DataConstructor qualified as DataConstructor
import Mlatu.Definition (WordDefinition (..), mainName)
import Mlatu.Entry (Entry)
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Parameter (Parameter (..))
import Mlatu.Fragment (Fragment (..))
import Mlatu.Fragment qualified as Fragment
import Mlatu.Ice (ice)
import Mlatu.Instantiated (Instantiated (..))
import Mlatu.Intrinsic (Intrinsic (..))
import Mlatu.Kind (Kind (..))
import Mlatu.Literal (Base (..), FloatLiteral (..), IntegerLiteral (..), floatValue, integerBase, integerValue)
import Mlatu.Metadata (Metadata (..))
import Mlatu.Metadata qualified as Metadata
import Mlatu.Name (Closed (..), ClosureIndex (..), GeneralName (..), LocalIndex (..), Qualified (..), Qualifier (..), Root (..), Unqualified (..))
import Mlatu.Origin qualified as Origin
import Mlatu.Signature (Signature (..))
import Mlatu.Term (Case (..), CoercionHint (..), Else (..), MatchHint (..), Term (..), Value (..))
import Mlatu.Term qualified as Term
import Mlatu.Token qualified as Token
import Mlatu.Type (Constructor (..), Type (..), TypeId (..), Var (..))
import Mlatu.Type qualified as Type
import Mlatu.TypeDefinition (TypeDefinition (..))
import Numeric (showIntAtBase)
import Optics
import Prettyprinter
import Relude hiding (Compose, Constraint, Type, group)
import Relude.Unsafe qualified as Unsafe
import Text.Show qualified

punctuateComma :: [Doc a] -> Doc a
punctuateComma = hsep . punctuate comma

printIntegerLiteral :: IntegerLiteral -> Doc a
printIntegerLiteral literal =
  hsep $
    catMaybes
      [ sign,
        case view integerBase literal of
          Binary -> Just "0b"
          Octal -> Just "0o"
          Decimal -> Nothing
          Hexadecimal -> Just "0x",
        Just $ pretty $ showIntAtBase base (\i -> Unsafe.fromJust (digits !!? i)) (abs value) ""
      ]
  where
    sign = if value < 0 then Just "-" else Nothing
    value = view integerValue literal
    (base, digits) = case view integerBase literal of
      Binary -> (2, "01")
      Octal -> (8, ['0' .. '7'])
      Decimal -> (10, ['0' .. '9'])
      Hexadecimal -> (16, ['0' .. '9'] ++ ['A' .. 'F'])

printFloatLiteral :: FloatLiteral -> Doc a
printFloatLiteral literal =
  if value < 0 then "-" else "" <> pretty value
  where
    value :: Double
    value = floatValue literal

printParameter :: Parameter -> Doc a
printParameter (Parameter _ name Value _) = printUnqualified name
printParameter (Parameter _ name Stack _) = printUnqualified name
printParameter (Parameter _ name (_ :-> _) _) = printUnqualified name <> "[_]"

printQualified :: Qualified -> Doc a
printQualified (Qualified (Qualifier Absolute []) unqualifiedName) = printUnqualified unqualifiedName
printQualified (Qualified qualifier unqualifiedName) =
  printQualifier qualifier <> "::" <> printUnqualified unqualifiedName

printQualifier :: Qualifier -> Doc a
printQualifier (Qualifier Absolute parts) = printQualifier $ Qualifier Relative parts
printQualifier (Qualifier Relative parts) =
  pretty $ Text.intercalate "::" parts

printOrigin :: Origin.Origin -> Doc a
printOrigin origin =
  pretty (view Origin.name origin) <> colon
    <> pretty al
    <> dot
    <> pretty ac
    <> "-"
    <> (if al == bl then pretty bc else pretty bl <> dot <> pretty bc)
  where
    al = view Origin.beginLine origin
    bl = view Origin.endLine origin
    ac = view Origin.beginColumn origin
    bc = view Origin.endColumn origin

printKind :: Kind -> Doc a
printKind Value = "value"
printKind Stack = "stack"
printKind (a :-> b) =
  parens $
    printKind a <+> "->" <+> printKind b

printUnqualified :: Unqualified -> Doc a
printUnqualified (Unqualified unqualified) = pretty unqualified

printGeneralName :: GeneralName -> Doc a
printGeneralName (QualifiedName qualified) = printQualified qualified
printGeneralName (UnqualifiedName unqualified) = printUnqualified unqualified
printGeneralName (LocalName index) = printLocalIndex index

printLocalIndex :: LocalIndex -> Doc a
printLocalIndex (LocalIndex i) = "local." <> pretty i

printClosureIndex :: ClosureIndex -> Doc a
printClosureIndex (ClosureIndex i) = "closure." <> pretty i

printClosed :: Closed -> Doc a
printClosed (ClosedLocal index) = printLocalIndex index
printClosed (ClosedClosure index) = printClosureIndex index

printConstructor :: Constructor -> Doc a
printConstructor (Constructor name) = printQualified name

printType :: Type -> Doc a
printType type0 = recur type0
  where
    context = buildContext type0
    recur typ = case typ of
      Type.Fun _ a b ->
        parens $
          recur a <+> "->" <> recur b
      TypeConstructor _ "Fun" :@ a ->
        parens $
          recur a <+> "->"
      TypeConstructor _ "Fun" -> parens "->"
      Type.Prod _ a b ->
        punctuateComma [recur a, recur b]
      TypeConstructor _ "Prod" :@ a ->
        parens $ recur a <> comma <> space
      TypeConstructor _ "Prod" ->
        parens comma
      Type.Sum _ a b ->
        recur a <+> "|" <+> recur b
      a :@ b -> recur a <> brackets (recur b)
      TypeConstructor _ constructor -> printConstructor constructor
      TypeVar _ var@(Var name i _) ->
        -- The default cases here shouldn't happen if the context was built
        -- correctly, so it's fine if we fall back to something ugly.
        fromMaybe (printVar var) $ do
          ids <- Map.lookup name context
          case ids of
            -- Only one variable with this name: print without index.
            [(i', _)] | i == i' -> pure $ printUnqualified name
            -- No variables with this name: ugly-print.
            [] -> pure $ printVar var
            -- Multiple variables with this name: print with index.
            _list -> do
              index <- findIndex ((== i) . fst) ids
              let Unqualified unqualified = name
              pure $
                printUnqualified
                  ( Unqualified
                      (unqualified <> "_" <> show (index + 1))
                  )
      TypeConstant o var -> "∃" <> recur (TypeVar o var)
      Forall {} -> prettyForall typ []
        where
          prettyForall (Forall _ x t) vars = prettyForall t (x : vars)
          prettyForall t vars =
            list (recur . TypeVar (Type.origin t) <$> vars)
              <> parens (recur t)

type PrettyContext = Map Unqualified [(TypeId, Kind)]

buildContext :: Type -> PrettyContext
buildContext = go mempty
  where
    go :: PrettyContext -> Type -> PrettyContext
    go context typ = case typ of
      a :@ b -> go context a <> go context b
      TypeConstructor {} -> context
      TypeVar _ (Var name i k) -> record name i k context
      TypeConstant _ (Var name i k) -> record name i k context
      Forall _ (Var name i k) t -> go (record name i k context) t
      where
        record name i k = Map.insertWith (<>) name [(i, k)]

printTypeId :: TypeId -> Doc a
printTypeId (TypeId i) = "T" <> pretty i

printVar :: Var -> Doc a
printVar (Var (Unqualified unqualified) i _) =
  printUnqualified
    ( Unqualified $
        mconcat
          [ unqualified,
            "_",
            show $ printTypeId i
          ]
    )

printSignature :: Signature -> Doc a
printSignature (Application firstA b _) =
  printSignature finalA <> brackets (punctuateComma (printSignature <$> (as ++ [b])))
  where
    (finalA, as) = go [] firstA
    go l (Application x y _) = go (l ++ [y]) x
    go l x = (x, l)
printSignature (Bottom _) = "<bottom>"
printSignature (Function as bs _) =
  parens $
    mapNonEmpty "" (\sigs -> punctuateComma (printSignature <$> sigs) <> space) as
      <> "->"
      <> mapNonEmpty "" (\sigs -> space <> punctuateComma (printSignature <$> sigs)) bs
printSignature (Quantified names typ _) =
  "forall" <+> parens (punctuateComma (printParameterGroup <$> groups)) <+> "." <+> printSignature typ
  where
    printParameterGroup [] = ""
    printParameterGroup params@((Parameter _ _ k _) : _) = hsep (printParameter <$> params) <+> ":" <+> printKind k
    groups = groupBy (\(Parameter _ _ k1 _) (Parameter _ _ k2 _) -> k1 == k2) names
printSignature (Variable name _) = printGeneralName name
printSignature (Type t) = printType t

printToken :: Token.Token -> Doc a
printToken = \case
  Token.About -> "about"
  Token.Arrow -> "->"
  Token.As -> "as"
  Token.BlockBegin -> "{"
  Token.BlockEnd -> "}"
  Token.Case -> "case"
  Token.Character c -> squotes $ pretty c
  Token.Colon -> ":"
  Token.Comma -> ","
  Token.Define -> "define"
  Token.Dot -> "."
  Token.Else -> "else"
  Token.Field -> "field"
  Token.Float a -> pretty (floatValue a :: Double)
  Token.Forall -> "forall"
  Token.GroupBegin -> "("
  Token.GroupEnd -> ")"
  Token.If -> "if"
  Token.Ignore -> "_"
  Token.Integer literal -> printIntegerLiteral literal
  Token.Intrinsic -> "intrinsic"
  Token.Match -> "match"
  Token.Operator name -> printUnqualified name
  Token.Record -> "record"
  Token.Reference -> "\\"
  Token.Stack -> "Stack"
  Token.Text t -> dquotes $ pretty t
  Token.Type -> "type"
  Token.Value -> "Value"
  Token.VectorBegin -> "["
  Token.VectorEnd -> "]"
  Token.Vocab -> "vocab"
  Token.VocabLookup -> "::"
  Token.Where -> "where"
  Token.Word name -> printUnqualified name

-- Minor hack because Parsec requires 'Show'.
instance Show Token.Token where
  show = show . printToken

printInstantiated :: Instantiated -> Doc a
printInstantiated (Instantiated n []) = printQualified n
printInstantiated (Instantiated n ts) =
  printQualified n <> "::" <> list (printType <$> ts)

printDataConstructor :: DataConstructor.DataConstructor -> Doc a
printDataConstructor (DataConstructor.DataConstructor fields name _) =
  "case"
    <+> printUnqualified name
    <> printedFields
  where
    printedFields = if not $ null fields then space <> tupled (printSignature <$> fields) else ""

printIntrinsic :: Intrinsic -> Doc a
printIntrinsic (Intrinsic name _ signature) = "intrinsic" <+> printQualified name <+> printSignature signature

printTypeDefinition :: TypeDefinition -> Doc a
printTypeDefinition (TypeDefinition constructors name _ parameters) =
  group $ blockMulti ("type" <+> typeName) printDataConstructor constructors
  where
    typeName =
      printQualified name
        <> if not $ null parameters then (list . fmap printParameter) parameters else ""

printTerm :: Term a -> Doc b
printTerm t = fromMaybe "" $ maybePrintTerm t

maybePrintTerms :: [Term a] -> Maybe (Doc b)
maybePrintTerms = \case
  [] -> Nothing
  (Group a : Match BooleanMatch _ cases _ _ : xs) -> Just (printIf (Just a) cases `justVertical` xs)
  (Group a : Match AnyMatch _ cases (DefaultElse _ _) _ : xs) -> Just (printMatch (Just a) cases Nothing `justVertical` xs)
  (Group a : Match AnyMatch _ cases (Else elseBody _) _ : xs) -> Just (printMatch (Just a) cases (Just elseBody) `justVertical` xs)
  (Push _ (Quotation (Word _ name args _)) _ : Group a : xs) -> Just ((backslash <> printWord name args) `justHoriz` (Group a : xs))
  (Push _ (Quotation body) _ : Group a : xs) -> Just (printDo a body `justVertical` xs)
  (Coercion (AnyCoercion _) _ _ : xs) -> Nothing `horiz` xs
  (Group (Group a) : xs) -> printGroup a `horiz` xs
  (Group a : xs) -> printGroup a `horiz` xs
  (Lambda _ name _ body _ : xs) -> Just (printLambda name body `justHoriz` xs)
  (Match BooleanMatch _ cases _ _ : xs) -> Just (printIf Nothing cases `justVertical` xs)
  (Match AnyMatch _ cases (DefaultElse _ _) _ : xs) -> Just (vsep [printMatch Nothing cases Nothing] `justVertical` xs)
  (Match AnyMatch _ cases (Else elseBody _) _ : xs) -> Just (vsep [printMatch Nothing cases (Just elseBody)] `justVertical` xs)
  (Push _ value _ : xs) -> Just (printValue value `justHoriz` xs)
  (Word _ (QualifiedName (Qualified _ "drop")) [] o : xs) ->
    Just (printLambda "_" (Term.compose () o (Term.stripMetadata <$> xs)))
  (Word _ name args _ : xs) -> Just (printWord name args `justHoriz` xs)
  (t : _) -> ice $ "Mlatu.Pretty.maybePrintTerms - Formatting failed: " <> show (Term.stripMetadata t)
  where
    horiz :: Maybe (Doc b) -> [Term a] -> Maybe (Doc b)
    horiz = \case
      Nothing -> maybePrintTerms
      Just a -> Just . justHoriz a

    justHoriz :: Doc b -> [Term a] -> Doc b
    justHoriz a l =
      let (before, after) =
            break
              ( \case
                  Match {} -> True
                  _ -> False
              )
              l
       in case (maybePrintTerms before, maybePrintTerms after) of
            (Nothing, Nothing) -> a
            (Just b, Nothing) -> a <+> b
            (Nothing, Just b) -> vsep [a, b]
            (Just b, Just c) -> vsep [a <+> b, c]

    justVertical :: Doc b -> [Term a] -> Doc b
    justVertical a l = case maybePrintTerms l of
      Nothing -> a
      Just b -> vsep [a, b]

maybePrintTerm :: Term a -> Maybe (Doc b)
maybePrintTerm = maybePrintTerms . Term.decompose

printDo :: Term a -> Term a -> Doc b
printDo a body = group $ blockMaybe (maybe "do" ("do" <+>) (printGroup a)) (maybePrintTerm body)

printGroup :: Term a -> Maybe (Doc b)
printGroup a = parens <$> maybePrintTerm a

printLambda :: Unqualified -> Term a -> Doc b
printLambda name body =
  let lambda = printToken Token.Arrow <+> punctuateComma (printUnqualified <$> names) <> semi
   in case maybePrintTerm newBody of
        Nothing -> lambda
        Just a -> vsep [lambda, a]
  where
    (names, newBody) = go [name] body
    go ns (Lambda _ n _ b _) = go (n : ns) b
    go ns b = (ns, b)

printIf :: Maybe (Term a) -> [Case a] -> Doc b
printIf cond [Case _ trueBody _, Case _ falseBody _] =
  vsep
    [ group $ blockMaybe (maybe "if" ("if" <+>) (cond >>= printGroup)) (maybePrintTerm trueBody),
      group $ blockMaybe "else" (maybePrintTerm falseBody)
    ]
printIf _ _ = ice "Mlatu.Pretty.printIf - Expected a true and false case"

printMatch :: Maybe (Term a) -> [Case a] -> Maybe (Term a) -> Doc b
printMatch cond cases else_ =
  vsep
    ( maybe "match" ("match" <+>) (cond >>= printGroup) :
      ( ( (\(Case n b _) -> printCase n b)
            <$> cases
        )
          ++ maybe
            []
            (\e -> [group $ blockMaybe "else" (maybePrintTerm e)])
            else_
      )
    )

printCase :: GeneralName -> Term a -> Doc b
printCase n (Lambda _ name _ body _) =
  group $
    blockMaybe
      ("case" <+> printGeneralName n <+> printToken Token.Arrow <+> punctuateComma (printUnqualified <$> names))
      (maybePrintTerm newBody)
  where
    (names, newBody) = go [name] body
    go ns (Lambda _ ln _ b _) = go (ln : ns) b
    go ns b = (ns, b)
printCase n b = group $ blockMaybe ("case" <+> printGeneralName n) (maybePrintTerm b)

printWord :: GeneralName -> [Type] -> Doc a
printWord word args = printGeneralName word <> mapNonEmpty "" (\xs -> "::" <> list (printType <$> xs)) args

printValue :: Value a -> Doc b
printValue value = case value of
  Capture names term ->
    "$"
      <> tupled (printClosed <$> names)
      <> braces (printTerm term)
  Character c -> squotes (pretty c)
  Closed (ClosureIndex index) -> "closure" <> dot <> pretty index
  Float f -> printFloatLiteral f
  Integer i -> printIntegerLiteral i
  Local (LocalIndex index) -> "local" <> dot <> pretty index
  Name n -> backslash <> printQualified n
  Quotation w@Word {} -> backslash <> printTerm w
  Quotation body -> group $ maybeBlockMaybe Nothing (maybePrintTerm body)
  Text text -> (if Text.count "\n" text > 1 then multiline else singleline) text
    where
      multiline :: Text -> Doc a
      multiline t = vsep ([multilineQuote] ++ (pretty <$> lines t) ++ [multilineQuote])
        where
          multilineQuote = dquote <> dquote <> dquote

      singleline :: Text -> Doc a
      singleline t =
        dquotes $
          pretty
            ( concatMap
                ( \case
                    '\n' -> "\\n"
                    c -> [c]
                )
                (toString t)
            )

printMetadata :: Metadata -> Doc a
printMetadata metadata =
  vsep $
    ("about" <+> printGeneralName (view Metadata.name metadata) <+> lbrace) :
    ( (\(key, term) -> indent 2 $ blockMaybe (printUnqualified key) (maybePrintTerm term))
        <$> Map.toList
          ( view Metadata.fields metadata
          )
    )
      ++ [rbrace]

printWordDefinition :: (Show a) => WordDefinition a -> Maybe (Doc b)
printWordDefinition (WordDefinition name body _ _ _ _)
  | name == mainName = maybePrintTerm body
printWordDefinition (WordDefinition name body _ _ _ signature) =
  Just $ group $ blockMaybe ("define" <+> (printQualified name <+> printSignature signature)) (maybePrintTerm body)

printEntry :: Entry -> Doc a
printEntry (Entry.Word _ origin mSignature _) =
  vsep
    [ "word",
      hsep ["defined at", printOrigin origin],
      case mSignature of
        Just signature ->
          hsep
            ["with signature", dquotes $ printSignature signature]
        Nothing -> "with no signature"
    ]
printEntry (Entry.Metadata origin term) =
  vsep
    [ "metadata",
      hsep ["defined at", printOrigin origin],
      hsep ["with contents", printTerm term]
    ]
printEntry (Entry.Constructor origin _ signature _) =
  vsep
    [ "data constructor",
      hsep ["defined at", printOrigin origin],
      hsep ["with signature", printSignature signature]
    ]
printEntry (Entry.Type origin parameters ctors) =
  vsep
    [ "type",
      hsep ["defined at", printOrigin origin],
      hsep $
        "with parameters [" :
        intersperse ", " (printParameter <$> parameters),
      nestTwo $ vsep $ "and data constructors" : (constructor <$> ctors)
    ]
  where
    constructor ctor =
      hsep
        [ printUnqualified $ view DataConstructor.name ctor,
          " with fields (",
          hsep $
            intersperse ", " (printSignature <$> view DataConstructor.fields ctor),
          ")"
        ]
printEntry (Entry.InstantiatedType origin size) =
  vsep
    [ "instantiated type",
      hsep ["defined at", printOrigin origin],
      hsep ["with size", pretty size]
    ]

printFragment :: (Show a, Ord a) => Fragment a -> Doc b
printFragment fragment =
  vcat
    ( ( (\t -> printIntrinsic t <> line)
          <$> sort (view Fragment.intrinsics fragment)
      )
        ++ ( (\t -> printTypeDefinition t <> line)
               <$> sort (view Fragment.types fragment)
           )
        ++ mapMaybe
          (fmap (<> line) . printWordDefinition)
          ( sort (view Fragment.wordDefinitions fragment)
          )
        ++ ( (\m -> printMetadata m <> line)
               <$> sort (view Fragment.metadata fragment)
           )
    )

nestTwo :: Doc a -> Doc a
nestTwo = nest 2

block :: Doc a -> Doc a -> Doc a
block before inner = flatAlt (vsep [before <+> lbrace, indent 2 inner, rbrace]) (before <+> lbrace <+> inner <+> rbrace)

maybeBlock :: Maybe (Doc a) -> Doc a -> Doc a
maybeBlock Nothing inner = flatAlt (vsep [lbrace, indent 2 inner, rbrace]) (lbrace <+> inner <+> rbrace)
maybeBlock (Just before) inner = block before inner

blockMaybe :: Doc a -> Maybe (Doc a) -> Doc a
blockMaybe before Nothing = before <+> (lbrace <> rbrace)
blockMaybe before (Just inner) = block before inner

maybeBlockMaybe :: Maybe (Doc a) -> Maybe (Doc a) -> Doc a
maybeBlockMaybe (Just before) (Just inner) = block before inner
maybeBlockMaybe before (Just inner) = maybeBlock before inner
maybeBlockMaybe (Just before) inner = blockMaybe before inner
maybeBlockMaybe Nothing Nothing = lbrace <> rbrace

blockMulti :: Doc a -> (b -> Doc a) -> [b] -> Doc a
blockMulti before f = mapNonEmpty (blockMaybe before Nothing) (\xs -> flatAlt (vsep [before <+> lbrace, indent 2 (vsep (f <$> xs)), rbrace]) (before <+> lbrace <+> hsep (f <$> xs) <+> rbrace))

mapNonEmpty :: b -> ([a] -> b) -> [a] -> b
mapNonEmpty emptyCase nonEmptyFn l = fromMaybe emptyCase (viaNonEmpty (nonEmptyFn . toList) l)
