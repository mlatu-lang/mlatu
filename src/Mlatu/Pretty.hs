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

import Data.Char (isLetter)
import Data.HashMap.Strict qualified as HashMap
import Data.List (findIndex, groupBy)
import Data.Text qualified as Text
import Mlatu.Base qualified as Base
import Mlatu.DataConstructor qualified as DataConstructor
import Mlatu.Declaration (Declaration (..))
import Mlatu.Declaration qualified as Declaration
import Mlatu.Definition (Definition (Definition), mainName)
import Mlatu.Entry (Entry)
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Category qualified as Category
import Mlatu.Entry.Parameter (Parameter (..))
import Mlatu.Entry.Parent qualified as Parent
import Mlatu.Fragment (Fragment (..))
import Mlatu.Fragment qualified as Fragment
import Mlatu.Instantiated (Instantiated (..))
import Mlatu.Kind (Kind (..))
import Mlatu.Literal (FloatLiteral (..), IntegerLiteral (..), floatValue)
import Mlatu.Metadata (Metadata (..))
import Mlatu.Metadata qualified as Metadata
import Mlatu.Name (Closed (..), ClosureIndex (..), GeneralName (..), LocalIndex (..), Qualified (..), Qualifier (..), Root (..), Unqualified (..))
import Mlatu.Operator qualified as Operator
import Mlatu.Origin qualified as Origin
import Mlatu.Signature (Constraint (..), Signature (..))
import Mlatu.Synonym (Synonym (Synonym))
import Mlatu.Term (Case (..), CoercionHint (..), Else (..), MatchHint (..), Term (..), Value (..))
import Mlatu.Term qualified as Term
import Mlatu.Token qualified as Token
import Mlatu.Type (Constructor (..), Type (..), TypeId (..), Var (..))
import Mlatu.Type qualified as Type
import Mlatu.TypeDefinition (TypeDefinition (..))
import Numeric (showIntAtBase)
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
        case integerBase literal of
          Base.Binary -> Just "0b"
          Base.Octal -> Just "0o"
          Base.Decimal -> Nothing
          Base.Hexadecimal -> Just "0x",
        Just $ pretty $ showIntAtBase base (\i -> Unsafe.fromJust (digits !!? i)) (abs value) ""
      ]
  where
    sign = if value < 0 then Just "-" else Nothing
    value = integerValue literal
    (base, digits) = case integerBase literal of
      Base.Binary -> (2, "01")
      Base.Octal -> (8, ['0' .. '7'])
      Base.Decimal -> (10, ['0' .. '9'])
      Base.Hexadecimal -> (16, ['0' .. '9'] ++ ['A' .. 'F'])

printFloatLiteral :: FloatLiteral -> Doc a
printFloatLiteral literal =
  if value < 0 then "-" else "" <> pretty value
  where
    value :: Double
    value = floatValue literal

printParameter :: Parameter -> Doc a
printParameter (Parameter _ name Value) = printUnqualified name
printParameter (Parameter _ name Stack) = printUnqualified name <> "..."
printParameter (Parameter _ name Label) = "+" <> printUnqualified name
printParameter (Parameter _ name Permission) = "+" <> printUnqualified name
printParameter (Parameter _ name (_ :-> _)) = printUnqualified name <> "[_]"

printQualified :: Qualified -> Doc a
printQualified (Qualified (Qualifier Absolute []) unqualifiedName) = printUnqualified unqualifiedName
printQualified (Qualified qualifier unqualifiedName) =
  printQualifier qualifier <> "::" <> printUnqualified unqualifiedName

printQualifier :: Qualifier -> Doc a
printQualifier (Qualifier Absolute parts) = printQualifier $ Qualifier Relative ("_" : parts)
printQualifier (Qualifier Relative parts) =
  pretty $ Text.intercalate "::" parts

printOrigin :: Origin.Origin -> Doc a
printOrigin origin =
  pretty (Origin.name origin) <> colon
    <> pretty al
    <> dot
    <> pretty ac
    <> "-"
    <> (if al == bl then pretty bc else pretty bl <> dot <> pretty bc)
  where
    al = Origin.beginLine origin
    bl = Origin.endLine origin
    ac = Origin.beginColumn origin
    bc = Origin.endColumn origin

printCategory :: Category.Category -> Doc a
printCategory Category.Constructor = "constructor"
printCategory Category.Instance = "instance"
printCategory Category.Permission = "permission"
printCategory Category.Word = "word"

printKind :: Kind -> Doc a
printKind Value = "value"
printKind Stack = "stack"
printKind Label = "label"
printKind Permission = "permission"
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

printSynonym :: Synonym -> Doc a
printSynonym (Synonym name1 name2 _) = "synonym" <+> printQualified name1 <> printGeneralName name2

printConstructor :: Constructor -> Doc a
printConstructor (Constructor name) = printQualified name

printType :: Type -> Doc a
printType type0 = recur type0
  where
    context = buildContext type0
    recur typ = case typ of
      TypeConstructor _ "Fun" :@ a :@ b :@ p ->
        parens $
          recur a <+> "->" <> recur b <+> recur p
      TypeConstructor _ "Fun" :@ a :@ b ->
        parens $
          recur a <+> "->" <+> recur b
      TypeConstructor _ "Fun" :@ a ->
        parens $
          recur a <+> "->"
      TypeConstructor _ "Fun" -> parens "->"
      TypeConstructor _ "Prod" :@ a :@ b ->
        punctuateComma [recur a, recur b]
      TypeConstructor _ "Prod" :@ a ->
        parens $ recur a <> comma <> space
      TypeConstructor _ "Prod" ->
        parens comma
      TypeConstructor _ "Sum" :@ a :@ b ->
        recur a <+> "|" <+> recur b
      TypeConstructor _ "Join" :@ a :@ b ->
        "+" <> recur a <+> recur b
      TypeConstructor _ "Join" :@ a ->
        parens $ "+" <> recur a
      a :@ b -> recur a <> brackets (recur b)
      TypeConstructor _ constructor -> printConstructor constructor
      TypeVar _ var@(Var name i k) ->
        -- The default cases here shouldn't happen if the context was built
        -- correctly, so it's fine if we fall back to something ugly.
        fromMaybe (printVar var) $ do
          ids <- HashMap.lookup name context
          case ids of
            -- Only one variable with this name: print without index.
            [(i', _)] | i == i' -> pure $ prettyKinded name k
            -- No variables with this name: ugly-print.
            [] -> pure $ printVar var
            -- Multiple variables with this name: print with index.
            _list -> do
              index <- findIndex ((== i) . fst) ids
              let Unqualified unqualified = name
              pure $
                prettyKinded
                  ( Unqualified
                      (unqualified <> "_" <> show (index + 1))
                  )
                  k
      TypeConstant o var -> "∃" <> recur (TypeVar o var)
      Forall {} -> prettyForall typ []
        where
          prettyForall (Forall _ x t) vars = prettyForall t (x : vars)
          prettyForall t vars =
            list (map (recur . TypeVar (Type.origin t)) vars)
              <> parens (recur t)
      TypeValue _ value -> pretty value

type PrettyContext = HashMap Unqualified [(TypeId, Kind)]

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
      TypeValue {} -> context
      where
        record name i k = HashMap.insertWith (<>) name [(i, k)]

printTypeId :: TypeId -> Doc a
printTypeId (TypeId i) = "T" <> pretty i

printVar :: Var -> Doc a
printVar (Var (Unqualified unqualified) i k) =
  prettyKinded
    ( Unqualified $
        mconcat
          [ unqualified,
            "_",
            show $ printTypeId i
          ]
    )
    k

prettyKinded :: Unqualified -> Kind -> Doc a
prettyKinded name k = case k of
  Permission -> "+" <> printUnqualified name
  Stack -> printUnqualified name <> "..."
  _otherKind -> printUnqualified name

printConstraint :: Constraint -> Doc a
printConstraint (Constraint name params) = printUnqualified name <> brackets (punctuateComma (map printParameter params))

printSignature :: Signature -> Doc a
printSignature (Application firstA b _) =
  printSignature finalA <> brackets (punctuateComma (map printSignature (as ++ [b])))
  where
    (finalA, as) = go [] firstA
    go l (Application x y _) = go (l ++ [y]) x
    go l x = (x, l)
printSignature (Bottom _) = "<bottom>"
printSignature (Function as bs es _) =
  parens $
    mapNonEmpty "" (\sigs -> punctuateComma (map printSignature sigs) <> space) as
      <> "->"
      <> mapNonEmpty "" (\sigs -> space <> punctuateComma (map printSignature sigs)) bs
      <> mapNonEmpty "" (\names -> space <> hsep (map (\p -> "+" <> printGeneralName p) names)) es
printSignature (Quantified names constraints typ _) =
  mapNonEmpty "" (\ns -> brackets (punctuateComma (map printParameter ns)) <> flatAlt space "\n") names
    <> printSignature typ
    <> mapNonEmpty "" (\cs -> space <> "where" <+> hsep (map printConstraint cs)) constraints
printSignature (Variable name _) = printGeneralName name
printSignature (StackFunction r as s bs es _) =
  parens $
    punctuateComma ((printSignature r <> "...") : map printSignature as)
      <+> "->"
      <+> punctuateComma ((printSignature s <> "...") : map printSignature bs)
      <> mapNonEmpty "" (\xs -> space <> hsep (map (("+" <>) . printGeneralName) xs)) es
printSignature (Type t) = printType t

printToken :: Token.Token -> Doc a
printToken = \case
  Token.About -> "about"
  Token.AngleBegin -> "<"
  Token.AngleEnd -> ">"
  Token.Arrow -> "->"
  Token.As -> "as"
  Token.BlockBegin -> "{"
  Token.BlockEnd -> "}"
  Token.Case -> "case"
  Token.Character c -> squotes $ pretty c
  Token.Colon -> ":"
  Token.Comma -> ","
  Token.Define -> "define"
  Token.Do -> "do"
  Token.Ellipsis -> "..."
  Token.Else -> "else"
  Token.Float a -> pretty (floatValue a :: Double)
  Token.GroupBegin -> "("
  Token.GroupEnd -> ")"
  Token.If -> "if"
  Token.Ignore -> "_"
  Token.Instance -> "instance"
  Token.Integer literal -> printIntegerLiteral literal
  Token.Intrinsic -> "intrinsic"
  Token.Match -> "match"
  Token.Operator name -> printUnqualified name
  Token.Permission -> "permission"
  Token.Reference -> "\\"
  Token.Return -> "return"
  Token.Synonym -> "synonym"
  Token.Text t -> dquotes $ pretty t
  Token.Trait -> "trait"
  Token.Type -> "type"
  Token.VectorBegin -> "["
  Token.VectorEnd -> "]"
  Token.Vocab -> "vocab"
  Token.VocabLookup -> "::"
  Token.Where -> "where"
  Token.With -> "with"
  Token.Word name -> printUnqualified name

-- Minor hack because Parsec requires 'Show'.
instance Show Token.Token where
  show = show . printToken

printInstantiated :: Instantiated -> Doc a
printInstantiated (Instantiated n ts) =
  printQualified n <> "::" <> list (map printType ts) <> ""

printDataConstructor :: DataConstructor.DataConstructor -> Doc a
printDataConstructor (DataConstructor.DataConstructor fields name _) =
  "case"
    <+> printUnqualified name
    <> printedFields
  where
    printedFields = if not $ null fields then space <> tupled (map printSignature fields) else ""

printDeclaration :: Declaration -> Doc a
printDeclaration (Declaration category name _ signature) = printedCategory <+> printUnqualified (unqualifiedName name) <+> printSignature signature
  where
    printedCategory = case category of
      Declaration.Trait -> "trait"
      Declaration.Intrinsic -> "intrinsic"

printTypeDefinition :: TypeDefinition -> Doc a
printTypeDefinition (TypeDefinition constructors name _ parameters) =
  blockMaybeMulti ("type" <+> typeName) printDataConstructor constructors
  where
    typeName =
      printQualified name
        <> if not $ null $ fst parameters then (list . map printParameter . fst) parameters else ""

printTerm :: Term a -> Doc b
printTerm t = fromMaybe "" (maybePrintTerms (decompose t))
  where
    decompose term = go (Term.decompose (Term.stripMetadata term))

    go [] = []
    go (Word _ Operator.Postfix (QualifiedName (Qualified _ name)) [] o : xs)
      | name == "drop" = [Lambda () "_" () (Term.compose () o (concatMap (Term.decompose . Term.stripMetadata) xs)) o]
    go (x : xs) = x : go xs

maybePrintTerms :: [Term a] -> Maybe (Doc b)
maybePrintTerms = \case
  [] -> Nothing
  (Group a : Match BooleanMatch _ cases _ _ : xs) -> Just (printIf (Just a) cases `justVertical` xs)
  (Group a : Match AnyMatch _ cases (DefaultElse _ _) _ : xs) -> Just (printMatch (Just a) cases Nothing `justVertical` xs)
  (Group a : Match AnyMatch _ cases (Else elseBody _) _ : xs) -> Just (printMatch (Just a) cases (Just elseBody) `justVertical` xs)
  (Group a : NewVector _ 1 _ _ : xs) -> (brackets <$> maybePrintTerm a) `horiz` xs
  (Push _ (Quotation (Word _ fixity name args _)) _ : Group a : xs) -> Just ((backslash <> printWord fixity name args) `justHoriz` (Group a : xs))
  (Push _ (Quotation body) _ : Group a : xs) -> Just (printDo a body `justVertical` xs)
  (Coercion (AnyCoercion (Quantified [Parameter _ r1 Stack, Parameter _ s1 Stack] [] (Function [StackFunction (Variable r2 _) [] (Variable s2 _) [] grantNames _] [StackFunction (Variable r3 _) [] (Variable s3 _) [] revokeNames _] [] _) _)) _ _ : Word _ _ (QualifiedName (Qualified _ u)) _ _ : xs)
    | r1 == "R" && s1 == "S" && r2 == "R" && s2 == "S" && r3 == "R" && s3 == "S" && u == "call" ->
      Just (("with" <> space <> tupled (map (\g -> "+" <> printGeneralName g) grantNames ++ map (\r -> "-" <> printGeneralName r) revokeNames)) `justHoriz` xs)
  (Coercion (AnyCoercion _) _ _ : xs) -> Nothing `horiz` xs
  (Group (Group a) : xs) -> printGroup a `horiz` xs
  (Group a : xs) -> printGroup a `horiz` xs
  (Lambda _ name _ body _ : xs) -> Just (printLambda name body `justHoriz` xs)
  (Match BooleanMatch _ cases _ _ : xs) -> Just (printIf Nothing cases `justVertical` xs)
  (Match AnyMatch _ cases (DefaultElse _ _) _ : xs) -> Just (vsep [printMatch Nothing cases Nothing] `justVertical` xs)
  (Match AnyMatch _ cases (Else elseBody _) _ : xs) -> Just (vsep [printMatch Nothing cases (Just elseBody)] `justVertical` xs)
  (Push _ value _ : xs) -> Just (printValue value `justHoriz` xs)
  (Word _ Operator.Postfix (QualifiedName (Qualified _ name)) [] o : xs)
    | name == "drop" -> do
      let lambda = printToken Token.Arrow <+> punctuateComma (reverse (map printUnqualified ("_" : names))) <> semi
      ( case maybePrintTerm newBody of
          Nothing -> Just lambda
          Just a -> Just (vsep [lambda, a])
        )
    where
      (names, newBody) = go [] (Term.compose () o (map Term.stripMetadata xs))
      go ns (Lambda _ n _ b _) = go (n : ns) b
      go ns b = (ns, b)
  (Word _ fixity name args _ : xs) -> Just (printWord fixity name args `justHoriz` xs)
  (NewVector _ 0 _ _ : xs) -> Just (lbracket <> rbracket `justHoriz` xs)
  (t : _) -> error $ "Formatting failed: " <> show (Term.stripMetadata t)
  where
    horiz :: Maybe (Doc b) -> [Term a] -> Maybe (Doc b)
    horiz = \case
      Nothing -> maybePrintTerms
      Just a -> Just . justHoriz a

    justHoriz :: Doc b -> [Term a] -> Doc b
    justHoriz a l = 
      let (before, after) = break (\case
            Match {} -> True 
            _ -> False) l
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
printDo a body = blockMaybe (maybe "do" ("do" <+>) (printGroup a)) (maybePrintTerm body)

printGroup :: Term a -> Maybe (Doc b)
printGroup (Group a) = parens <$> maybePrintTerm a
printGroup a = parens <$> maybePrintTerm a

printLambda :: Unqualified -> Term a -> Doc b
printLambda name body =
  let lambda = printToken Token.Arrow <+> punctuateComma (map printUnqualified names) <> semi
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
    [ blockMaybe (maybe "if" ("if" <+>) (cond >>= printGroup)) (maybePrintTerm trueBody),
      blockMaybe "else" (maybePrintTerm falseBody)
    ]
printIf _ _ = error "Expected a true and false case"

printMatch :: Maybe (Term a) -> [Case a] -> Maybe (Term a) -> Doc b
printMatch cond cases else_ =
  vsep
    ( maybe "match" ("match" <+>) (cond >>= printGroup) :
      ( map
          (\(Case n b _) -> printCase n b)
          cases
          ++ maybe
            []
            (\e -> [blockMaybe "else" (maybePrintTerm e)])
            else_
      )
    )

printCase :: GeneralName -> Term a -> Doc b
printCase n (Lambda _ name _ body _) =
  blockMaybe
    ("case" <+> printGeneralName n <+> printToken Token.Arrow <+> punctuateComma (map printUnqualified names))
    (maybePrintTerm newBody)
  where
    (names, newBody) = go [name] body
    go ns (Lambda _ ln _ b _) = go (ln : ns) b
    go ns b = (ns, b)
printCase n b = blockMaybe ("case" <+> printGeneralName n) (maybePrintTerm b)

printWord :: Operator.Fixity -> GeneralName -> [Type] -> Doc a
printWord Operator.Postfix (UnqualifiedName (Unqualified name)) []
  | not (Text.any isLetter name) = parens $ pretty name
printWord _ word args = printGeneralName word <> mapNonEmpty "" (\xs -> "::" <> list (map printType xs)) args

printValue :: Value a -> Doc b
printValue value = case value of
  Capture names term ->
    "$"
      <> tupled (map printClosed names)
      <> braces (printTerm term)
  Character c -> squotes (pretty c)
  Closed (ClosureIndex index) -> "closure" <> dot <> pretty index
  Float f -> printFloatLiteral f
  Integer i -> printIntegerLiteral i
  Local (LocalIndex index) -> "local" <> dot <> pretty index
  Name n -> backslash <> printQualified n
  Quotation w@Word {} -> backslash <> printTerm w
  Quotation body -> lbrace <+> printTerm body <+> rbrace
  Text text -> (if Text.count "\n" text > 1 then multiline else singleline) text
    where
      multiline :: Text -> Doc a
      multiline t = vsep ([multilineQuote] ++ map pretty (lines t) ++ [multilineQuote])
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
    ("about" <+> printGeneralName (Metadata.name metadata) <+> lbrace) :
    map
      (\(key, term) -> indent 2 $ blockMaybe (printUnqualified key) (maybePrintTerm term))
      ( HashMap.toList $
          fields metadata
      )
      ++ [rbrace]

printDefinition :: (Show a) => Definition a -> Maybe (Doc b)
printDefinition (Definition Category.Constructor _ _ _ _ _ _ _ _) = Nothing
printDefinition (Definition Category.Permission name body _ _ _ _ signature _) =
  Just $ blockMaybe ("permission" <+> (printQualified name <+> printSignature signature)) (maybePrintTerm body)
printDefinition (Definition Category.Instance name body _ _ _ _ signature _) =
  Just $ blockMaybe ("instance" <+> (printQualified name <+> printSignature signature)) (maybePrintTerm body)
printDefinition (Definition Category.Word name body _ _ _ _ _ _)
  | name == mainName = maybePrintTerm body
printDefinition (Definition Category.Word name body _ _ _ _ signature _) =
  Just $ blockMaybe ("define" <+> (printQualified name <+> group (printSignature signature))) (maybePrintTerm body)

printEntry :: Entry -> Doc a
printEntry (Entry.Word category _ origin mParent mSignature _) =
  vsep
    [ printCategory category,
      hsep ["defined at", printOrigin origin],
      case mSignature of
        Just signature ->
          hsep
            ["with signature", dquotes $ printSignature signature]
        Nothing -> "with no signature",
      case mParent of
        Just parent ->
          hsep
            [ "with parent",
              case parent of
                Parent.Trait a -> "trait" <+> printQualified a
                Parent.Type a -> "type" <+> printQualified a
            ]
        Nothing -> "with no parent"
    ]
printEntry (Entry.Metadata origin term) =
  vsep
    [ "metadata",
      hsep ["defined at", printOrigin origin],
      hsep ["with contents", printTerm term]
    ]
printEntry (Entry.Synonym origin name) =
  vsep
    [ "synonym",
      hsep ["defined at", printOrigin origin],
      hsep ["standing for", printQualified name]
    ]
printEntry (Entry.Trait origin signature) =
  vsep
    [ "trait",
      hsep ["defined at", printOrigin origin],
      hsep ["with signature", printSignature signature]
    ]
printEntry (Entry.Type origin parameters constraints ctors) =
  vsep
    [ "type",
      hsep ["defined at", printOrigin origin],
      hsep $
        "with parameters [" :
        intersperse ", " (map printParameter parameters)
          ++ ["] constrained so that "]
          ++ intersperse "," (map printConstraint constraints),
      nestTwo $ vsep $ "and data constructors" : map constructor ctors
    ]
  where
    constructor ctor =
      hsep
        [ printUnqualified $ DataConstructor.name ctor,
          " with fields (",
          hsep $
            intersperse ", " $
              map printSignature $ DataConstructor.fields ctor,
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
    ( map
        (\g -> printGrouped g <> line)
        groupedDeclarations
        ++ map
          (\s -> printSynonym s <> line)
          ( sort (synonyms fragment)
          )
        ++ map
          (\t -> printTypeDefinition t <> line)
          ( sort (Fragment.types fragment)
          )
        ++ mapMaybe
          (fmap (<> line) . printDefinition)
          ( sort (definitions fragment)
          )
        ++ map
          (\m -> printMetadata m <> line)
          ( sort (metadata fragment)
          )
    )
  where
    groupedDeclarations = groupBy (\a b -> (qualifierName . Declaration.name) a == (qualifierName . Declaration.name) b) (declarations fragment)
    printGrouped decls =
      if noVocab
        then vsep (map (\d -> printDeclaration d <> line) $ sort decls)
        else vsep $ ("vocab" <+> printQualifier commonName <+> lbrace) : map (indent 2 . printDeclaration) (sort decls) ++ [rbrace]
      where
        (commonName, noVocab) = case qualifierName $ Declaration.name $ decls Unsafe.!! 0 of
          (Qualifier Absolute parts) -> (Qualifier Relative parts, null parts)
          n -> (n, False)

nestTwo :: Doc a -> Doc a
nestTwo = nest 2

block :: Doc a -> Doc a -> Doc a
block pre inner = vsep [pre <> space <> lbrace, indent 2 inner, rbrace]

blockMaybe :: Doc a -> Maybe (Doc a) -> Doc a
blockMaybe pre Nothing = pre <+> (lbrace <> rbrace)
blockMaybe pre (Just inner) = block pre inner

blockMaybeMulti :: Doc a -> (b -> Doc a) -> [b] -> Doc a
blockMaybeMulti pre f = mapNonEmpty (blockMaybe pre Nothing) (\xs -> vsep [pre <> space <> lbrace, indent 2 (vsep (map f xs)), rbrace])

mapNonEmpty :: b -> ([a] -> b) -> [a] -> b
mapNonEmpty emptyCase nonEmptyFn l = fromMaybe emptyCase (viaNonEmpty (nonEmptyFn . toList) l)