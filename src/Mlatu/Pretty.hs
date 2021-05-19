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
    printDefinition,
    printTypeDefinition,
    printInstantiated,
    printKind,
    printFragment,
    printQualifier,
  )
where

import Data.List (findIndex, groupBy)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Mlatu.DataConstructor qualified as DataConstructor
import Mlatu.Definition (Definition (Definition), mainName)
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Category qualified as Category
import Mlatu.Entry.Parameter (Parameter (..))
import Mlatu.Entry.Parent qualified as Parent
import Mlatu.Fragment (Fragment (..))
import Mlatu.Fragment qualified as Fragment
import Mlatu.Ice (ice)
import Mlatu.Instantiated (Instantiated (..))
import Mlatu.Kind (Kind (..))
import Mlatu.Metadata (Metadata (..))
import Mlatu.Metadata qualified as Metadata
import Mlatu.Name (Closed (..), ClosureIndex (..), GeneralName (..), LocalIndex (..), Qualified (..), Qualifier (..), Root (..), Unqualified (..))
import Mlatu.Origin qualified as Origin
import Mlatu.Signature (Signature (..))
import Mlatu.Term (Case (..), CoercionHint (..), Else (..), MatchHint (..), Term (..), Value (..))
import Mlatu.Term qualified as Term
import Mlatu.Token qualified as Token
import Mlatu.Trait (Trait (..))
import Mlatu.Trait qualified as Trait
import Mlatu.Type (Constructor (..), Type (..), TypeId (..), Var (..))
import Mlatu.Type qualified as Type
import Mlatu.TypeDefinition (TypeDefinition (..))
import Optics
import Prettyprinter
import Relude hiding (Compose, Constraint, Type, group)
import Relude.Unsafe qualified as Unsafe
import Text.Show qualified

punctuateComma :: [Doc a] -> Doc a
punctuateComma = hsep . punctuate comma

printParameter :: Parameter -> Doc a
printParameter (Parameter _ name Value _) = printUnqualified name
printParameter (Parameter _ name Stack _) = printUnqualified name
printParameter (Parameter _ name Label _) = "+" <> printUnqualified name
printParameter (Parameter _ name Permission _) = "+" <> printUnqualified name
printParameter (Parameter _ name (_ :-> _) _) = printUnqualified name <> "[_]"

printQualified :: Qualified -> Doc a
printQualified (Qualified (Qualifier Absolute []) unqualifiedName) = printUnqualified unqualifiedName
printQualified (Qualified qualifier unqualifiedName) =
  printQualifier qualifier <> "." <> printUnqualified unqualifiedName

printQualifier :: Qualifier -> Doc a
printQualifier (Qualifier Absolute parts) = printQualifier $ Qualifier Relative ("_" : parts)
printQualifier (Qualifier Relative parts) = pretty $ Text.intercalate "." parts

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

printCategory :: Category.Category -> Doc a
printCategory Category.Constructor = "constructor"
printCategory Category.Instance = "instance"
printCategory Category.Permission = "permission"
printCategory Category.Deconstructor = "deconstructor"
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

printConstructor :: Constructor -> Doc a
printConstructor (Constructor name) = printQualified name

printType :: Type -> Doc a
printType type0 = recur type0
  where
    context = buildContext type0
    recur typ = case typ of
      Type.Fun _ a b p ->
        parens $
          recur a <+> "->" <+> recur b <+> recur p
      TypeConstructor _ "fun" :@ a :@ b ->
        parens $
          recur a <+> "->" <+> recur b
      TypeConstructor _ "fun" :@ a ->
        parens $
          recur a <+> "->"
      TypeConstructor _ "fun" -> parens "->"
      Type.Prod _ a b ->
        punctuateComma [recur a, recur b]
      TypeConstructor _ "prod" :@ a ->
        parens $ recur a <> comma <> space
      TypeConstructor _ "prod" ->
        parens comma
      Type.Sum _ a b ->
        recur a <+> "|" <+> recur b
      Type.Join _ a b ->
        "+" <> recur a <+> recur b
      TypeConstructor _ "join" :@ a ->
        parens $ "+" <> recur a
      a :@ b -> recur b <+> recur a
      TypeConstructor _ constructor -> printConstructor constructor
      TypeVar _ var@(Var name i k) ->
        -- The default cases here shouldn't happen if the context was built
        -- correctly, so it's fine if we fall back to something ugly.
        fromMaybe (printVar var) $ do
          ids <- Map.lookup name context
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
                      (unqualified <> "-" <> show (index + 1))
                  )
                  k
      TypeConstant o var -> "exists" <+> recur (TypeVar o var)
      Forall {} -> prettyForall typ []
        where
          prettyForall (Forall _ x t) vars = prettyForall t (x : vars)
          prettyForall t vars =
            parens
              ( "for"
                  <+> hsep (recur . TypeVar (Type.origin t) <$> vars)
                  <+> "."
                  <+> recur t
              )
      TypeValue _ value -> pretty value

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
      TypeValue {} -> context
      where
        record name i k = Map.insertWith (<>) name [(i, k)]

printTypeId :: TypeId -> Doc a
printTypeId (TypeId i) = "t" <> pretty i

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
  _otherKind -> printUnqualified name

printSignature :: Signature -> Doc a
printSignature (Application a b _) = printSignature b <+> printSignature a
printSignature (Bottom _) = "<bottom>"
printSignature (Function as bs es _) =
  parens $
    punctuateComma (printSignature <$> as)
      <+> "->"
      <+> punctuateComma (printSignature <$> bs)
      <> case es of
        [] -> ""
        _ -> space <> encloseSep "<" ">" " + " (printGeneralName <$> es)
printSignature (Quantified names typ _) =
  parens $ "for" <+> hsep (printParameter <$> names) <> "." <+> printSignature typ
printSignature (Variable name _) = printGeneralName name
printSignature (StackFunction r as s bs es _) =
  parens $
    punctuateComma (printSignature <$> (r : as))
      <+> "->"
      <+> punctuateComma (printSignature <$> (s : bs))
      <> case es of
        [] -> ""
        _ -> space <> encloseSep "<" ">" " + " (printGeneralName <$> es)
printSignature (Type t) = printType t

printToken :: Token.Token -> Doc a
printToken = \case
  Token.About -> "about"
  Token.AngleBegin -> "<"
  Token.AngleEnd -> ">"
  Token.Alias -> "alias"
  Token.Arrow -> "->"
  Token.As -> "as"
  Token.BlockBegin -> "{"
  Token.BlockEnd -> "}"
  Token.Case -> "|"
  Token.Character c -> squotes $ pretty c
  Token.Colon -> ":"
  Token.Comma -> ","
  Token.Define -> "define"
  Token.Do -> "do"
  Token.Dot -> "."
  Token.Else -> "else"
  Token.Field -> "field"
  Token.For -> "for"
  Token.GroupBegin -> "("
  Token.GroupEnd -> ")"
  Token.If -> "if"
  Token.Ignore -> "_"
  Token.Instance -> "instance"
  Token.Integer num -> pretty num
  Token.Intrinsic -> "intrinsic"
  Token.Match -> "match"
  Token.Module -> "module"
  Token.Operator name -> printUnqualified name
  Token.Permission -> "permission"
  Token.Reference -> "\\"
  Token.Record -> "record"
  Token.Text t -> dquotes $ pretty t
  Token.Trait -> "trait"
  Token.Type -> "type"
  Token.VectorBegin -> "["
  Token.VectorEnd -> "]"
  Token.Where -> "where"
  Token.With -> "with"
  Token.UpperWord name -> printUnqualified name
  Token.LowerWord name -> printUnqualified name

-- Minor hack because Parsec requires 'Show'.
instance Show Token.Token where
  show x = "`" <> show (printToken x) <> "`"

printInstantiated :: Instantiated -> Doc a
printInstantiated (Instantiated n []) = printQualified n
printInstantiated (Instantiated n ts) =
  printQualified n <> "." <> list (printType <$> ts)

printDataConstructor :: DataConstructor.DataConstructor -> Doc a
printDataConstructor (DataConstructor.DataConstructor fields name _) =
  "case"
    <+> printUnqualified name
    <> printedFields
  where
    printedFields = if not $ null fields then space <> tupled (printSignature <$> fields) else ""

printDeclaration :: Trait -> Doc a
printDeclaration (Trait name _ signature) =
  "trait" <+> printUnqualified (unqualifiedName name) <+> printSignature signature

printTypeDefinition :: TypeDefinition -> Doc a
printTypeDefinition (TypeDefinition constructors name _ parameters) =
  "type" <+> typeName <> encloseSep " { " " } " " | " (printDataConstructor <$> constructors)
  where
    typeName =
      printQualified name
        <> if not $ null parameters then (list . fmap printParameter) parameters else ""

printTerm :: Term a -> Doc b
printTerm t = fromMaybe "" (maybePrintTerms (decompose t))
  where
    decompose = Term.decompose . Term.stripMetadata

maybePrintTerms :: [Term a] -> Maybe (Doc b)
maybePrintTerms = \case
  [] -> Nothing
  (Group a : Match BooleanMatch _ cases _ _ : xs) -> Just (printIf (Just a) cases `justVertical` xs)
  (Group a : Match AnyMatch _ cases (DefaultElse _ _) _ : xs) -> Just (printMatch (Just a) cases Nothing `justVertical` xs)
  (Group a : Match AnyMatch _ cases (Else elseBody _) _ : xs) -> Just (printMatch (Just a) cases (Just elseBody) `justVertical` xs)
  (Push _ (Quotation (Word _ name args _)) _ : Group a : xs) -> Just ((backslash <> printWord name args) `justHoriz` (Group a : xs))
  (Push _ (Quotation body) _ : Group a : xs) -> Just (printDo a body `justVertical` xs)
  ( Coercion
      ( AnyCoercion
          ( Quantified
              [ Parameter _ "R" Stack Nothing,
                Parameter _ "S" Stack Nothing
                ]
              ( Function
                  [ StackFunction
                      (Variable "R" _)
                      []
                      (Variable "S" _)
                      []
                      grantNames
                      _
                    ]
                  [StackFunction (Variable "R" _) [] (Variable "S" _) [] revokeNames _]
                  []
                  _
                )
              _
            )
        )
      _
      _
      : Word _ (QualifiedName (Qualified _ "call")) _ _
      : xs
    ) ->
      Just (("with" <> space <> tupled (((\g -> "+" <> printGeneralName g) <$> grantNames) ++ ((\r -> "-" <> printGeneralName r) <$> revokeNames))) `justHoriz` xs)
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
            (\e -> [group $ blockMaybe "| _ " (maybePrintTerm e)])
            else_
      )
    )

printCase :: GeneralName -> Term a -> Doc b
printCase n (Lambda _ name _ body _) =
  group $
    blockMaybe
      ("|" <+> printGeneralName n <+> printToken Token.Arrow <+> punctuateComma (printUnqualified <$> names))
      (maybePrintTerm newBody)
  where
    (names, newBody) = go [name] body
    go ns (Lambda _ ln _ b _) = go (ln : ns) b
    go ns b = (ns, b)
printCase n b = group $ blockMaybe ("case" <+> printGeneralName n) (maybePrintTerm b)

printWord :: GeneralName -> [Type] -> Doc a
printWord word [] = printGeneralName word
printWord word args = printGeneralName word <> "." <> list (printType <$> args)

printValue :: Value a -> Doc b
printValue value = case value of
  Capture names term ->
    "$"
      <> tupled (printClosed <$> names)
      <> braces (printTerm term)
  Character c -> squotes (pretty c)
  Closed (ClosureIndex index) -> "closure" <> dot <> pretty index
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

printDefinition :: (Show a) => Definition a -> Maybe (Doc b)
printDefinition (Definition Category.Constructor _ _ _ _ _ _ _) = Nothing
printDefinition (Definition Category.Deconstructor _ _ _ _ _ _ _) = Nothing
printDefinition (Definition Category.Permission name body _ _ _ signature _) =
  Just $ group $ blockMaybe ("permission" <+> (printQualified name <+> printSignature signature)) (maybePrintTerm body)
printDefinition (Definition Category.Instance name body _ _ _ signature _) =
  Just $ group $ blockMaybe ("instance" <+> (printQualified name <+> printSignature signature)) (maybePrintTerm body)
printDefinition (Definition Category.Word name body _ _ _ _ _)
  | name == mainName = maybePrintTerm body
printDefinition (Definition Category.Word name body _ _ _ signature _) =
  Just $ group $ blockMaybe ("define" <+> (printQualified name <+> printSignature signature)) (maybePrintTerm body)

printWordEntry (Entry.WordEntry category _ origin mParent mSignature _) =
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
                Parent.Record a -> "record" <+> printQualified a
            ]
        Nothing -> "with no parent"
    ]

printMetadataEntry (Entry.MetadataEntry origin term) =
  vsep
    [ "metadata",
      hsep ["defined at", printOrigin origin],
      hsep ["with contents", printTerm term]
    ]

printTypeEntry (Entry.TypeEntry origin parameters ctors) =
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

printFragment :: (Show a, Ord a) => Fragment a -> Doc b
printFragment fragment =
  vcat
    ( ( (\g -> printGrouped g <> line)
          <$> groupedDeclarations
      )
        ++ ( (\t -> printTypeDefinition t <> line)
               <$> sort (view Fragment.types fragment)
           )
        ++ mapMaybe
          (fmap (<> line) . printDefinition)
          ( sort (view Fragment.definitions fragment)
          )
        ++ ( (\m -> printMetadata m <> line)
               <$> sort (view Fragment.metadata fragment)
           )
    )
  where
    groupedDeclarations =
      groupBy
        ( \a b ->
            (qualifierName . view Trait.name) a
              == (qualifierName . view Trait.name) b
        )
        (view Fragment.traits fragment)
    printGrouped decls =
      if noVocab
        then vsep ((\d -> printDeclaration d <> line) <$> sort decls)
        else blockMulti ("module" <+> printQualifier commonName) printDeclaration (sort decls)
      where
        (commonName, noVocab) = case qualifierName $ view Trait.name $ decls Unsafe.!! 0 of
          (Qualifier Absolute parts) -> (Qualifier Relative parts, null parts)
          n -> (n, False)

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
blockMulti before f = mapNonEmpty (blockMaybe before Nothing) (\xs -> vsep [before <+> lbrace, indent 2 (vsep (f <$> xs)), rbrace])

mapNonEmpty :: b -> ([a] -> b) -> [a] -> b
mapNonEmpty emptyCase nonEmptyFn l = fromMaybe emptyCase (viaNonEmpty (nonEmptyFn . toList) l)
