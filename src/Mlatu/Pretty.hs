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
    printDataDefinition,
    printInstantiated,
    printKind,
    printFragment,
    printQualifier,
  )
where

import Data.List (findIndex, groupBy)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Mlatu.CodataDefinition (CodataDefinition (..))
import Mlatu.DataDefinition (DataDefinition (..))
import Mlatu.Definition (Definition (Definition), mainName)
import Mlatu.Entry.Category qualified as Category
import Mlatu.Entry.Parameter (Parameter (..))
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
import Mlatu.Term (Case (..), CoercionHint (..), Else (..), Term (..), Value (..))
import Mlatu.Term qualified as Term
import Mlatu.Token qualified as Token
import Mlatu.Trait (Trait (..))
import Mlatu.Trait qualified as Trait
import Mlatu.Type (Constructor (..), Type (..), TypeId (..), Var (..))
import Mlatu.Type qualified as Type
import Optics
import Prettyprinter
import Relude hiding (Compose, Constraint, Type, group)
import Relude.Unsafe qualified as Unsafe
import Text.Show qualified

punctuateComma :: [Doc a] -> [Doc a]
punctuateComma = punctuate comma

printParameter :: Parameter -> Doc a
printParameter (Parameter _ name Value _) = printUnqualified name
printParameter (Parameter _ name Stack _) = printUnqualified name
printParameter (Parameter _ name Label _) = "+" <> printUnqualified name
printParameter (Parameter _ name Permission _) = "+" <> printUnqualified name
printParameter (Parameter _ name (_ :-> _) _) = printUnqualified name <> "[_]"

printQualified :: Qualified -> Doc a
printQualified (Qualified (Qualifier Absolute []) unqualifiedName) = printUnqualified unqualifiedName
printQualified (Qualified qualifier unqualifiedName) =
  printQualifier qualifier <> dot <> printUnqualified unqualifiedName

printQualifier :: Qualifier -> Doc a
printQualifier (Qualifier _ parts) = pretty $ Text.intercalate "." parts

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
        recur a <+> comma <+> recur b
      TypeConstructor _ "prod" :@ a ->
        parens $ recur a <> comma <> space
      TypeConstructor _ "prod" ->
        parens comma
      Type.Sum _ a b ->
        recur a <+> pipe <+> recur b
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
                  <+> sep (recur . TypeVar (Type.origin t) <$> vars)
                  <+> dot
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
printSignature (Application a b _) = printSimpleSignature b <+> printSimpleSignature a
printSignature (Bottom _) = "<bottom>"
printSignature (Function as bs es _) =
  (group . hsep . punctuateComma) (printSimpleSignature <$> as)
    <+> "->"
    <+> (group . hsep . punctuateComma) (printSimpleSignature <$> bs)
    <> case es of
      [] -> emptyDoc
      _ -> space <> group (encloseSep langle rangle " + " (printGeneralName <$> es))
printSignature (Quantified names typ _) =
  "for" <+> hsep (printParameter <$> names) <> dot <+> group (printSignature typ)
printSignature (Variable name _) = printGeneralName name
printSignature (StackFunction r as s bs es _) =
  (group . hsep . punctuateComma) (printSimpleSignature <$> (r : as))
    <+> "->"
    <+> (group . hsep . punctuateComma) (printSimpleSignature <$> (s : bs))
    <> case es of
      [] -> emptyDoc
      _ -> space <> group (encloseSep langle rangle " + " (printGeneralName <$> es))
printSignature (Type t) = printType t

printSimpleSignature :: Signature -> Doc a
printSimpleSignature t@Function {} = parens (printSignature t)
printSimpleSignature t@StackFunction {} = parens (printSignature t)
printSimpleSignature t@Variable {} = printSignature t
printSimpleSignature t@Type {} = printSignature t
printSimpleSignature t@Bottom {} = printSignature t
printSimpleSignature t@Application {} = printSignature t
printSimpleSignature t@Quantified {} = parens (printSignature t)

printToken :: Token.Token -> Doc a
printToken = \case
  Token.About -> "about"
  Token.AngleBegin -> langle
  Token.AngleEnd -> rangle
  Token.Alias -> "alias"
  Token.Arrow -> "->"
  Token.As -> "as"
  Token.BlockBegin -> lbrace
  Token.BlockEnd -> rbrace
  Token.Case -> pipe
  Token.Character c -> squotes $ pretty c
  Token.Codata -> "codata"
  Token.Colon -> colon
  Token.Comma -> comma
  Token.Data -> "data"
  Token.Define -> "define"
  Token.Dot -> dot
  Token.Field -> "field"
  Token.For -> "for"
  Token.GroupBegin -> lparen
  Token.GroupEnd -> rparen
  Token.Ignore -> "_"
  Token.Instance -> "instance"
  Token.Integer num -> pretty num
  Token.Match -> "match"
  Token.Module -> "module"
  Token.Operator name -> printUnqualified name
  Token.Permission -> "permission"
  Token.Reference -> backslash
  Token.Text t -> dquotes $ pretty t
  Token.Trait -> "trait"
  Token.VectorBegin -> lbracket
  Token.VectorEnd -> rbracket
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
  printQualified n <> dot <> list (printType <$> ts)

printDeclaration :: Trait -> Doc a
printDeclaration (Trait name _ signature) =
  "trait" <+> printUnqualified (unqualifiedName name) <+> parens (printSignature signature)

printDataDefinition :: DataDefinition -> Doc a
printDataDefinition (DataDefinition constructors name _ parameters) =
  "data" <+> typeName <> encloseSep " { " " } " " | " (printDataConstructor <$> constructors)
  where
    typeName = case parameters of
      [] -> printQualified name
      _ -> (parens . hsep) (printParameter <$> reverse parameters) <+> printQualified name
    printDataConstructor (name, fields, _) =
      printUnqualified name
        <> if not $ null fields then space <> tupled (printSignature <$> fields) else emptyDoc

printCodataDefinition :: CodataDefinition -> Doc a
printCodataDefinition (CodataDefinition constructors name _ parameters) =
  "codata" <+> typeName <> encloseSep " { " " } " " + " (printDataDeconstructor <$> constructors)
  where
    typeName = case parameters of
      [] -> printQualified name
      _ -> (parens . hsep) (printParameter <$> reverse parameters) <+> printQualified name
    printDataDeconstructor (name, sig, _) = printUnqualified name <+> parens (printSignature sig)

printTerm :: Term a -> Doc b
printTerm t = fromMaybe emptyDoc (maybePrintTerms (decompose t))
  where
    decompose = Term.decompose . Term.stripMetadata

maybePrintTerms :: [Term a] -> Maybe (Doc b)
maybePrintTerms = \case
  [] -> Nothing
  (Group a : Match _ cases (DefaultElse _ _) _ : xs) -> printMatch (Just a) cases Nothing `maySep` maybePrintTerms xs
  (Group a : Match _ cases (Else elseBody _) _ : xs) -> printMatch (Just a) cases (Just elseBody) `maySep` maybePrintTerms xs
  (Push _ (Quotation (Word _ name args _)) _ : Group a : xs) -> (backslash <> printWord name args) `maySep` maybePrintTerms (Group a : xs)
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
      : Push _ (Text "call") _
      : Word _ (UnqualifiedName "extern") _ _
      : xs
    ) ->
      ( "with" <> space
          <> tupled
            ( ( (\g -> "+" <> printGeneralName g)
                  <$> grantNames
              )
                ++ ((\r -> "-" <> printGeneralName r) <$> revokeNames)
            )
      )
        `maySep` maybePrintTerms xs
  (Coercion (AnyCoercion _) _ _ : xs) -> maybePrintTerms xs
  (Group (Group a) : xs) -> printGroup a `maybeSep` maybePrintTerms xs
  (Group a : xs) -> printGroup a `maybeSep` maybePrintTerms xs
  (Lambda _ name _ body _ : xs) -> printLambda name body `maySep` maybePrintTerms xs
  (Match _ cases (DefaultElse _ _) _ : xs) ->
    printMatch Nothing cases Nothing `maySep` maybePrintTerms xs
  (Match _ cases (Else elseBody _) _ : xs) ->
    printMatch Nothing cases (Just elseBody) `maySep` maybePrintTerms xs
  (Push _ value _ : xs) -> printValue value `maySep` maybePrintTerms xs
  (Word _ (QualifiedName (Qualified _ "drop")) [] o : xs) ->
    Just (printLambda "_" (Term.compose () o (Term.stripMetadata <$> xs)))
  (Word _ (UnqualifiedName "zero") _ _ : xs) ->
    let go :: Int -> [Term a] -> (Int, [Term a])
        go n ((Word _ (UnqualifiedName "succ") _ _) : xs') = go (n + 1) xs'
        go n rest = (n, rest)
     in let (s, rest) = go 0 xs in pretty s `maySep` maybePrintTerms rest
  (Word _ name args _ : xs) -> printWord name args `maySep` maybePrintTerms xs
  (t : _) -> ice $ "Mlatu.Pretty.maybePrintTerms - Formatting failed: " <> show (Term.stripMetadata t)

maySep :: Doc a -> Maybe (Doc a) -> Maybe (Doc a)
maySep a (Just b) = Just (sep [a, b])
maySep a Nothing = Just a

maybeSep :: Maybe (Doc a) -> Maybe (Doc a) -> Maybe (Doc a)
maybeSep (Just a) (Just b) = Just (sep [a, b])
maybeSep (Just a) Nothing = Just a
maybeSep Nothing (Just b) = Just b
maybeSep Nothing Nothing = Nothing

maybePrintTerm :: Term a -> Maybe (Doc b)
maybePrintTerm = maybePrintTerms . Term.decompose

printGroup :: Term a -> Maybe (Doc b)
printGroup a = parens <$> maybePrintTerm a

printLambda :: Unqualified -> Term a -> Doc b
printLambda name body =
  let lambda = "->" <+> (hsep . punctuateComma) (printUnqualified <$> names) <> semi
   in case maybePrintTerm newBody of
        Nothing -> lambda
        Just a -> sep [lambda, a]
  where
    (names, newBody) = go [name] body
    go ns (Lambda _ n _ b _) = go (n : ns) b
    go ns b = (ns, b)

printMatch :: Maybe (Term a) -> [Case a] -> Maybe (Term a) -> Doc b
printMatch cond cases else_ =
  sep
    ( maybe "match" ("match" <+>) (cond >>= printGroup) :
      ( ((\(Case n b _) -> printCase n b) <$> cases)
          <> maybe [] (\e -> [blockMaybe "| _ " (maybePrintTerm e)]) else_
      )
    )

printCase :: GeneralName -> Term a -> Doc b
printCase n (Lambda _ name _ body _) =
  blockMaybe
    (pipe <+> printGeneralName n <+> "->" <+> (hsep . punctuateComma) (printUnqualified <$> names))
    (maybePrintTerm newBody)
  where
    (names, newBody) = go [name] body
    go ns (Lambda _ ln _ b _) = go (ln : ns) b
    go ns b = (ns, b)
printCase n b = blockMaybe (pipe <+> printGeneralName n) (maybePrintTerm b)

printWord :: GeneralName -> [Type] -> Doc a
printWord word [] = printGeneralName word
printWord word args = printGeneralName word <> dot <> list (printType <$> args)

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
  Quotation body -> blockedMaybe (maybePrintTerm body)
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
  Just $
    blockMaybe
      ( "permission"
          <+> (printQualified name <+> parens (printSignature signature))
      )
      (maybePrintTerm body)
printDefinition (Definition Category.Instance name body _ _ _ signature _) =
  Just $
    blockMaybe
      ( "instance"
          <+> ( printQualified name
                  <+> parens (printSignature signature)
              )
      )
      (maybePrintTerm body)
printDefinition (Definition Category.Word name body _ _ _ signature _)
  | name == mainName = maybePrintTerm body
  | otherwise =
    Just $
      blockMaybe
        ( "define"
            <+> ( printQualified name
                    <+> parens (printSignature signature)
                )
        )
        (maybePrintTerm body)

printFragment :: (Show a, Ord a) => Fragment a -> Doc b
printFragment fragment =
  vcat
    ( ( (\g -> printGrouped g <> line)
          <$> groupedDeclarations
      )
        <> ( (\t -> printDataDefinition t <> line)
               <$> sort (view Fragment.dataDefinitions fragment)
           )
        <> ( (\t -> printCodataDefinition t <> line)
               <$> sort (view Fragment.codataDefinitions fragment)
           )
        <> mapMaybe
          (fmap (<> line) . printDefinition)
          (sort (view Fragment.definitions fragment))
        <> ( (\m -> printMetadata m <> line)
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
        else blockLines ("module" <+> printQualifier commonName) printDeclaration (sort decls)
      where
        (commonName, noVocab) = case qualifierName $ view Trait.name $ decls Unsafe.!! 0 of
          (Qualifier Absolute parts) -> (Qualifier Relative parts, null parts)
          n -> (n, False)

block :: Maybe (Doc a) -> Doc a -> Doc a
block Nothing inner = sep [lbrace, inner, rbrace]
block (Just before) inner =
  flatAlt
    (vsep [before <+> lbrace, indent 2 inner, rbrace])
    (sep [before <+> lbrace, inner, rbrace])

blockLines :: Doc a -> (b -> Doc a) -> [b] -> Doc a
blockLines before _ [] = before <+> lbrace <> rbrace
blockLines before f xs =
  flatAlt
    (vsep [before <+> lbrace, indent 2 (vsep (f <$> xs)), rbrace])
    (sep [before <+> lbrace, sep (f <$> xs), rbrace])

blockMaybe :: Doc a -> Maybe (Doc a) -> Doc a
blockMaybe before Nothing = before <+> lbrace <> rbrace
blockMaybe before (Just inner) = block (Just before) inner

blockedMaybe :: Maybe (Doc a) -> Doc a
blockedMaybe Nothing = lbrace <> rbrace
blockedMaybe (Just inner) = block Nothing inner
