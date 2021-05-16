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
import Mlatu.Declaration (Declaration (..))
import Mlatu.Declaration qualified as Declaration
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
import Mlatu.Term (Case (..), CoercionHint (..), Else (..), MatchHint (..), Term (..), Value (..))
import Mlatu.Term qualified as Term
import Mlatu.Token qualified as Token
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
  printQualifier qualifier <> dot <> printUnqualified unqualifiedName

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
printSignature (Application a b _) = printSignature b <+> printSignature a
printSignature (Bottom _) = "<bottom>"
printSignature (Function as bs es _) =
  punctuateComma
    ( ( \sig -> case sig of
          Function {} -> parens (printSignature sig)
          StackFunction {} -> parens (printSignature sig)
          Quantified {} -> parens (printSignature sig)
          _ -> printSignature sig
      )
        <$> as
    )
    <+> "->"
    <+> punctuateComma
      ( ( \sig -> case sig of
            Function {} -> parens (printSignature sig)
            StackFunction {} -> parens (printSignature sig)
            Quantified {} -> parens (printSignature sig)
            _ -> printSignature sig
        )
          <$> bs
      )
    <> case es of
      [] -> ""
      _ -> space <> angles (hsep (punctuate " +" (printGeneralName <$> es)))
printSignature (Quantified names typ _) =
  "for" <+> hsep (printParameter <$> names) <> dot <+> printSignature typ
printSignature (Variable name _) = printGeneralName name
printSignature (StackFunction r as s bs es _) =
  punctuateComma
    ( ( \sig -> case sig of
          Function {} -> parens (printSignature sig)
          StackFunction {} -> parens (printSignature sig)
          Quantified {} -> parens (printSignature sig)
          _ -> printSignature sig
      )
        <$> (r : as)
    )
    <+> "->"
    <+> punctuateComma
      ( ( \sig -> case sig of
            Function {} -> parens (printSignature sig)
            StackFunction {} -> parens (printSignature sig)
            Quantified {} -> parens (printSignature sig)
            _ -> printSignature sig
        )
          <$> (s : bs)
      )
    <> case es of
      [] -> ""
      _ -> space <> encloseSep "<" ">" " + " (printGeneralName <$> es)
printSignature (Type t) = printType t

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
  Token.Colon -> colon
  Token.Comma -> semi
  Token.Define -> "define"
  Token.Do -> "do"
  Token.Dot -> dot
  Token.Else -> "else"
  Token.Field -> "field"
  Token.For -> "for"
  Token.GroupBegin -> lparen
  Token.GroupEnd -> rparen
  Token.If -> "if"
  Token.Ignore -> "_"
  Token.Instance -> "instance"
  Token.Integer num -> pretty num
  Token.Intrinsic -> "intrinsic"
  Token.Match -> "match"
  Token.Module -> "module"
  Token.Operator name -> printUnqualified name
  Token.Permission -> "permission"
  Token.Reference -> backslash
  Token.Record -> "record"
  Token.Text t -> dquotes $ pretty t
  Token.Trait -> "trait"
  Token.Type -> "type"
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

printDataConstructor :: DataConstructor.DataConstructor -> Doc a
printDataConstructor (DataConstructor.DataConstructor fields name _) =
  printUnqualified name <> printedFields
  where
    printedFields = if not $ null fields then space <> tupled (printSignature <$> fields) else ""

printDeclaration :: Declaration -> Doc a
printDeclaration (Declaration Declaration.Intrinsic name _ signature) =
  "intrinsic" <+> printUnqualified (unqualifiedName name) <+> parens (printSignature signature)
printDeclaration (Declaration Declaration.Trait name _ signature) =
  "trait" <+> printUnqualified (unqualifiedName name) <+> parens (printSignature signature)

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
  (Match AnyMatch _ cases (DefaultElse _ _) _ : xs) -> Just (sep [printMatch Nothing cases Nothing] `justVertical` xs)
  (Match AnyMatch _ cases (Else elseBody _) _ : xs) -> Just (sep [printMatch Nothing cases (Just elseBody)] `justVertical` xs)
  (Push _ value _ : xs) -> Just (printValue value `justHoriz` xs)
  (Word _ (QualifiedName (Qualified _ "drop")) [] o : xs) ->
    Just (printLambda "_" (Term.compose () o (Term.stripMetadata <$> xs)))
  (Word _ (UnqualifiedName "zero") [] _ : terms) ->
    Just (pretty num `justHoriz` terms')
    where
      go :: [Term a] -> (Int, [Term a])
      go (Word _ (UnqualifiedName "succ") [] _ : xs) = case go xs of (s, xs') -> (s + 1, xs')
      go xs = (0, xs)

      (num, terms') = go terms
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
            (Nothing, Just b) -> sep [a, b]
            (Just b, Just c) -> sep [a <+> b, c]

    justVertical :: Doc b -> [Term a] -> Doc b
    justVertical a l = case maybePrintTerms l of
      Nothing -> a
      Just b -> sep [a, b]

maybePrintTerm :: Term a -> Maybe (Doc b)
maybePrintTerm = maybePrintTerms . Term.decompose

printDo :: Term a -> Term a -> Doc b
printDo a body = blockMaybe (maybe "do" ("do" <+>) (printGroup a)) (maybePrintTerm body)

printGroup :: Term a -> Maybe (Doc b)
printGroup a = parens <$> maybePrintTerm a

printLambda :: Unqualified -> Term a -> Doc b
printLambda name body =
  let lambda = printToken Token.Arrow <+> punctuateComma (printUnqualified <$> names) <> semi
   in case maybePrintTerm newBody of
        Nothing -> lambda
        Just a -> sep [lambda, a]
  where
    (names, newBody) = go [name] body
    go ns (Lambda _ n _ b _) = go (n : ns) b
    go ns b = (ns, b)

printIf :: Maybe (Term a) -> [Case a] -> Doc b
printIf cond [Case _ trueBody _, Case _ falseBody _] =
  sep
    [ blockMaybe (maybe "if" ("if" <+>) (cond >>= printGroup)) (maybePrintTerm trueBody),
      blockMaybe "else" (maybePrintTerm falseBody)
    ]
printIf _ _ = ice "Mlatu.Pretty.printIf - Expected a true and false case"

printMatch :: Maybe (Term a) -> [Case a] -> Maybe (Term a) -> Doc b
printMatch cond cases else_ =
  sep
    ( maybe "match" ("match" <+>) (cond >>= printGroup) :
      ( ( (\(Case n b _) -> printCase n b)
            <$> cases
        )
          ++ maybe
            []
            (\e -> [blockMaybe "| _ " (maybePrintTerm e)])
            else_
      )
    )

printCase :: GeneralName -> Term a -> Doc b
printCase n (Lambda _ name _ body _) =
  blockMaybe
    (pipe <+> printGeneralName n <+> printToken Token.Arrow <+> punctuateComma (printUnqualified <$> names))
    (maybePrintTerm newBody)
  where
    (names, newBody) = go [name] body
    go ns (Lambda _ ln _ b _) = go (ln : ns) b
    go ns b = (ns, b)
printCase n b = blockMaybe ("|" <+> printGeneralName n) (maybePrintTerm b)

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
  Quotation body -> case maybePrintTerm body of
    Nothing -> brackets ""
    Just a -> brackets a
  Text text -> (if Text.count "\n" text > 1 then multiline else singleline) text
    where
      multiline :: Text -> Doc a
      multiline t = sep ([multilineQuote] ++ (pretty <$> lines t) ++ [multilineQuote])
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
  sep $
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
  Just $ blockMaybe ("permission" <+> (printQualified name <+> parens (printSignature signature))) (maybePrintTerm body)
printDefinition (Definition Category.Instance name body _ _ _ signature _) =
  Just $ blockMaybe ("instance" <+> (printQualified name <+> parens (printSignature signature))) (maybePrintTerm body)
printDefinition (Definition Category.Word name body _ _ _ _ _)
  | name == mainName = maybePrintTerm body
printDefinition (Definition Category.Word name body _ _ _ signature _) =
  Just $ blockMaybe ("define" <+> (printQualified name <+> parens (printSignature signature))) (maybePrintTerm body)

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
            (qualifierName . view Declaration.name) a
              == (qualifierName . view Declaration.name) b
        )
        (view Fragment.declarations fragment)
    printGrouped decls =
      if noVocab
        then sep ((\d -> printDeclaration d <> line) <$> sort decls)
        else blockLines ("module" <+> printQualifier commonName) (printDeclaration <$> sort decls)
      where
        (commonName, noVocab) = case qualifierName $ view Declaration.name $ decls Unsafe.!! 0 of
          (Qualifier Absolute parts) -> (Qualifier Relative parts, null parts)
          n -> (n, False)

blockLines :: Doc a -> [Doc a] -> Doc a
blockLines a [] = a <+> brackets ""
blockLines a bs = sep [nest 2 (sep (a <+> "{" : bs)), "}"]

blockMaybe :: Doc a -> Maybe (Doc a) -> Doc a
blockMaybe a Nothing = a <+> brackets ""
blockMaybe a (Just b) = blockLines a [b]
