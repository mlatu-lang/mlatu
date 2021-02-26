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
module Mlatu.Pretty where

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
import Mlatu.Kind qualified as Kind
import Mlatu.Literal (FloatLiteral (..), IntegerLiteral (..), floatValue)
import Mlatu.Metadata (Metadata (..))
import Mlatu.Metadata qualified as Metadata
import Mlatu.Name (Closed (..), ClosureIndex (..), ConstructorIndex (..), GeneralName (..), LocalIndex (..), Qualified (..), Qualifier (..), Root (..), Unqualified (..))
import Mlatu.Operator qualified as Operator
import Mlatu.Origin qualified as Origin
import Mlatu.Signature (Constraint (..), Signature (..))
import Mlatu.Signature qualified as Signature
import Mlatu.Synonym (Synonym (Synonym))
import Mlatu.Term (Case (..), CoercionHint (..), Else (..), MatchHint (..), Permit (..), Term (..), Value (..))
import Mlatu.Term qualified as Term
import Mlatu.Token qualified as Token
import Mlatu.Type (Constructor (..), Type (..), TypeId (..), Var (..))
import Mlatu.Type qualified as Type
import Mlatu.TypeDefinition (TypeDefinition (..))
import Mlatu.Vocabulary qualified as Vocabulary
import Numeric (showIntAtBase)
import Prettyprinter
import Relude hiding (Constraint, Type)
import Relude.Unsafe qualified as Unsafe
import Text.Show qualified

commaPunctuated :: [Doc a] -> Doc a
commaPunctuated [] = emptyDoc
commaPunctuated [a] = a
commaPunctuated as = hsep $ punctuate "," as

printIntegerLiteral :: IntegerLiteral -> Doc a
printIntegerLiteral literal =
  hsep
    [ if value < 0 then "-" else "",
      case integerBase literal of
        Base.Binary -> "0b"
        Base.Octal -> "0o"
        Base.Decimal -> ""
        Base.Hexadecimal -> "0x",
      pretty $ showIntAtBase base (\i -> Unsafe.fromJust (digits !!? i)) (abs value) ""
    ]
  where
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

printOperator :: Operator.Operator -> Doc a
printOperator operator =
  hsep $
    ("infix" :) $
      ( case Operator.associativity operator of
          Operator.Nonassociative -> id
          Operator.Leftward -> ("left" :)
          Operator.Rightward -> ("right" :)
      )
        [ printPrecedence $ Operator.precedence operator,
          printQualified $ Operator.name operator
        ]

printPrecedence :: Operator.Precedence -> Doc a
printPrecedence (Operator.Precedence i) = pretty i

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
  hsep $
    [ pretty $ Origin.name origin,
      ":",
      pretty al,
      ".",
      pretty ac,
      "-"
    ]
      ++ (if al == bl then [pretty bc] else [pretty bl, ".", pretty bc])
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
          hsep [recur a, "->", recur b, recur p]
      TypeConstructor _ "Fun" :@ a :@ b ->
        parens $
          hsep [recur a, "->", recur b]
      TypeConstructor _ "Fun" :@ a ->
        parens $
          hsep [recur a, "->"]
      TypeConstructor _ "Fun" -> parens "->"
      TypeConstructor _ "Prod" :@ a :@ b ->
        hsep [recur a, ", ", recur b]
      TypeConstructor _ "Prod" :@ a ->
        parens $ hsep [recur a, ", "]
      TypeConstructor _ "Prod" ->
        parens ","
      TypeConstructor _ "Sum" :@ a :@ b ->
        hsep [recur a, " | ", recur b]
      TypeConstructor _ "Join" :@ a :@ b ->
        hsep ["+", recur a, " ", recur b]
      TypeConstructor _ "Join" :@ a ->
        parens $ hsep ["+", recur a]
      a :@ b -> hsep [recur a, brackets $ recur b]
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
      TypeConstant o var -> hsep ["∃", recur $ TypeVar o var]
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
prettyKinded name k = hsep $ case k of
  Permission -> ["+", printUnqualified name]
  Stack -> [printUnqualified name, "..."]
  _otherKind -> [printUnqualified name]

printConstraint :: Constraint -> Doc a
printConstraint (Constraint name params) = printUnqualified name <> list (map printParameter params)

printSignature :: Signature -> Doc a
printSignature (Application firstA b _) =
  hsep
    [printSignature finalA, list (map printSignature (as ++ [b]))]
  where
    (finalA, as) = go [] firstA
    go l (Application x y _) = go (l ++ [y]) x
    go l x = (x, l)
printSignature (Bottom _) = "<bottom>"
printSignature (Function as bs es _) =
  parens $
    hsep $
      [ commaPunctuated $ map printSignature as,
        "->",
        commaPunctuated $ map printSignature bs
      ]
        ++ map (("+" <>) . printGeneralName) es
printSignature (Quantified names constraints typ _) =
  list (map printParameter names)
    <+> printSignature typ
    <> if not $ null constraints then " where" <+> hsep (map printConstraint constraints) else emptyDoc
printSignature (Variable name _) = printGeneralName name
printSignature (StackFunction r as s bs es _) =
  parens $
    (printSignature r <> "...")
      <+> hsep (map printSignature as)
      <+> "->"
      <+> printSignature s
      <> "..."
      <+> hsep (map printSignature bs ++ map (("+" <>) . printGeneralName) es)
printSignature (Type t) = printType t

printToken :: Token.Token l -> Doc a
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
  Token.Elif -> "elif"
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
instance Show (Token.Token l) where
  show = show . printToken

printInstantiated :: Instantiated -> Doc a
printInstantiated (Instantiated n ts) =
  printQualified n <> "::" <> list (map printType ts) <> ""

printDataConstructor :: DataConstructor.DataConstructor -> Doc a
printDataConstructor (DataConstructor.DataConstructor fields name _) =
  "case"
    <+> printUnqualified name
    <+> tupled (map printSignature fields)

printDeclaration :: Declaration -> Doc a
printDeclaration (Declaration category name _ signature) = printedCategory <+> printUnqualified (unqualifiedName name) <+> printSignature signature
  where
    printedCategory = case category of
      Declaration.Trait -> "trait"
      Declaration.Intrinsic -> "intrinsic"

printTypeDefinition :: TypeDefinition -> Doc a
printTypeDefinition (TypeDefinition constructors name _ parameters) =
  if null printedConstructors
    then "type" <+> typeName <+> "{}"
    else nest 2 $ vsep ["type" <+> typeName <> ":", vsep $ map printDataConstructor constructors]
  where
    typeName =
      printQualified name
        <> brackets printedParameters
    printedParameters = commaPunctuated (map printParameter (fst parameters))
    printedConstructors = map printDataConstructor constructors

printTerm :: Term a -> Doc b
printTerm term =
  case Term.decompose term of
    [] -> emptyDoc
    (Word _ Operator.Postfix (QualifiedName (Qualified _ name)) [] o : ts)
      | name == "drop" -> printLambda "_" body
      where
        body :: Term ()
        body = Term.compose () o (map Term.stripMetadata ts)
    (Push _ (Quotation body) _ : Group a : ts) ->
      vsep [printDo a body, printTerms ts]
    (Group a : NewVector _ 1 _ _ : ts) ->
      braces (printTerm a) <> printTerms ts
    (Group a : Match BooleanMatch _ cases _ _ : ts) ->
      vsep [printIf (Just a) cases, printTerms ts]
    (Group a : Match _ _ cases (Else elseBody _) _ : ts) ->
      vsep [printMatch (Just a) cases (Just elseBody), printTerms ts]
    ( Coercion
        ( AnyCoercion
            ( Signature.Quantified
                [Parameter _ r1 Kind.Stack, Parameter _ s1 Kind.Stack]
                []
                ( Signature.Function
                    [ Signature.StackFunction
                        (Signature.Variable r2 _)
                        []
                        (Signature.Variable s2 _)
                        []
                        grantNames
                        _
                      ]
                    [ Signature.StackFunction
                        (Signature.Variable r3 _)
                        []
                        (Signature.Variable s3 _)
                        []
                        revokeNames
                        _
                      ]
                    []
                    _
                  )
                _
              )
          )
        _
        _
        : Word _ _ (QualifiedName name) _ _
        : ts
      )
        | r1 == "R" && r2 == "R" && r3 == "R" && s1 == "S" && s2 == "S" && s3 == "S" && unqualifiedName name == "call" ->
          "with"
            <+> parens
              ( commaPunctuated (map (\g -> "+" <> printGeneralName g) grantNames ++ map (\r -> "-" <> printGeneralName r) revokeNames)
              )
            <> printTerms ts
    (Coercion {} : ts) -> printTerms ts
    (Group a : ts) -> printGroup a <> printTerms ts
    (Lambda _ name _ body _ : ts) -> printLambda name body <> printTerms ts
    (Match BooleanMatch _ cases _ _ : ts) -> vsep [printIf Nothing cases, printTerms ts]
    (Match AnyMatch _ cases (Else elseBody _) _ : ts) -> vsep [printMatch Nothing cases (Just elseBody), printTerms ts]
    (Push _ value _ : ts) -> printValue value <> printTerms ts
    (Word _ fixity name args _ : ts) -> printWord fixity name args <> printTerms ts
    (New _ (ConstructorIndex i) _ _ : ts) -> printNew i <> printTerms ts
    (NewClosure _ i _ : ts) -> printClosure i <> printTerms ts
    (NewVector _ i _ _ : ts) -> printVector i <> printTerms ts
    _ -> error "Formatting failed"
  where
    printTerms [] = emptyDoc
    printTerms [t] = space <> printTerm t
    printTerms (t : ts) = space <> printTerm t <> printTerms ts

printDo :: Term a -> Term a -> Doc b
printDo a body = nest 2 $ vsep ["do" <+> printGroup a <> ":", printTerm body]

printGroup :: Term a -> Doc b
printGroup a = parens (printTerm a)

printLambda :: Unqualified -> Term a -> Doc b
printLambda name body =
  ( "-> "
      <> foldr (\e acc -> (printUnqualified e <> ",") <+> acc) (printUnqualified name) (reverse names)
      <> ";"
  )
    <> line
    <> printTerm newBody
  where
    (names, newBody) = go [] body
    go ns (Lambda _ n _ b _) = go (ns ++ [n]) b
    go ns b = (ns, b)

printIf :: Maybe (Term a) -> [Case a] -> Doc b
printIf cond [Case _ trueBody _] =
  nest
    2
    ( vsep
        [ "if" <> maybe emptyDoc (\c -> space <> printGroup c) cond <> ":",
          printTerm trueBody
        ]
    )
printIf cond [Case _ trueBody _, Case _ falseBody _] =
  vsep
    [ nest 2 $
        vsep
          [ "if" <> maybe emptyDoc (\c -> space <> printGroup c) cond <> ":",
            printTerm trueBody
          ],
      nest 2 $
        vsep
          [ "else:",
            printTerm falseBody
          ]
    ]
printIf _ _ = error "Expected one or two cases"

printMatch :: Maybe (Term a) -> [Case a] -> Maybe (Term a) -> Doc b
printMatch cond cases (Just (Word _ _ name _ _)) | name == abortName = printMatch cond cases Nothing
  where
    abortName = QualifiedName (Qualified Vocabulary.global "abort")
printMatch cond cases else_ =
  vsep
    [ "match"
        <> maybe
          emptyDoc
          (\c -> space <> printGroup c)
          cond
        <> ":",
      nest
        2
        ( vsep
            ( map
                (\(Case n b _) -> nest 2 $ vsep ["case" <+> printGeneralName n <> ":", printTerm b])
                cases
            )
        ),
      maybe emptyDoc (\b -> nest 2 $ vsep ["else:", printTerm b]) else_
    ]

printWord :: Operator.Fixity -> GeneralName -> [Type] -> Doc a
printWord Operator.Postfix (UnqualifiedName (Unqualified name)) []
  | not (Text.all isLetter name) = parens $ pretty name
printWord _ word [] = printGeneralName word
printWord _ word args = printGeneralName word <> "::" <> list (map printType args)

printNew :: Int -> Doc a
printNew i = "new." <> pretty i

printClosure :: Int -> Doc a
printClosure i = "new.closure." <> pretty i

printVector :: Int -> Doc a
printVector 0 = "[]"
printVector i = "new.vec." <> pretty i

printPermit :: Permit -> Doc a
printPermit (Permit allow name) =
  (if allow then "+" else "-") <> printGeneralName name

printValue :: Value a -> Doc b
printValue value = case value of
  Capture names term ->
    "$"
      <> tupled (map printClosed names)
      <> braces (printTerm term)
  Character c -> squotes (pretty c)
  Closed (ClosureIndex index) -> "closure." <> pretty index
  Float f -> printFloatLiteral f
  Integer i -> printIntegerLiteral i
  Local (LocalIndex index) -> "local." <> pretty index
  Name n -> "\\" <> printQualified n
  Quotation w@Word {} -> "\\" <> printTerm w
  Quotation body -> "{" <+> printTerm body <+> "}"
  Text text -> (if Text.count "\n" text > 1 then multiline else singleline) text
    where
      multiline :: Text -> Doc a
      multiline t = vsep ("\"\"\"" : map pretty (lines t) ++ ["\"\"\""])

      singleline :: Text -> Doc a
      singleline t =
        dquotes $
          pretty $
            concatMap
              ( \case
                  '\n' -> "\\n"
                  c -> [c]
              )
              $ toString t

printMetadata :: Metadata -> Doc a
printMetadata metadata =
  vsep
    [ "about" <+> printGeneralName (Metadata.name metadata) <> ":",
      nest
        2
        ( vsep $
            map field $
              HashMap.toList $
                fields metadata
        )
    ]
  where
    field (key, value) = nest 2 $ vsep [printUnqualified key <> ":", printTerm value]

printDefinition :: Definition a -> Doc b
printDefinition (Definition Category.Constructor _ _ _ _ _ _ _ _) = emptyDoc
printDefinition (Definition Category.Permission name body _ _ _ _ signature _) =
  nest 2 $
    vsep
      [ "permission:" <+> (printQualified name <> printSignature signature <> ":"),
        printTerm body
      ]
printDefinition (Definition Category.Instance name body _ _ _ _ signature _) =
  nest 2 $
    vsep
      [ "instance" <+> (printQualified name <+> printSignature signature <> ":"),
        printTerm body
      ]
printDefinition (Definition Category.Word name body _ _ _ _ _ _)
  | name == mainName = printTerm body
printDefinition (Definition Category.Word name body _ _ _ _ signature _) =
  nest 2 $
    vsep
      [ "define" <+> (printQualified name <> printSignature signature <> ":"),
        printTerm body
      ]

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
      nest 2 $
        vsep
          [ "and data constructors",
            vsep $
              map constructor ctors
          ]
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

printFragment :: (Ord a) => Fragment a -> Doc b
printFragment fragment =
  maybeVsep
    [ mapMaybe printGrouped groupedDeclarations,
      map printSynonym $ sort (synonyms fragment),
      map printTypeDefinition $ sort (Fragment.types fragment),
      map printDefinition $ sort (definitions fragment),
      map printMetadata $ sort (metadata fragment)
    ]
  where
    groupedDeclarations = groupBy (\a b -> (qualifierName . Declaration.name) a == (qualifierName . Declaration.name) b) (declarations fragment)
    printGrouped decls =
      if noVocab
        then ifThereVsep (map printDeclaration $ sort decls)
        else ifThereVsep (map printDeclaration $ sort decls) <&> (\x -> nest 2 $ vsep ["vocab" <+> printQualifier commonName <> ":", x])
      where
        (commonName, noVocab) = case qualifierName $ Declaration.name $ decls Unsafe.!! 0 of
          (Qualifier Absolute parts) -> (Qualifier Relative parts, null parts)
          n -> (n, False)

maybeVsep :: [[Doc a]] -> Doc a
maybeVsep l =
  maybe
    emptyDoc
    vsep
    ( guarded
        (not . null)
        (mapMaybe (fmap vsep . guarded (not . null)) l)
    )

ifThereVsep :: [Doc a] -> Maybe (Doc a)
ifThereVsep = fmap vsep . guarded (not . null)

ifThereHsep :: [Doc a] -> Maybe (Doc a)
ifThereHsep = fmap hsep . guarded (not . null)