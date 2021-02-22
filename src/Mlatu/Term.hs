-- |
-- Module      : Mlatu.Term
-- Description : The core language
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Term
  ( Case (..),
    CoercionHint (..),
    Else (..),
    MatchHint (..),
    Permit (..),
    Term (..),
    Value (..),
    asCoercion,
    compose,
    decompose,
    identityCoercion,
    origin,
    permissionCoercion,
    quantifierCount,
    stripMetadata,
    stripValue,
    typ,
  )
where

import Data.Char (isLetter)
import Data.List (partition)
import Data.Text (count)
import Data.Text qualified as Text
import Mlatu.Entry.Parameter (Parameter (..))
import Mlatu.Kind qualified as Kind
import Mlatu.Literal (FloatLiteral, IntegerLiteral)
import Mlatu.Name
  ( Closed,
    ClosureIndex (..),
    ConstructorIndex (..),
    GeneralName (..),
    LocalIndex (..),
    Qualified (..),
    Unqualified (..),
    unqualifiedName,
  )
import Mlatu.Operator (Fixity (Postfix))
import Mlatu.Origin (Origin)
import Mlatu.Pretty qualified as Pretty
import Mlatu.Signature (Signature)
import Mlatu.Signature qualified as Signature
import Mlatu.Type (Type, TypeId)
import Relude hiding (Compose, Type)
import Text.PrettyPrint (($$), (<+>))
import Text.PrettyPrint.HughesPJ qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

-- | This is the core language. It permits pushing values to the stack, invoking
-- definitions, and moving values between the stack and local variables.
--
-- It also permits empty programs and program concatenation. Together these form
-- a monoid over programs. The denotation of the concatenation of two programs
-- is the composition of the denotations of those two programs. In other words,
-- there is a homomorphism from the syntactic monoid onto the semantic monoid.
--
-- A value of type @'Term' a@ is a term annotated with a value of type @a@. A
-- parsed term may have a type like @'Term' ()@, while a type-inferred term may
-- have a type like @'Term' 'Type'@.
data Term a
  = -- | @id@, @as (T)@, @with (+A -B)@: coerces the stack to a particular type.
    Coercion !CoercionHint !a !Origin
  | -- | @e1 e2@: composes two terms.
    Compose !a !(Term a) !(Term a)
  | -- | @Λx. e@: generic terms that can be specialized.
    Generic !Unqualified !TypeId !(Term a) !Origin
  | -- | @(e)@: precedence grouping for infix operators.
    Group !(Term a)
  | -- | @→ x; e@: local variable introductions.
    Lambda !a !Unqualified !a !(Term a) !Origin
  | -- | @match { case C {...}... else {...} }@, @if {...} else {...}@:
    -- pattern-matching.
    Match !MatchHint !a ![Case a] !(Else a) !Origin
  | -- | @new.n@: ADT allocation.
    New !a !ConstructorIndex !Int !Origin
  | -- | @new.closure.n@: closure allocation.
    NewClosure !a !Int !Origin
  | -- | @new.vec.n@: vector allocation.
    NewVector !a !Int !a !Origin
  | -- | @push v@: push of a value.
    Push !a !(Value a) !Origin
  | -- | @f@: an invocation of a word.
    Word !a !Fixity !GeneralName ![Type] !Origin
  deriving (Ord, Eq, Show)

-- | The type of coercion to perform.
data CoercionHint
  = -- | The identity coercion, generated by empty terms.
    IdentityCoercion
  | -- | A coercion to a particular type.
    AnyCoercion !Signature
  deriving (Ord, Eq, Show)

-- | The original source of a @match@ expression
data MatchHint
  = -- | @match@ generated from @if@.
    BooleanMatch
  | -- | @match@ explicitly in the source.
    AnyMatch
  deriving (Ord, Eq, Show)

-- | A case branch in a @match@ expression.
data Case a = Case !GeneralName !(Term a) !Origin
  deriving (Ord, Eq, Show)

-- | An @else@ branch in a @match@ (or @if@) expression.
data Else a = Else !(Term a) !Origin
  deriving (Ord, Eq, Show)

-- | A permission to grant or revoke in a @with@ expression.
data Permit = Permit
  { permitted :: !Bool,
    permitName :: !GeneralName
  }
  deriving (Ord, Eq, Show)

-- | A value, used to represent literals in a parsed program, as well as runtime
-- values in the interpreter.
data Value a
  = -- | A quotation with explicit variable capture; see "Mlatu.Scope".
    Capture ![Closed] !(Term a)
  | -- | A character literal.
    Character !Char
  | -- | A captured variable.
    Closed !ClosureIndex
  | -- | A floating-point literal.
    Float !FloatLiteral
  | -- | An integer literal.
    Integer !IntegerLiteral
  | -- | A local variable.
    Local !LocalIndex
  | -- | A reference to a name.
    Name !Qualified
  | -- | A parsed quotation.
    Quotation !(Term a)
  | -- | A text literal.
    Text !Text
  deriving (Ord, Eq, Show)

-- FIXME: 'compose' should work on 'Term ()'.
compose :: a -> Origin -> [Term a] -> Term a
compose x o = foldr (Compose x) (identityCoercion x o)

asCoercion :: a -> Origin -> [Signature] -> Term a
asCoercion x o ts = Coercion (AnyCoercion signature) x o
  where
    signature = Signature.Quantified [] [] (Signature.Function ts ts [] o) o

identityCoercion :: a -> Origin -> Term a
identityCoercion = Coercion IdentityCoercion

permissionCoercion :: [Permit] -> a -> Origin -> Term a
permissionCoercion permits x o = Coercion (AnyCoercion signature) x o
  where
    signature =
      Signature.Quantified
        [ Parameter o "R" Kind.Stack,
          Parameter o "S" Kind.Stack
        ]
        []
        ( Signature.Function
            [ Signature.StackFunction
                (Signature.Variable "R" o)
                []
                (Signature.Variable "S" o)
                []
                (map permitName grants)
                o
            ]
            [ Signature.StackFunction
                (Signature.Variable "R" o)
                []
                (Signature.Variable "S" o)
                []
                (map permitName revokes)
                o
            ]
            []
            o
        )
        o
    (grants, revokes) = partition permitted permits

decompose :: Term a -> [Term a]
-- TODO: Verify that this is correct.
decompose (Generic _name _id t _origin) = decompose t
decompose (Compose _ a b) = decompose a ++ decompose b
decompose (Coercion IdentityCoercion _ _) = []
decompose term = [term]

origin :: Term a -> Origin
origin term = case term of
  Coercion _ _ o -> o
  Compose _ a _ -> origin a
  Generic _ _ _ o -> o
  Group a -> origin a
  Lambda _ _ _ _ o -> o
  New _ _ _ o -> o
  NewClosure _ _ o -> o
  NewVector _ _ _ o -> o
  Match _ _ _ _ o -> o
  Push _ _ o -> o
  Word _ _ _ _ o -> o

quantifierCount :: Term a -> Int
quantifierCount = countFrom 0
  where
    countFrom !c (Generic _ _ body _) = countFrom (c + 1) body
    countFrom c _ = c

-- Deduces the explicit type of a term.

typ :: Term Type -> Type
typ = metadata

metadata :: Term a -> a
metadata term = case term of
  Coercion _ t _ -> t
  Compose t _ _ -> t
  Generic _ _ term' _ -> metadata term'
  Group term' -> metadata term'
  Lambda t _ _ _ _ -> t
  Match _ t _ _ _ -> t
  New t _ _ _ -> t
  NewClosure t _ _ -> t
  NewVector t _ _ _ -> t
  Push t _ _ -> t
  Word t _ _ _ _ -> t

stripMetadata :: Term a -> Term ()
stripMetadata term = case term of
  Coercion a _ b -> Coercion a () b
  Compose _ a b -> Compose () (stripMetadata a) (stripMetadata b)
  Generic a b term' c -> Generic a b (stripMetadata term') c
  Group term' -> stripMetadata term'
  Lambda _ a _ b c -> Lambda () a () (stripMetadata b) c
  Match a _ b c d -> Match a () (map stripCase b) (stripElse c) d
  New _ a b c -> New () a b c
  NewClosure _ a b -> NewClosure () a b
  NewVector _ a _ b -> NewVector () a () b
  Push _ a b -> Push () (stripValue a) b
  Word _ a b c d -> Word () a b c d
  where
    stripCase :: Case a -> Case ()
    stripCase case_ = case case_ of
      Case a b c -> Case a (stripMetadata b) c

    stripElse :: Else a -> Else ()
    stripElse else_ = case else_ of
      Else a b -> Else (stripMetadata a) b

stripValue :: Value a -> Value ()
stripValue v = case v of
  Capture a b -> Capture a (stripMetadata b)
  Character a -> Character a
  Closed a -> Closed a
  Float a -> Float a
  Integer a -> Integer a
  Local a -> Local a
  Name a -> Name a
  Quotation a -> Quotation (stripMetadata a)
  Text a -> Text a

instance Pretty (Term a) where
  pPrint term = case decompose term of
    [] -> Pretty.empty
    (Word _ Postfix (QualifiedName (Qualified _ name)) [] o : ts)
      | name == "drop" -> printTerm (Lambda () "_" () body o)
      where
        body :: Term ()
        body = compose () o (map stripMetadata ts)
    (Push _ (Quotation body) _ : Group a : ts) ->
      ("do " <> Pretty.parens (pPrint a) <> ":") $$ Pretty.nest 2 (pPrint body) $$ printTerms ts
    (Group a : Match BooleanMatch _ [Case _ trueBody _] _ _ : ts) ->
      ("if " <> Pretty.parens (pPrint a) <> ":")
        $$ Pretty.nest 2 (pPrint trueBody)
        $$ printTerms ts
    (Group a : Match BooleanMatch _ [Case _ trueBody _, Case _ falseBody _] _ _ : ts) ->
      ("if " <> Pretty.parens (pPrint a) <> ":")
        $$ Pretty.nest 2 (pPrint trueBody)
        $$ printIfNotEmpty
          "else:"
          (Pretty.nest 2 $ pPrint falseBody)
          ($$)
        $$ printTerms ts
    (Group a : Match _ _ cases else_ _ : ts) ->
      ("match " <> Pretty.parens (pPrint a))
        $$ Pretty.vcat
          (map pPrint cases)
        $$ pPrint else_
        $$ printTerms ts
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
            <+> Pretty.parens
              ( Pretty.list (map (\g -> "+" <> pPrint g) grantNames ++ map (\r -> "-" <> pPrint r) revokeNames)
              )
            <+> printTerms ts
    (t : ts) -> printTerm t <+> printTerms ts
    where
      printTerms = Pretty.hsep . map pPrint

printTerm :: Term a -> Pretty.Doc
printTerm term = case term of
  Coercion {} -> Pretty.empty
  Generic name i body _ ->
    Pretty.braces (pPrint name <> "/*" <> pPrint i <> "*/")
      <+> pPrint body
  Group a -> Pretty.parens (pPrint a)
  Lambda _ name _ body _ ->
    ("-> "
      <> foldr (\e acc -> pPrint e <> ", " <> acc) (pPrint name) (reverse names)
      <> ";")
      $$ pPrint newBody
    where
      (names, newBody) = go [] body
      go ns (Lambda _ n _ b _) = go (ns ++ [n]) b
      go ns b = (ns, b)
  Match BooleanMatch _ [Case _ trueBody _, Case _ falseBody _] _ _ ->
    "if:" $$ Pretty.nest 2 (pPrint trueBody)
      $$ printIfNotEmpty "else:" (Pretty.nest 2 (pPrint falseBody)) ($$)
  Match _ _ cases else_ _ ->
    "match"
      $$ Pretty.vcat
        (map pPrint cases)
      $$ pPrint else_
  New _ (ConstructorIndex index) _size _ -> "new." <> Pretty.int index
  NewClosure _ size _ -> "new.closure." <> pPrint size
  NewVector _ size _ _ -> "new.vec." <> pPrint size
  Push _ value _ -> pPrint value
  Word _ Postfix (UnqualifiedName (Unqualified name)) [] _
    | not (Text.all isLetter name) -> Pretty.parens $ Pretty.text $ toString name
  Word _ _ name [] _ -> pPrint name
  Word _ _ name args _ ->
    pPrint name <> "::[" <> Pretty.hcat (intersperse ", " (map pPrint args)) <> "]"
  n -> pPrint n

printIfNotEmpty :: Pretty.Doc -> Pretty.Doc -> (Pretty.Doc -> Pretty.Doc -> Pretty.Doc) -> Pretty.Doc
printIfNotEmpty f s g = if not $ Pretty.isEmpty s then g f s else Pretty.empty

instance Pretty (Case a) where
  pPrint (Case name body _) =
    ("case " <> pPrint name <> ":")
      $$ Pretty.nest 2 (pPrint body)

instance Pretty (Else a) where
  pPrint (Else (Word _ _ name _ _) _)
    | name == "abort" = Pretty.empty
  pPrint (Else body _) = "else:" $$ Pretty.nest 2 (pPrint body)

instance Pretty Permit where
  pPrint (Permit allow name) =
    (if allow then "+" else "-") <> pPrint name

instance Pretty (Value a) where
  pPrint value = case value of
    Capture names term ->
      Pretty.char '$'
        <> Pretty.parens (Pretty.list $ map pPrint names)
        <> Pretty.braces (pPrint term)
    Character c -> Pretty.quotes $ Pretty.char c
    Closed (ClosureIndex index) -> "closure." <> Pretty.int index
    Float f -> pPrint f
    Integer i -> pPrint i
    Local (LocalIndex index) -> "local." <> Pretty.int index
    Name n -> "\\" <> pPrint n
    Quotation w@Word {} -> "\\" <> pPrint w
    Quotation body -> "{ " <> pPrint body <> " }"
    Text text -> (if count "\n" text > 1 then multiline else singleline) text
      where
        multiline :: Text -> Pretty.Doc
        multiline t = Pretty.text "\"\"\"" $$ Pretty.vcat (map (Pretty.text . toString) (lines t)) $$ Pretty.text "\"\"\""

        singleline :: Text -> Pretty.Doc
        singleline t =
          Pretty.doubleQuotes $
            Pretty.text $
              concatMap
                ( \case
                    '\n' -> "\\n"
                    c -> [c]
                )
                $ toString t
