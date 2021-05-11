module Test.Infer
  ( spec,
  )
where

import Mlatu (Prelude (..), compilePrelude, fragmentFromSource)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Enter qualified as Enter
import Mlatu.Entry qualified as Entry
import Mlatu.Informer (errorCheckpoint)
import Mlatu.InstanceCheck (instanceCheck)
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Kind (Kind (..))
import Mlatu.Monad (runMlatuExceptT)
import Mlatu.Name (GeneralName (..), Qualified (..))
import Mlatu.Origin qualified as Origin
import Mlatu.Pretty (printEntry, printInstantiated, printType)
import Mlatu.Report (human)
import Mlatu.Term qualified as Term
import Mlatu.Type (Type (..), TypeId (..), Var (..))
import Mlatu.Type qualified as Type
import Mlatu.Vocabulary qualified as Vocabulary
import Prettyprinter (Pretty (pretty), hsep, list, tupled, vcat)
import Relude hiding (Type)
import Test.Common (Sign (..))
import Test.HUnit (assertBool, assertFailure)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  describe "with trivial programs" $ do
    it "typechecks empty program" $ do
      testTypecheck
        Positive
        "define test (->) {}"
        $ Type.Fun o r r e

    it "typechecks single literals" $ do
      testTypecheck
        Positive
        "define test (-> int) { 0 }"
        $ Type.Fun o r (Type.Prod o r int) e

      testTypecheck
        Positive
        "define test (-> double) { 1.0 }"
        $ Type.Fun o r (Type.Prod o r float) e

    it "typechecks compound literals" $ do
      testTypecheck
        Positive
        "define test (-> (int int pair) list) { [1 1 mk-pair, 2 2 mk-pair, 3 3 mk-pair] }"
        $ Type.Fun o r (Type.Prod o r (ctor "list" :@ (ctor "pair" :@ int :@ int))) e

    it "typechecks intrinsics" $ do
      testTypecheck
        Positive
        "vocab Mlatu {\
        \ intrinsic magic (for r.. s.. . r.. -> s..)\
        \}\
        \define test (for r.. s.. +p . r.. -> s.. +p) { Mlatu.magic }"
        $ Type.Fun o r s e

      testTypecheck
        Positive
        "define test (-> int) { 1 2 Mlatu.add-int64 }"
        $ Type.Fun o r (Type.Prod o r int) e

    it "typechecks data types and records" $ do
      testTypecheck
        Positive
        "record unit { } \n\
        \define test (-> unit) { mk-unit }"
        $ Type.Fun o r (Type.Prod o r (ctor "unit")) e

      testTypecheck
        Positive
        "type unit { case mk-unit () }\n\
        \define test (-> unit) { mk-unit }"
        $ Type.Fun o r (Type.Prod o r (ctor "unit")) e

    it "typechecks definitions" $ do
      testTypecheck
        Positive
        "define one (-> int) { 1 }\n\
        \define test (-> int) { one }"
        $ Type.Fun o r (Type.Prod o r int) e

      testTypecheck
        Positive
        "define one (-> int) { 1 }\n\
        \define two (-> int) { 2 }\n\
        \define test (-> int) { one two Mlatu.add_int64 }"
        $ Type.Fun o r (Type.Prod o r int) e

      testTypecheck
        Positive
        "define up (int -> int) { 1 Mlatu.add_int64 }\n\
        \define down (int -> int) { -1 Mlatu.add_int64 }\n\
        \define test (-> int) { 1 up 2 down Mlatu.add_int64 }"
        $ Type.Fun o r (Type.Prod o r int) e

    it "typechecks operators" $ do
      testTypecheck
        Positive
        "define test (-> int) { 1 1 + }"
        $ Type.Fun o r (Type.Prod o r int) e

    it "typechecks nested scopes" $ do
      testTypecheck
        Positive
        "intrinsic add (int, int -> int)\n\
        \define test (-> int, int) {\n\
        \  1000 -> x1;\n\
        \  100 -> y1;\n\
        \  10\n\
        \  {\n\
        \    -> a1;\n\
        \    a1 x1 add\n\
        \    {\n\
        \      -> b1;\n\
        \      b1 y1 add\n\
        \    } call\n\
        \  } call\n\
        \  \n\
        \  1000 -> x2;\n\
        \  100 -> y2;\n\
        \  10\n\
        \  {\n\
        \    -> a2;\n\
        \    a2 y2 add\n\
        \    {\n\
        \      -> b2;\n\
        \      b2 x2 add\n\
        \    } call\n\
        \  } call\n\
        \}"
        $ Type.Fun o r (Type.Prod o (Type.Prod o r int) int) e

    it "typechecks closures with multiple types" $ do
      testTypecheck
        Positive
        "define test (-> (-> int, double)) {\n\
        \  0 0.0 -> x, y;\n\
        \  { x y }\n\
        \}"
        $ Type.Fun
          o
          r
          ( Type.Prod
              o
              r
              (Type.Fun o r (Type.Prod o (Type.Prod o r int) float) e)
          )
          e

  describe "with instance checking" $ do
    it "rejects invalid signature" $ do
      testTypecheck
        Negative
        "record Pair[A, B] { field first (a) field second (b) }\n\
        \define flip[A, B] (b a pair -> a b pair) {\n\
        \  match case mk-pair -> x, y { y x mk-pair }\n\
        \}\n\
        \define test (-> double char pair) { 1 '1' pair flip }"
        $ Type.Fun o r (Type.Prod o r (ctor "pair" :@ char :@ int)) e

    it "accepts valid permissions" $ do
      testTypecheck
        Positive
        "define test (-> +Fail) { abort }"
        $ Type.Fun o r r (Type.Join o fail_ e)

      testTypecheck
        Positive
        "intrinsic launch_missiles (-> +io)\n\
        \define test (-> +fail +io) { launch_missiles abort }"
        $ Type.Fun o r r (Type.Join o fail_ (Type.Join o io e))

      testTypecheck
        Positive
        "intrinsic launch_missiles (-> +io)\n\
        \define test (-> +io +fail) { launch_missiles abort }"
        $ Type.Fun o r r (Type.Join o fail_ (Type.Join o io e))

    it "accepts redundant permissions" $ do
      testTypecheck
        Positive
        "define test (-> +fail) {}"
        $ Type.Fun o r r (Type.Join o fail_ e)

      testTypecheck
        Positive
        "define test (-> +fail +io) {}"
        $ Type.Fun o r r (Type.Join o fail_ (Type.Join o io e))

      testTypecheck
        Positive
        "define test (-> +io +fail) {}"
        $ Type.Fun o r r (Type.Join o fail_ (Type.Join o io e))

    it "rejects missing permissions" $ do
      testTypecheck
        Negative
        "define test (->) { abort }"
        $ Type.Fun o r r e

      testTypecheck
        Negative
        "intrinsic launch_missiles (-> +io)\n\
        \define test (->) { launch_missiles abort }"
        $ Type.Fun o r r e

  describe "with higher-order functions" $ do
    it "typechecks curried functions" $ do
      testTypecheck
        Positive
        "define curried_add (int -> int -> int) {\n\
        \  -> x; { -> y; x y Mlatu.add_int64 }\n\
        \}\n\
        \define test (-> int) { 1 2 curried_add call }"
        $ Type.Fun o r (Type.Prod o r int) e

    it "typechecks permissions of higher-order functions" $ do
      testTypecheck
        Positive
        "intrinsic launch_missiles (-> +io)\n\
        \intrinsic map (for a b +p. a list, (a -> b +p) -> b list +p)\n\
        \define test (-> int list +io) { [1, 2, 3] \\launch_missiles map }"
        $ Type.Fun o r (Type.Prod o r (ctor "List" :@ int)) (Type.Join o io e)

  describe "with coercions" $ do
    it "typechecks identity coercions" $ do
      testTypecheck
        Positive
        "define test (-> int) { 1 as (int) }"
        $ Type.Fun o r (Type.Prod o r int) e
  where
    o = Origin.point "" 0 0
    r = TypeVar o $ Var "r" (TypeId 0) Stack
    s = TypeVar o $ Var "s" (TypeId 1) Stack
    e = TypeVar o $ Var "p" (TypeId 2) Permission
    ctor =
      TypeConstructor o . Type.Constructor
        . Qualified Vocabulary.global
    char = ctor "char"
    int = ctor "int"
    io = ctor "io"
    fail_ = ctor "fail"
    float = ctor "double"

testTypecheck :: Sign -> Text -> Type -> IO ()
testTypecheck sign input expected = do
  mDictionary <- runMlatuExceptT $ compilePrelude Common ioPermission Nothing
  case mDictionary of
    Left reports ->
      error $ show $ vcat $ pretty ("unable to set up inference tests:" :: Text) : (human <$> reports)
    Right dictionary -> do
      result <- runMlatuExceptT $ do
        fragment <- fragmentFromSource ioPermission Nothing 1 "<test>" input
        Enter.fragment fragment dictionary
      case Dictionary.toList <$> result of
        Right definitions -> case find matching definitions of
          Just (_, Entry.Word _ _ _ _ _ (Just term)) -> do
            let actual = Term.typ term
            check <- runMlatuExceptT $ do
              instanceCheck actual expected
              errorCheckpoint
            case sign of
              Positive ->
                assertBool
                  (show $ hsep [printType actual, "<:", printType expected])
                  $ isRight check
              Negative ->
                assertBool
                  (show $ hsep [printType actual, "</:", printType expected])
                  $ isLeft check
          _ ->
            assertFailure $
              show $
                hsep
                  ["missing main word definition:", list $ (\(i, e) -> tupled [printInstantiated i, printEntry e]) <$> definitions]
          where
            matching (Instantiated (Qualified v "test") _, _)
              | v == Vocabulary.global =
                True
            matching _ = False
        Left reports -> case sign of
          Positive ->
            assertFailure $
              toString $
                unlines $
                  show . human <$> reports
          -- FIXME: This might accept a negative test for the wrong
          -- reason.
          Negative -> pass
  where
    ioPermission = [QualifiedName $ Qualified Vocabulary.global "io"]
