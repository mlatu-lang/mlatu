module Test.InstanceCheck
  ( spec,
  )
where

import Mlatu.Informer (errorCheckpoint)
import Mlatu.InstanceCheck (instanceCheck)
import Mlatu.Kind (Kind (..))
import Mlatu.Monad (runMlatuExceptT)
import Mlatu.Name (Qualified (..))
import Mlatu.Origin qualified as Origin
import Mlatu.Pretty (printType)
import Mlatu.Type (Type (..), TypeId (..), Var (..))
import Mlatu.Type qualified as Type
import Mlatu.Vocabulary qualified as Vocabulary
import Prettyprinter (hsep)
import Relude hiding (Type)
import Test.Common (Sign (..))
import Test.HUnit (assertBool)
import Test.Hspec (Spec, it)

spec :: Spec
spec = do
  it "with concrete types" $ do
    --    Int
    -- <: Int
    testInstanceCheck Positive int int

    --    <T> (T)
    -- <: Int
    testInstanceCheck Positive (fx x) int

    --    [R..., T, +P] (R... -> R..., T +P)
    -- <: [R..., +P]    (R... -> R..., Int +P)
    testInstanceCheck
      Positive
      (fr $ fx $ Type.Fun o r (Type.Prod o r x))
      (fr $ Type.Fun o r (Type.Prod o r int))

  it "with parameterized types" $ do
    --    [A, B] (Pair[A, B])
    -- <: [A]    (Pair[A, A])
    testInstanceCheck
      Positive
      (fx $ fy $ pair :@ x :@ y)
      (fx $ pair :@ x :@ x)

    --     <A, B> (Pair<A, B> -> Pair<B, A>)
    -- </: <A, B> (Pair<A, B> -> Pair<A, B>)
    testInstanceCheck
      Negative
      ( fr $
          fx $
            fy $
              Type.Fun
                o
                (Type.Prod o r (pair :@ x :@ y))
                (Type.Prod o r (pair :@ y :@ x))
      )
      ( fr $
          fx $
            fy $
              Type.Fun
                o
                (Type.Prod o r (pair :@ x :@ y))
                (Type.Prod o r (pair :@ x :@ y))
      )
  where
    o = Origin.point "" 0 0
    r = TypeVar o rv
    x = TypeVar o xv
    y = TypeVar o yv
    rv = Var "R" (TypeId 0) Stack
    xv = Var "X" (TypeId 2) Star
    yv = Var "Y" (TypeId 3) Star
    fr = Type.Forall o rv
    fx = Type.Forall o xv
    fy = Type.Forall o yv
    ctor =
      TypeConstructor o . Type.Constructor
        . Qualified Vocabulary.global
    int = ctor "Int"
    pair = ctor "Pair"

testInstanceCheck :: Sign -> Type -> Type -> IO ()
testInstanceCheck sign a b = do
  result <- runMlatuExceptT $ do
    instanceCheck "polymorphic" a "concrete" b
    errorCheckpoint
  case sign of
    Positive ->
      assertBool (show $ hsep [printType a, "<:", printType b]) $
        isRight result
    Negative ->
      assertBool (show $ hsep [printType a, "</:", printType b]) $
        isLeft result
