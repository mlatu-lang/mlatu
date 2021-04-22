module Test.Zonk
  ( spec,
  )
where

import Data.Map qualified as Map
import Mlatu.Kind (Kind (..))
import Mlatu.Origin qualified as Origin
import Mlatu.Type (Type (..), TypeId (..), Var (..))
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Zonk qualified as Zonk
import Optics
import Relude hiding (Type)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "does nothing to free type variables" $ do
    Zonk.typ TypeEnv.empty va `shouldBe` va
  it "substitutes one level" $ do
    Zonk.typ (set TypeEnv.tvs (Map.singleton ia vb) TypeEnv.empty) va `shouldBe` vb
  it "substitutes multiple levels" $ do
    Zonk.typ (set TypeEnv.tvs (Map.fromList [(ia, vb), (ib, int)]) TypeEnv.empty) va `shouldBe` int
  where
    o = Origin.point "" 0 0
    ia = TypeId 0
    va = TypeVar o $ Var "A" ia kv
    ib = TypeId 1
    vb = TypeVar o $ Var "B" ib kv
    kv = Star
    int = TypeConstructor o "Int"
