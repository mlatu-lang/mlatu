module Test.Parse
  ( spec,
  )
where

import Mlatu (fragmentFromSource)
import Mlatu.Monad (runMlatuExceptT)
import Mlatu.Report qualified as Report
import Relude
import Test.Common
import Test.HUnit (Assertion, assertFailure)
import Test.Hspec (Spec, describe, it)
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

spec :: Spec
spec = do
  describe "with definitions" $ do
    it "accepts unqualified word as definition name" $ do
      testParse Positive "define word (->) {}"
    it "accepts qualified word as definition name" $ do
      testParse Positive "define vocabulary::word (->) {}"
    it "accepts unqualified word as definition name within vocab" $ do
      testParse Positive "vocab vocabulary { define word (->) {} }"
    it "accepts qualified word as definition name within vocab" $ do
      testParse Positive "vocab outer { define inner::word (->) {} }"

    it "accepts unqualified operator as definition name" $ do
      testParse Positive "define + (->) {}"
    it "accepts qualified operator as definition name" $ do
      testParse Positive "define vocabulary::+ (->) {}"
    it "accepts unqualified operator as definition name within vocab" $ do
      testParse Positive "vocab vocabulary { define + (->) {} }"
    it "accepts qualified operator as definition name within vocab" $ do
      testParse Positive "vocab outer { define inner::+ (->) {} }"

    it "accepts unqualified word as type name" $ do
      testParse Positive "type Word {}"
    it "accepts qualified word as type name" $ do
      testParse Positive "type vocabulary::Word {}"
    it "accepts unqualified word as type name within vocab" $ do
      testParse Positive "vocab vocabulary { type Word {} }"
    it "accepts qualified word as type name within vocab" $ do
      testParse Positive "vocab outer { type inner::Word {} }"

    it "rejects unqualified operator as type name" $ do
      testParse Negative "type + {}"
    it "rejects qualified operator as type name" $ do
      testParse Negative "type vocabulary::+ {}"

testParse :: Sign -> Text -> Assertion
testParse sign input = do
  result <-
    runMlatuExceptT $
      fragmentFromSource ioPermission Nothing 1 "<test>" input
  case result of
    Left reports -> case sign of
      Positive ->
        assertFailure $
          toString $
            unlines $
              map (toText . Pretty.render . Report.human) reports
      -- TODO: Test error messages for negative tests.
      Negative -> pass
    Right fragment -> case sign of
      Positive -> pass
      Negative -> assertFailure $ Pretty.render $ pPrint fragment
