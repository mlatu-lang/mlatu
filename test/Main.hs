module Main
  ( main,
  )
where

import Relude
import Test.Hspec (Spec, describe, hspec)
import Test.Infer qualified
import Test.InstanceCheck qualified
import Test.Interpret qualified
import Test.Origin qualified
import Test.Parse qualified
import Test.Resolve qualified
import Test.Tokenize qualified
import Test.Zonk qualified

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tokenization" Test.Tokenize.spec
  describe "source locations" Test.Origin.spec
  describe "parsing" Test.Parse.spec
  describe "name resolution" Test.Resolve.spec
  describe "instance checking" Test.InstanceCheck.spec
  describe "type inference" Test.Infer.spec
  describe "zonking" Test.Zonk.spec
  describe "interpretation" Test.Interpret.spec
