{-# LANGUAGE TemplateHaskell #-}

module Main where 

import Test.QuickCheck
import Mlatu.Back.Print qualified as Erlang
import Mlatu (Prelude (..), compileWithPrelude, runMlatu)
import System.Directory (removeFile)

boolToMl :: Bool -> Text 
boolToMl True = "true" 
boolToMl False = "false"

compilationTest ::  Text -> (Text -> Property) -> IO Property 
compilationTest source fun 
  = do 
        writeFileText "test.mlt" source
        (result, _) <- runMlatu (compileWithPrelude Common [] Nothing ["test.mlt"])
        removeFile "test.mlt"
        case result of 
            Nothing -> discard
            Just program -> do 
                contents <- Erlang.generate program Nothing
                pure (fun contents)

singleConstant :: Text -> Text -> Property 
singleConstant source value = idempotentIOProperty $ compilationTest source (=== "-module(mlatu).\n -export([main/1]).\n main(_) -> mmain({[], []}).\n\nmmain({Rest0, Closure0}) -> { ["<> value <>" | Rest0] , Closure0 }.\n\n")

instance Arbitrary Natural where 
    arbitrary = arbitrarySizedNatural
    shrink 0 = []
    shrink x = [0, x-1]

prop_succ :: Natural -> Property 
prop_succ a  = singleConstant (show a <> " succ" ) $ show (a + 1)

prop_plus :: Natural -> Natural -> Property 
prop_plus a b  = singleConstant (show a <> " " <> show b <> " +") $ show (a + b)

prop_mul :: Natural -> Natural -> Property 
prop_mul a b  = singleConstant (show a <> " " <> show b <> " *") $ show (a * b)

prop_div :: Natural -> Natural -> Property 
prop_div a 0 = singleConstant (show a <> " 0 /") "0"
prop_div a b = singleConstant (show a <> " " <> show b <> " /") $ show (a `div` b)

prop_not :: Bool -> Property 
prop_not b = singleConstant (boolToMl b <> " not") $ boolToMl (not b)

prop_and :: Bool -> Bool -> Property 
prop_and b1 b2 = singleConstant (boolToMl b1 <> " " <> boolToMl b2 <> " and") $ boolToMl (b1 && b2)

prop_or :: Bool -> Bool -> Property 
prop_or b1 b2 = singleConstant (boolToMl b1 <> " " <> boolToMl b2 <> " or") $ boolToMl (b1 || b2)

prop_xor :: Bool -> Bool -> Property 
prop_xor b1 b2 = singleConstant (boolToMl b1 <> " " <> boolToMl b2 <> " xor") $ boolToMl (b1 /= b2)

prop_implies :: Bool -> Bool -> Property 
prop_implies b1 b2 = singleConstant (boolToMl b1 <> " " <> boolToMl b2 <> " implies") $ boolToMl ((not b1) || b2)

return []

main :: IO Bool
main = do 
    result <- $quickCheckAll 
    if result then exitSuccess else exitFailure