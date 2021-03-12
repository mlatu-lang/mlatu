{-# LANGUAGE NegativeLiterals #-}

module Test.Interpret
  ( spec,
  )
where

import Data.ByteString qualified as ByteString
import Data.Knob qualified as Knob
import Mlatu (Prelude (..), compilePrelude, fragmentFromSource)
import Mlatu.Dictionary (Dictionary)
import Mlatu.Enter qualified as Enter
import Mlatu.Interpret (Rep (..), interpret, printRep)
import Mlatu.Monad (runMlatuExceptT)
import Mlatu.Name (ConstructorIndex (ConstructorIndex))
import Mlatu.Report (human)
import Prettyprinter (hsep, list, vcat, viaShow)
import Relude hiding (stderr, stdin, stdout)
import System.IO (hClose)
import Test.Common (ioPermission)
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec (Spec, describe, it, runIO)

spec :: Spec
spec = do
  testInterpretWithHandles <- runIO $ do
    mDictionary <- runMlatuExceptT $ compilePrelude Common ioPermission Nothing
    case mDictionary of
      Left reports ->
        error $
          show $
            vcat $
              "unable to set up interpreter tests:" : map human reports
      Right dictionary -> return $ testInterpretFull dictionary

  let testInterpret = testInterpretWithHandles "" Nothing Nothing

  describe "with trivial programs" $ do
    it "interprets literals" $ do
      testInterpret "0" [Int64 0]
      testInterpret "0.0" [Float64 0.0]
      testInterpret
        "1 2"
        [ Int64 2,
          Int64 1
        ]
      testInterpret
        "\"meow\""
        [ Text "meow"
        ]
    it "interprets 'hello world'" $ do
      testInterpret "\"meow\" println" []

  describe "with operators" $ do
    it "interprets Int64 operators" $ do
      testInterpret "2 + 3" [Int64 5]
      testInterpret "2 - 3" [Int64 -1]
      testInterpret "2 * 3" [Int64 6]
      testInterpret "2 / 3" [Int64 0]
      testInterpret "2 % 3" [Int64 2]
    it "interprets chains of Int64 operators" $ do
      testInterpret "2 + 3 + 4" [Int64 9]
      testInterpret "2 + 3 * 4" [Int64 14]
      testInterpret "2 * 3 + 4" [Int64 10]
      testInterpret "2 * 3 * 4" [Int64 24]
    it "wraps Int64" $ do
      testInterpret "9223372036854775807 + 1" [Int64 -9223372036854775808]
      testInterpret "-9223372036854775808 - 1" [Int64 9223372036854775807]
    it "interprets Float64 operators" $ do
      testInterpret "2.0 + 3.0" [Float64 5]
      testInterpret "2.0 - 3.0" [Float64 -1]
      testInterpret "2.0 * 3.0" [Float64 6]
      testInterpret "2.0 / 4.0" [Float64 0.5]
      testInterpret "2.0 % 3.0" [Float64 2]
    it "interprets Bool operators" $ do
      let false = Algebraic (ConstructorIndex 0) []
          true = Algebraic (ConstructorIndex 1) []
      testInterpret "false & false" [false]
      testInterpret "false & true" [false]
      testInterpret "true & false" [false]
      testInterpret "true & true" [true]
      testInterpret "false | false" [false]
      testInterpret "false | true" [true]
      testInterpret "true | false" [true]
      testInterpret "true | true" [true]
      testInterpret "false ~ false" [false]
      testInterpret "false ~ true" [true]
      testInterpret "true ~ false" [true]
      testInterpret "true ~ true" [false]
      testInterpret "false --> false" [true]
      testInterpret "false --> true" [true]
      testInterpret "true --> false" [false]
      testInterpret "true --> true" [true]

  describe "with scope" $ do
    it "looks up locals and closure values correctly" $ do
      testInterpretWithHandles
        ""
        (Just "1110\n1110\n")
        Nothing
        "1000 -> x1;\n\
        \100 -> y1;\n\
        \10\n\
        \{\n\
        \  -> a1;\n\
        \  (a1 + x1)\n\
        \  {\n\
        \    -> b1;\n\
        \    b1 + y1\n\
        \  } call\n\
        \} call\n\
        \println\n\
        \1000 -> x2;\n\
        \100 -> y2;\n\
        \10\n\
        \{\n\
        \  -> a2;\n\
        \  (a2 + y2)\n\
        \  {\n\
        \    -> b2;\n\
        \    b2 + x2\n\
        \  } call\n\
        \} call\n\
        \println\n\
        \\&"
        []

  describe "with common math words" $ do
    it "computes absolute values" $ do
      testInterpret "0 abs" [Int64 0]
      testInterpret "+0 abs" [Int64 0]
      testInterpret "-0 abs" [Int64 0]
      testInterpret "1 abs" [Int64 1]
      testInterpret "+1 abs" [Int64 1]
      testInterpret "1000 abs" [Int64 1000]
      testInterpret "+1000 abs" [Int64 1000]
      testInterpret "-1000 abs" [Int64 1000]
      testInterpret "9223372036854775807 abs" [Int64 9223372036854775807]
      testInterpret "-9223372036854775808 abs" [Int64 -9223372036854775808]

  describe "with functional combinators" $ do
    it "computes fixed points" $ do
      testInterpret
        "5 { -> n, rec;\
        \  if (n <= 0) { 1 }\
        \  else {(n - 1) rec call * n}\
        \} fix"
        [Int64 120]

testInterpretFull ::
  Dictionary ->
  ByteString ->
  Maybe ByteString ->
  Maybe ByteString ->
  Text ->
  [Rep] ->
  IO ()
testInterpretFull
  commonDictionary
  standardInput
  mExpectedStdout
  mExpectedStderr
  input
  expectedStack = do
    result <- runMlatuExceptT $ do
      fragment <- fragmentFromSource ioPermission Nothing 1 "<test>" input
      Enter.fragment fragment commonDictionary
    (_stdinKnob, stdin) <- do
      knob <- Knob.newKnob standardInput
      (,) knob <$> Knob.newFileHandle knob "stdin" ReadMode
    (stdoutKnob, stdout) <- do
      knob <- Knob.newKnob $ ByteString.pack []
      (,) knob <$> Knob.newFileHandle knob "stdout" WriteMode
    (stderrKnob, stderr) <- do
      knob <- Knob.newKnob $ ByteString.pack []
      (,) knob <$> Knob.newFileHandle knob "stderr" WriteMode
    case result of
      Right dictionary -> do
        actualStack <- interpret dictionary Nothing [] stdin stdout stderr []
        hClose stdin
        hClose stdout
        hClose stderr
        assertEqual
          ( show $
              hsep
                ["stack", list $ map printRep expectedStack, "=", list $ map printRep actualStack]
          )
          expectedStack
          actualStack
        case mExpectedStdout of
          Just expectedStdout -> do
            actualStdout <- Knob.getContents stdoutKnob
            assertEqual
              ( show $
                  hsep
                    [ "stdout",
                      viaShow expectedStdout,
                      "=",
                      viaShow actualStdout
                    ]
              )
              expectedStdout
              actualStdout
          Nothing -> pass
        case mExpectedStderr of
          Just expectedStderr -> do
            actualStderr <- Knob.getContents stderrKnob
            assertEqual
              ( show $
                  hsep
                    [ "stderr",
                      viaShow expectedStderr,
                      "=",
                      viaShow actualStderr
                    ]
              )
              expectedStderr
              actualStderr
          Nothing -> pass
      Left reports ->
        assertFailure $
          toString $
            unlines $
              map (show . human) reports
