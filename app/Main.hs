module Main where

import Arguments (Arguments, parseArguments)
import Arguments qualified
-- import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Interact qualified
import Mlatu (compile, getCommonPaths, runMlatu)
import Mlatu.Interpret (interpret)
import Mlatu.Name (GeneralName (..), Qualified (..))
import Mlatu.Vocabulary qualified as Vocabulary
import Paths_Mlatu (getDataDir)
import Relude
import Report (reportAll)
import System.IO (hPutStrLn, hSetEncoding, utf8)
import Text.PrettyPrint.HughesPJ qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

main :: IO ()
main = do
  hSetEncoding stdout utf8
  arguments <- parseArguments
  case Arguments.inputPaths arguments of
    [] -> case Arguments.compileMode arguments of
      Arguments.CheckMode -> do
        hPutStrLn stderr "Cannot run interactively in check mode."
        exitFailure
      Arguments.CompileMode {} -> do
        hPutStrLn stderr "Cannot run interactively in compile mode."
        exitFailure
      Arguments.InterpretMode -> Interact.run
      Arguments.FormatMode -> do
        hPutStrLn stderr "Cannot run interactively in format mode."
        exitFailure
    (_ : _) -> runBatch arguments

runBatch :: Arguments -> IO ()
runBatch arguments = do
  let paths = Arguments.inputPaths arguments
  commonPaths <- getCommonPaths getDataDir
  -- time1 <- getCurrentTime
  result <- runMlatu $ compile mainPermissions Nothing (commonPaths ++ paths)
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    Right program ->
      case Arguments.compileMode arguments of
        Arguments.CheckMode -> pass
        Arguments.CompileMode _ -> pass
        Arguments.InterpretMode -> do
          reps <- interpret program Nothing [] stdin stdout stderr []
          -- time2 <- getCurrentTime
          -- putStrLn $ "Time: " <> show (diffUTCTime time2 time1)
          forM_ reps (putStrLn . Pretty.render . pPrint)
        Arguments.FormatMode -> pass
  where
    mainPermissions =
      [ QualifiedName $ Qualified Vocabulary.global "IO",
        QualifiedName $ Qualified Vocabulary.global "Fail"
      ]