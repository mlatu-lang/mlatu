module Main where

import Arguments (Arguments, parseArguments)
import Arguments qualified
import Interact qualified
import Mlatu (compile, compilePrelude, fragmentFromSource, runMlatu)
import Mlatu.Interpret (interpret)
import Mlatu.Name (GeneralName (..), Qualified (..))
import Mlatu.Pretty (printFragment)
import Mlatu.Vocabulary qualified as Vocabulary
import Relude
import Report (reportAll)
import System.Directory (makeAbsolute)
import System.IO (hPutStrLn, hSetEncoding, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  arguments <- parseArguments
  if null $ Arguments.inputPaths arguments
    then case Arguments.compileMode arguments of
      Arguments.CheckMode -> hPutStrLn stderr "Cannot run interactively in check mode." >> exitFailure
      Arguments.FormatMode -> hPutStrLn stderr "Cannot run interactively in format mode." >> exitFailure
      Arguments.InterpretMode -> Interact.run (Arguments.prelude arguments)
    else runBatch arguments

runBatch :: Arguments -> IO ()
runBatch arguments = do
  paths <- forM (Arguments.inputPaths arguments) makeAbsolute
  case Arguments.compileMode arguments of
    Arguments.FormatMode -> forM_ paths formatMode
    Arguments.CheckMode -> checkMode (Arguments.prelude arguments) paths
    Arguments.InterpretMode -> interpretMode (Arguments.prelude arguments) paths

mainPermissions :: [GeneralName]
mainPermissions =
  [ QualifiedName $ Qualified Vocabulary.global "IO",
    QualifiedName $ Qualified Vocabulary.global "Fail"
  ]

formatMode :: FilePath -> IO ()
formatMode path = do
  bs <- readFileBS path
  let text = decodeUtf8 bs
  result <- runExceptT $ runMlatu $ fragmentFromSource mainPermissions Nothing 0 path text
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    Right fragment -> do
      let newText = show $ printFragment fragment
      writeFile path newText
      if newText /= toString text
        then print $ "Formatted " <> path <> " successfully"
        else print $ path <> "was already formatted"

checkMode :: Arguments.Prelude -> [FilePath] -> IO ()
checkMode prelude paths = do
  result <-
    runExceptT $
      runMlatu
        ( do
            commonDictionary <- compilePrelude prelude mainPermissions Nothing
            compile mainPermissions Nothing paths (Just commonDictionary)
        )
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    Right _ -> pass

interpretMode :: Arguments.Prelude -> [FilePath] -> IO ()
interpretMode prelude paths = do
  result <- runExceptT $
    runMlatu $ do
      commonDictionary <- compilePrelude prelude mainPermissions Nothing
      compile mainPermissions Nothing paths (Just commonDictionary)
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    Right program -> do
      _ <- interpret program Nothing [] stdin stdout stderr []
      pass
