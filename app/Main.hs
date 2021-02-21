module Main where

import Arguments (Arguments, parseArguments)
import Arguments qualified
import Interact qualified
import Mlatu (compile, compilePrelude, fragmentFromSource, runMlatu)
import Mlatu.Interpret (interpret)
import Mlatu.Name (GeneralName (..), Qualified (..))
import Mlatu.Vocabulary qualified as Vocabulary
import Relude
import Report (reportAll)
import System.Directory (makeAbsolute)
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
      Arguments.FormatMode -> do
        hPutStrLn stderr "Cannot run interactively in format mode."
        exitFailure
      Arguments.InterpretMode -> Interact.run (Arguments.prelude arguments)
    (_ : _) -> runBatch arguments

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
  result <- runExceptT $ runMlatu $ fragmentFromSource mainPermissions Nothing 0 path (decodeUtf8 bs)
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    Right text -> writeFile path $ Pretty.render $ pPrint text

checkMode :: Arguments.Prelude -> [FilePath] -> IO ()
checkMode prelude paths = do
  result <- runExceptT $ do
    commonDictionary <- runMlatu $ compilePrelude prelude mainPermissions Nothing
    runMlatu $ compile mainPermissions Nothing paths (Just commonDictionary)
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    Right _ -> pass

interpretMode :: Arguments.Prelude -> [FilePath] -> IO ()
interpretMode prelude paths = do
  result <- runExceptT $ do
    commonDictionary <- runMlatu $ compilePrelude prelude mainPermissions Nothing
    runMlatu $ compile mainPermissions Nothing paths (Just commonDictionary)
  case result of
    Left reports -> do
      reportAll reports
      exitFailure
    Right program -> void $ interpret program Nothing [] stdin stdout stderr []
