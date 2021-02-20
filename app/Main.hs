module Main where

import Arguments (Arguments, parseArguments)
import Arguments qualified
-- import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Interact qualified
import Mlatu (compile, compilePrelude, fragmentFromSource, runMlatu)
import Mlatu.Interpret (interpret)
import Mlatu.Name (GeneralName (..), Qualified (..))
import Mlatu.Vocabulary qualified as Vocabulary
import Relude
import Report (reportAll)
import System.Directory (makeAbsolute)
import System.IO (hPutStrLn, hSetEncoding, utf8)
import Mlatu.Report (Report)
import Text.PrettyPrint.HughesPJ qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

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
  result <- runMlatu $ fragmentFromSource mainPermissions Nothing 0 path (decodeUtf8 bs)
  handleLeft result (\fragment -> writeFile path $ Pretty.render $ pPrint fragment)

checkMode :: Arguments.Prelude -> [FilePath] -> IO ()
checkMode prelude paths = do
  commonResult <- runMlatu $ compilePrelude prelude mainPermissions Nothing
  handleLeft commonResult $ \commonDictionary -> do 
      result <- runMlatu $ compile mainPermissions Nothing paths (Just commonDictionary)
      handleLeft result $ const pass

interpretMode :: Arguments.Prelude -> [FilePath] -> IO ()
interpretMode prelude paths = do 
  commonResult <- runMlatu $ compilePrelude prelude mainPermissions Nothing
  handleLeft commonResult  $ \commonDictionary -> do 
      result <- runMlatu $ compile mainPermissions Nothing paths (Just commonDictionary)
      handleLeft result $ \program -> void $ interpret program Nothing [] stdin stdout stderr []
  

handleLeft :: Either [Report] r -> (r -> IO ()) -> IO ()
handleLeft (Left reports) _ = do 
  reportAll reports 
  exitFailure 
handleLeft (Right r) f = f r