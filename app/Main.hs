module Main where

import Arguments (Arguments, parseArguments)
import Arguments qualified
import Interact qualified
import Mlatu (compile, compilePrelude, fragmentFromSource, runMlatu)
import Mlatu.Interpret (interpret)
import Mlatu.Name (GeneralName (..), Qualified (..))
import Mlatu.Pretty (printFragment)
import Mlatu.Report (Report)
import Mlatu.Vocabulary qualified as Vocabulary
import Relude
import Report (reportAll)
import System.Directory (makeAbsolute)
import System.IO (hPutStrLn, hSetEncoding, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  arguments <- parseArguments
  case Arguments.inputPaths arguments of
    [] -> case Arguments.compileMode arguments of
      Arguments.CheckMode -> hPutStrLn stderr "Cannot run interactively in check mode." >> exitFailure
      Arguments.FormatMode -> hPutStrLn stderr "Cannot run interactively in format mode." >> exitFailure
      Arguments.InterpretMode -> Interact.run (Arguments.prelude arguments)
    (_ : _) -> runBatch arguments

runBatch :: Arguments -> IO ()
runBatch arguments = do
  paths <- forM (Arguments.inputPaths arguments) makeAbsolute
  result <- runExceptT $ case Arguments.compileMode arguments of
    Arguments.FormatMode -> forM_ paths formatMode
    Arguments.CheckMode -> checkMode (Arguments.prelude arguments) paths
    Arguments.InterpretMode -> interpretMode (Arguments.prelude arguments) paths
  bifor_ result ((>> exitFailure) . reportAll) (const pass)

mainPermissions :: [GeneralName]
mainPermissions =
  [ QualifiedName $ Qualified Vocabulary.global "IO",
    QualifiedName $ Qualified Vocabulary.global "Fail"
  ]

formatMode :: FilePath -> ExceptT [Report] IO ()
formatMode path =
  readFileBS path
    >>= ( runMlatu
            . (formatFragment <=< fragmentFromBS)
        )
  where
    formatFragment = writeFile path . show . printFragment
    fragmentFromBS = fragmentFromSource mainPermissions Nothing 0 path . decodeUtf8

checkMode :: Arguments.Prelude -> [FilePath] -> ExceptT [Report] IO ()
checkMode prelude paths = do
  runMlatu
    ( do
        commonDictionary <- compilePrelude prelude mainPermissions Nothing
        compile mainPermissions Nothing paths (Just commonDictionary)
    )
    >> pass

interpretMode :: Arguments.Prelude -> [FilePath] -> ExceptT [Report] IO ()
interpretMode prelude paths = do
  program <- runMlatu $ do
    commonDictionary <- compilePrelude prelude mainPermissions Nothing
    compile mainPermissions Nothing paths (Just commonDictionary)
  return $ pass (interpret program Nothing [] stdin stdout stderr [])
