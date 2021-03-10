module Main where

import Data.Version (makeVersion)
import Interact qualified
import Mlatu (Prelude (..), compile, compilePrelude, fragmentFromSource, runMlatu)
import Mlatu.Interpret (interpret)
import Mlatu.Name (GeneralName (..), Qualified (..))
import Mlatu.Pretty (printFragment)
import Mlatu.Report (Report)
import Mlatu.Vocabulary qualified as Vocabulary
import Relude
import Report (reportAll)
import SimpleCmdArgs
import System.Directory (makeAbsolute)
import System.IO (hSetEncoding, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  simpleCmdArgs (Just (makeVersion [0, 1])) "mlatu" "The Mlatu programming language" $
    subcommands
      [ Subcommand "fmt" "Formats Mlatu files prettily" $
          ( \ps -> do
              paths <- absolutizePaths ps
              forM_ paths (handleErrors . formatFile)
          )
            <$> some (strArg "FILE..."),
        Subcommand "check" "Checks Mlatu files for errors" $
          ( \foundationOnly ps -> do
              paths <- absolutizePaths ps
              handleErrors
                ( checkFiles
                    ( if foundationOnly
                        then Foundation
                        else Common
                    )
                    paths
                )
          )
            <$> preludeOpts
            <*> some (strArg "FILE..."),
        Subcommand "run" "Interprets Mlatu files" $
          ( \foundationOnly ps -> do
              paths <- absolutizePaths ps
              handleErrors
                ( runFiles
                    ( if foundationOnly
                        then Foundation
                        else Common
                    )
                    paths
                )
          )
            <$> preludeOpts
            <*> some (strArg "FILE..."),
        Subcommand "repl" "Starts the Mlatu REPL" $
          ( \foundationOnly ->
              Interact.run
                ( if foundationOnly
                    then Foundation
                    else Common
                )
          )
            <$> preludeOpts
      ]
  where
    preludeOpts = switchWith 'f' "foundation" "Use only foundation"

    absolutizePaths :: [FilePath] -> IO [FilePath]
    absolutizePaths paths = forM paths makeAbsolute

mainPermissions :: [GeneralName]
mainPermissions =
  [ QualifiedName $ Qualified Vocabulary.global "IO",
    QualifiedName $ Qualified Vocabulary.global "Fail"
  ]

handleErrors :: ExceptT [Report] IO () -> IO ()
handleErrors x = do
  result <- runExceptT x
  bifor_ result ((>> exitFailure) . reportAll) (const pass)

formatFile :: FilePath -> ExceptT [Report] IO ()
formatFile path =
  readFileBS path
    >>= ( runMlatu
            . (formatFragment <=< fragmentFromBS)
        )
  where
    formatFragment = writeFile path . show . printFragment
    fragmentFromBS = fragmentFromSource mainPermissions Nothing 0 path . decodeUtf8

checkFiles :: Prelude -> [FilePath] -> ExceptT [Report] IO ()
checkFiles prelude paths = do
  runMlatu
    ( do
        commonDictionary <- compilePrelude prelude mainPermissions Nothing
        compile mainPermissions Nothing paths (Just commonDictionary)
    )
    >> pass

runFiles :: Prelude -> [FilePath] -> ExceptT [Report] IO ()
runFiles prelude paths = do
  program <- runMlatu $ do
    commonDictionary <- compilePrelude prelude mainPermissions Nothing
    compile mainPermissions Nothing paths (Just commonDictionary)
  return $ pass (interpret program Nothing [] stdin stdout stderr [])
