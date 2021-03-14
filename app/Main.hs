module Main where

import Arguments qualified
import Interact qualified
import Mlatu (compile, compilePrelude, fragmentFromSource, runMlatu, Prelude(..))
import Mlatu.Interpret (interpret)
import Mlatu.Name (GeneralName (..), Qualified (..))
import Mlatu.Report (Report)
import Mlatu.Pretty (printFragment)
import Mlatu.Vocabulary qualified as Vocabulary
import Relude
import Report (reportAll)
import System.Directory (makeAbsolute)
import System.IO (hSetEncoding, utf8)
import Options.Applicative (info, helper, header, execParser)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  arguments <- execParser opts
  case arguments of 
    Arguments.FormatFiles files -> formatFiles files
    Arguments.Repl prelude -> Interact.run prelude 
    Arguments.CheckFiles prelude files -> checkFiles prelude files
    Arguments.RunFiles prelude files -> runFiles prelude files
 where 
  opts = 
    info (Arguments.options <**> helper) (header "The Mlatu programming language")

mainPermissions :: [GeneralName]
mainPermissions =
  [ QualifiedName $ Qualified Vocabulary.global "IO",
    QualifiedName $ Qualified Vocabulary.global "Fail"
  ]

handleReports :: [Report] -> IO ()
handleReports reports = do 
  reportAll reports 
  exitFailure

formatFiles :: [FilePath] -> IO ()
formatFiles paths = forM_ paths $ \relativePath -> do
  path <- makeAbsolute relativePath
  bs <- readFileBS path
  let text = decodeUtf8 bs
  result <- runExceptT $ runMlatu $ fragmentFromSource mainPermissions Nothing 0 path text
  case result of
    Left reports -> handleReports reports
    Right fragment -> do
      let newText = show $ printFragment fragment
      writeFile path newText
      if newText /= toString text
        then putStrLn $ "Formatted " <> path <> " successfully"
        else putStrLn $ path <> " was already formatted"

checkFiles :: Prelude -> [FilePath] -> IO ()
checkFiles prelude relativePaths = do
  paths <- forM relativePaths makeAbsolute
  result <- runExceptT $ runMlatu $ do
            commonDictionary <- compilePrelude prelude mainPermissions Nothing
            compile mainPermissions Nothing paths (Just commonDictionary)
  case result of
    Left reports -> handleReports reports
    Right _ -> pass

runFiles :: Prelude -> [FilePath] -> IO ()
runFiles prelude relativePaths = do
  paths <- forM relativePaths makeAbsolute
  result <- runExceptT $ runMlatu $ do
      commonDictionary <- compilePrelude prelude mainPermissions Nothing
      compile mainPermissions Nothing paths (Just commonDictionary)
  case result of
    Left reports -> handleReports reports
    Right program -> do
      _ <- interpret program Nothing [] stdin stdout stderr []
      pass
