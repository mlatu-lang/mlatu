module Main where

import Arguments qualified
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Interact qualified
import Mlatu (Prelude (..), compile, compilePrelude, fragmentFromSource)
import Mlatu.Codegen qualified as Codegen
import Mlatu.Interpret (interpret)
import Mlatu.Monad (runMlatuExceptT)
import Mlatu.Name (GeneralName (..), Qualified (..))
import Mlatu.Pretty (printFragment)
import Mlatu.Report (Report)
import Mlatu.Vocabulary qualified as Vocabulary
import Options.Applicative (execParser, header, helper, info)
import Relude
import Report (reportAll)
import System.Directory (makeAbsolute, removeFile)
import System.IO (hSetEncoding, utf8)
import System.Process.Typed (proc, runProcess_)
import Text.Printf (printf)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  arguments <- execParser opts
  case arguments of
    Arguments.FormatFiles files -> formatFiles files
    Arguments.Repl prelude -> do
      exitCode <- Interact.run prelude
      case exitCode of
        0 -> exitSuccess
        _ -> exitFailure
    Arguments.CheckFiles prelude files -> checkFiles prelude files
    Arguments.RunFiles prelude files -> runFiles prelude files
    Arguments.CompileFiles prelude files -> compileFiles prelude files
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
formatFiles paths = for_ paths $ \relativePath -> do
  path <- makeAbsolute relativePath
  bs <- readFileBS path
  let text = decodeUtf8 bs
  result <- runMlatuExceptT $ fragmentFromSource mainPermissions Nothing 0 path text
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
  (result, t1) <- timed $
    runMlatuExceptT $ do
      commonDictionary <- compilePrelude prelude mainPermissions Nothing
      compile mainPermissions Nothing paths (Just commonDictionary)
  case result of
    Left reports -> handleReports reports
    Right _ -> reportTime [("generate the IR from the source", t1)]

runFiles :: Prelude -> [FilePath] -> IO ()
runFiles prelude relativePaths =
  forM relativePaths makeAbsolute
    >>= ( \paths ->
            timed (runMlatuExceptT (compilePrelude prelude mainPermissions Nothing >>= (compile mainPermissions Nothing paths . Just)))
              >>= ( \(result, t1) -> case result of
                      Left reports -> handleReports reports
                      Right program ->
                        timed (interpret program Nothing [] stdin stdout stderr [])
                          >>= (\(_, t2) -> reportTime [("generate the IR from the source", t1), ("interpet the IR", t2)])
                  )
        )

compileFiles :: Prelude -> [FilePath] -> IO ()
compileFiles prelude relativePaths = do
  paths <- forM relativePaths makeAbsolute
  result <-
    runMlatuExceptT $ do
      commonDictionary <- compilePrelude prelude mainPermissions Nothing
      compile mainPermissions Nothing paths (Just commonDictionary)
  case result of
    Left reports -> handleReports reports
    Right program -> do
      bs <- Codegen.generate program
      writeFileBS "output.rs" bs
      runProcess_ $ proc "rustfmt" ["output.rs"]
      runProcess_ $ proc "rustc" ["-C", "opt-level=3", "output.rs"]
      removeFile "output.rs"
      runProcess_ $ proc "upx" ["--best", "-q", "output"]

timed' :: IO () -> IO NominalDiffTime
timed' comp =
  getCurrentTime
    >>= (\t1 -> comp >>= const (getCurrentTime >>= (\t2 -> pure (diffUTCTime t2 t1))))

timed :: IO a -> IO (a, NominalDiffTime)
timed comp =
  getCurrentTime
    >>= (\t1 -> comp >>= (\result -> getCurrentTime >>= (\t2 -> pure (result, diffUTCTime t2 t1))))

reportTime :: [(String, NominalDiffTime)] -> IO ()
reportTime times = putStrLn ("\n---" <> concatMap report times <> printf "\nTotal time: %.4f seconds\n---" whole)
  where
    whole :: Double
    whole = sum (fmap (realToFrac . snd) times)

    report :: (String, NominalDiffTime) -> String
    report (text, time) = printf "\nTime taken to %s: %.4f seconds, %.2f%% of total time" text fracTime (100.0 * (fracTime / whole))
      where
        fracTime :: Double
        fracTime = realToFrac time
