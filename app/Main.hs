module Main where

import Arguments qualified
import Interact qualified
import Mlatu (Prelude (..), compileWithPrelude, fragmentFromSource, runMlatuExceptT)
import Mlatu.Codegen qualified as Codegen
import Mlatu.Interpret (interpret)
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

handleReports :: [Report] -> IO ()
handleReports reports = do
  reportAll reports
  exitFailure

formatFiles :: [FilePath] -> IO ()
formatFiles paths = for_ paths $ \relativePath -> do
  path <- makeAbsolute relativePath
  bs <- readFileBS path
  let text = decodeUtf8 bs
  result <- runMlatuExceptT $ fragmentFromSource Nothing 0 path text
  case result of
    Left reports -> handleReports reports
    Right fragment -> do
      let newText = show $ printFragment fragment
      writeFile path newText
      putStrLn $
        if newText /= toString text
          then "Formatted " <> path <> " successfully"
          else path <> " was already formatted"

checkFiles :: Prelude -> [FilePath] -> IO ()
checkFiles prelude relativePaths =
  forM relativePaths makeAbsolute
    >>= (\paths -> runMlatuExceptT (compileWithPrelude prelude Nothing paths) >>= (`whenLeft_` handleReports))

runFiles :: Prelude -> [FilePath] -> IO ()
runFiles prelude relativePaths =
  forM relativePaths makeAbsolute
    >>= ( \paths ->
            runMlatuExceptT (compileWithPrelude prelude Nothing paths)
              >>= ( \case
                      Left reports -> handleReports reports
                      Right program -> void (interpret program Nothing [] stdin stdout stderr [])
                  )
        )

compileFiles :: Prelude -> [FilePath] -> IO ()
compileFiles prelude relativePaths = do
  forM relativePaths makeAbsolute
    >>= (runMlatuExceptT . compileWithPrelude prelude Nothing)
    >>= ( \case
            Left reports -> handleReports reports
            Right program ->
              ( Codegen.generate program
                  >>= writeFileBS "output.rs"
              )
                >> runProcess_
                  ( proc "rustfmt" ["output.rs"]
                  )
                >> runProcess_
                  ( proc "rustc" ["-C", "opt-level=3", "output.rs"]
                  )
                >> removeFile "output.rs"
                >> runProcess_
                  ( proc "upx" ["--best", "-q", "output"]
                  )
        )
