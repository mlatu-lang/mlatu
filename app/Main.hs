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
    Arguments.RunFiles prelude files -> compileFiles prelude files True
    Arguments.CompileFiles prelude files -> compileFiles prelude files False
  where
    opts =
      info (Arguments.options <**> helper) (header "The Mlatu programming language")

mainPermissions :: [GeneralName]
mainPermissions =
  [ QualifiedName $ Qualified Vocabulary.global "io",
    QualifiedName $ Qualified Vocabulary.global "fail"
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
      putStrLn $
        if newText /= toString text
          then "Formatted " <> path <> " successfully"
          else path <> " was already formatted"

checkFiles :: Prelude -> [FilePath] -> IO ()
checkFiles prelude relativePaths =
  forM relativePaths makeAbsolute
    >>= (\paths -> runMlatuExceptT (compileWithPrelude prelude mainPermissions Nothing paths) >>= (`whenLeft_` handleReports))

compileFiles :: Prelude -> [FilePath] -> Bool -> IO ()
compileFiles prelude relativePaths toRun =
  let name = "output"
   in forM relativePaths makeAbsolute
        >>= (runMlatuExceptT . compileWithPrelude prelude mainPermissions Nothing)
        >>= ( \case
                Left reports -> handleReports reports
                Right program ->
                  ( Codegen.generate program
                      >>= writeFileBS (name <> ".rs")
                  )
                    >> runProcess_
                      ( proc "rustfmt" [name <> ".rs"]
                      )
                    >> runProcess_
                      ( proc "rustc" ["-C", "opt-level=3", name <> ".rs"]
                      )
                    >> removeFile (name <> ".rs")
                    >> runProcess_
                      ( if toRun
                          then proc ("./" <> name) []
                          else proc "upx" ["--best", "-q", name]
                      )
            )
