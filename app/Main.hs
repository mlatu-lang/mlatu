module Main where

import Arguments qualified
import Data.ByteString (hPut)
import Interact qualified
import Mlatu (Prelude (..), compileWithPrelude, fragmentFromSource, runMlatuExceptT)
import Mlatu.Codegen qualified as Codegen
import Mlatu.Name (GeneralName (..))
import Mlatu.Pretty (printFragment)
import Mlatu.Report (Report)
import Mlatu.Vocabulary
import Options.Applicative (execParser, header, helper, info)
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.Text (renderIO)
import Relude
import Report (reportAll)
import System.Directory (makeAbsolute, removeFile)
import System.IO (hClose, hSetEncoding, utf8)
import System.IO.Temp (withSystemTempFile)
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
    Arguments.RunFiles prelude b files -> compileFiles prelude files (runRust b)
    Arguments.CompileFiles prelude files -> compileFiles prelude files buildRust
  where
    opts =
      info (Arguments.options <**> helper) (header "The Mlatu programming language")

mainPermissions :: [GeneralName]
mainPermissions =
  [ QualifiedName $ Global "io",
    QualifiedName $ Global "fail"
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
      withFile path WriteMode (`renderIO` layoutSmart defaultLayoutOptions (printFragment fragment))
      putStrLn ("Formatted " <> path <> " successfully")

checkFiles :: Prelude -> [FilePath] -> IO ()
checkFiles prelude relativePaths =
  forM relativePaths makeAbsolute
    >>= (\paths -> runMlatuExceptT (compileWithPrelude prelude mainPermissions Nothing paths) >>= (`whenLeft_` handleReports))

compileFiles :: Prelude -> [FilePath] -> RustHandler -> IO ()
compileFiles prelude relativePaths handler =
  forM relativePaths makeAbsolute
    >>= (runMlatuExceptT . compileWithPrelude prelude mainPermissions Nothing)
    >>= ( \case
            Left reports -> handleReports reports
            Right program ->
              ( Codegen.generate program Nothing
                  >>= (withSystemTempFile "output.rs" . handler)
              )
        )

type RustHandler = ByteString -> FilePath -> Handle -> IO ()

runRust :: Bool -> RustHandler
runRust b contents path handle =
  hPut handle contents
    >> runProcess_
      ( proc
          "rustc"
          [ "--emit=link",
            "--crate-type=bin",
            "--edition=2018",
            "-C",
            "opt-level=3",
            "-C",
            "lto=y",
            "-C",
            "panic=abort",
            "-C",
            "codegen-units=1",
            "-o",
            "./output",
            path
          ]
      )
    >> hClose handle
    >> if b
      then runProcess_ (proc "time" ["./output"])
      else
        runProcess_ (proc "./output" [])
          >> removeFile "./output"

buildRust :: RustHandler
buildRust contents path handle =
  hPut handle contents
    >> runProcess_
      ( proc
          "rustc"
          [ "--emit=link",
            "--crate-type=bin",
            "--edition=2018",
            "-C",
            "opt-level=z",
            "-C",
            "lto=y",
            "-C",
            "panic=abort",
            "-C",
            "codegen-units=1",
            "-o",
            "./output",
            path
          ]
      )
    >> hClose handle
    >> runProcess_ (proc "strip" ["./output"])
    >> runProcess_ (proc "upx" ["--best", "--lzma", "./output"])
