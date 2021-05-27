module Main where

import Arguments qualified
import Interact qualified
import Mlatu (Prelude (..), compileWithPrelude, fragmentFromSource, runMlatu)
import Mlatu.Codegen qualified as Codegen
import Mlatu.Informer (Report)
import Mlatu.Name (GeneralName (..))
import Mlatu.Pretty (printFragment)
import Mlatu.Vocabulary
import Options.Applicative (execParser, header, helper, info)
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.Text (renderIO)
import Relude
import Report (reportAll)
import System.Directory (copyFile, createDirectory, makeAbsolute, removeDirectoryRecursive, removeFile, withCurrentDirectory)
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
    Arguments.BenchFiles prelude files -> benchFiles prelude files
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
  result <- runMlatu $ fragmentFromSource mainPermissions Nothing 0 path text
  case result of
    Left reports -> handleReports reports
    Right fragment -> do
      withFile path WriteMode (`renderIO` layoutSmart defaultLayoutOptions (printFragment fragment))
      putStrLn ("Formatted " <> path <> " successfully")

checkFiles :: Prelude -> [FilePath] -> IO ()
checkFiles prelude relativePaths =
  forM relativePaths makeAbsolute
    >>= (\paths -> runMlatu (compileWithPrelude prelude mainPermissions Nothing paths) >>= (`whenLeft_` handleReports))

runFiles :: Prelude -> [FilePath] -> IO ()
runFiles prelude relativePaths =
  forM relativePaths makeAbsolute
    >>= (runMlatu . compileWithPrelude prelude mainPermissions Nothing)
    >>= ( \case
            Left reports -> handleReports reports
            Right program ->
              ( Codegen.generate program Nothing >>= \contents ->
                  createDirectory "t"
                    >> writeFileBS "t/Cargo.toml" cargoToml
                    >> createDirectory "t/src"
                    >> writeFileBS "t/src/main.rs" contents
                    >> withCurrentDirectory
                      "t"
                      ( runProcess_
                          (proc "cargo" ["+nightly", "run", "--quiet"])
                      )
                    >> removeDirectoryRecursive "t"
              )
        )

compileFiles :: Prelude -> [FilePath] -> IO ()
compileFiles prelude relativePaths =
  forM relativePaths makeAbsolute
    >>= (runMlatu . compileWithPrelude prelude mainPermissions Nothing)
    >>= ( \case
            Left reports -> handleReports reports
            Right program ->
              ( Codegen.generate program Nothing >>= \contents ->
                  createDirectory "t"
                    >> writeFileBS "t/Cargo.toml" cargoToml
                    >> createDirectory "t/src"
                    >> writeFileBS "t/src/main.rs" contents
                    >> withCurrentDirectory
                      "t"
                      ( runProcess_
                          ( proc
                              "cargo"
                              [ "+nightly",
                                "build",
                                "--quiet"
                              ]
                          )
                      )
                    >> copyFile "t/target/debug/output" "./output"
                    >> removeDirectoryRecursive "t"
                    >> runProcess_ (proc "strip" ["./output"])
                    >> runProcess_ (proc "upx" ["./output"])
              )
        )

benchFiles :: Prelude -> [FilePath] -> IO ()
benchFiles prelude relativePaths =
  forM relativePaths makeAbsolute
    >>= (runMlatu . compileWithPrelude prelude mainPermissions Nothing)
    >>= ( \case
            Left reports -> handleReports reports
            Right program ->
              ( Codegen.generate program Nothing >>= \contents ->
                  createDirectory "t"
                    >> writeFileBS "t/Cargo.toml" cargoToml
                    >> createDirectory "t/src"
                    >> writeFileBS "t/src/main.rs" contents
                    >> withCurrentDirectory
                      "t"
                      ( runProcess_
                          ( proc
                              "cargo"
                              [ "+nightly",
                                "build",
                                "--quiet"
                              ]
                          )
                      )
                    >> copyFile "t/target/debug/output" "./output"
                    >> removeDirectoryRecursive "t"
                    >> runProcess_ (proc "time" ["./output"])
                    >> removeFile "./output"
              )
        )

cargoToml :: ByteString
cargoToml =
  "[package] \n\
  \name = \"output\" \n\
  \version = \"0.1.0\" \n\
  \[dependencies.smallvec] \n\
  \version = \"1.6.1\" \n\
  \features = [\"union\"]"
