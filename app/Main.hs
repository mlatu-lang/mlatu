module Main where

import Arguments qualified
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
    Arguments.RunFiles prelude o files -> runFiles prelude o files
    Arguments.CompileFiles prelude o files -> compileFiles prelude o files
    Arguments.BenchFiles prelude o files -> benchFiles prelude o files
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

runFiles :: Prelude -> Arguments.Onlineness -> [FilePath] -> IO ()
runFiles prelude o relativePaths =
  forM relativePaths makeAbsolute
    >>= (runMlatuExceptT . compileWithPrelude prelude mainPermissions Nothing)
    >>= ( \case
            Left reports -> handleReports reports
            Right program ->
              ( Codegen.generate program Nothing (o == Arguments.Online) >>= \contents ->
                  createDirectory "t"
                    >> writeFileBS "t/Cargo.toml" (cargoToml False (o == Arguments.Online))
                    >> createDirectory "t/src"
                    >> writeFileBS "t/src/main.rs" contents
                    >> withCurrentDirectory
                      "t"
                      ( runProcess_
                          (proc "cargo" ["+nightly", "run", "--release", "--quiet"])
                      )
                    >> removeDirectoryRecursive "t"
              )
        )

compileFiles :: Prelude -> Arguments.Onlineness -> [FilePath] -> IO ()
compileFiles prelude o relativePaths =
  forM relativePaths makeAbsolute
    >>= (runMlatuExceptT . compileWithPrelude prelude mainPermissions Nothing)
    >>= ( \case
            Left reports -> handleReports reports
            Right program ->
              ( Codegen.generate program Nothing (o == Arguments.Online) >>= \contents ->
                  createDirectory "t"
                    >> writeFileBS "t/Cargo.toml" (cargoToml True (o == Arguments.Online))
                    >> createDirectory "t/src"
                    >> writeFileBS "t/src/main.rs" contents
                    >> withCurrentDirectory
                      "t"
                      ( runProcess_
                          ( proc
                              "cargo"
                              [ "+nightly",
                                "build",
                                "--release",
                                "--quiet"
                              ]
                          )
                      )
                    >> copyFile "t/target/release/output" "./output"
                    >> removeDirectoryRecursive "t"
                    >> runProcess_ (proc "strip" ["./output"])
                    >> runProcess_ (proc "upx" ["./output"])
              )
        )

benchFiles :: Prelude -> Arguments.Onlineness -> [FilePath] -> IO ()
benchFiles prelude o relativePaths =
  forM relativePaths makeAbsolute
    >>= (runMlatuExceptT . compileWithPrelude prelude mainPermissions Nothing)
    >>= ( \case
            Left reports -> handleReports reports
            Right program ->
              ( Codegen.generate program Nothing (o == Arguments.Online) >>= \contents ->
                  createDirectory "t"
                    >> writeFileBS "t/Cargo.toml" (cargoToml False (o == Arguments.Online))
                    >> createDirectory "t/src"
                    >> writeFileBS "t/src/main.rs" contents
                    >> withCurrentDirectory
                      "t"
                      ( runProcess_
                          ( proc
                              "cargo"
                              [ "+nightly",
                                "build",
                                "--release",
                                "--quiet"
                              ]
                          )
                      )
                    >> copyFile "t/target/release/output" "./output"
                    >> removeDirectoryRecursive "t"
                    >> runProcess_ (proc "time" ["./output"])
                    >> removeFile "./output"
              )
        )

cargoToml b o =
  "[package] \n \
  \ name = \"output\" \n \
  \ version = \"0.1.0\" \n "
    <> ( if o
           then "[dependencies.smallvec] \nversion = \"1.6.1\" \nfeatures = [\"union\"] \n"
           else ""
       )
    <> " [profile.release] \n \
       \ lto = true \n \
       \ codegen-units = 1 \n\
       \ panic = 'abort' "
    <> if b
      then "\n opt-level = \"s\" \n"
      else ""
