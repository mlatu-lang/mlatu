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
import System.Directory (createDirectory, makeAbsolute, removeDirectoryRecursive, removeFile, withCurrentDirectory)
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
    Arguments.RunFiles False prelude release files -> runFiles prelude release files
    Arguments.RunFiles True prelude release files -> benchFiles prelude release files
    Arguments.CompileFiles prelude release files -> compileFiles prelude release files
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

careTaken :: FilePath -> IO () -> IO ()
careTaken name action =
  createDirectory name
    >> withCurrentDirectory name action
    >> removeDirectoryRecursive name

runFiles :: Prelude -> Bool -> [FilePath] -> IO ()
runFiles prelude release relativePaths =
  forM relativePaths makeAbsolute
    >>= (runMlatu . compileWithPrelude prelude mainPermissions Nothing)
    >>= ( \case
            Left reports -> handleReports reports
            Right program ->
              ( Codegen.generate program Nothing >>= \contents ->
                  careTaken
                    "out"
                    ( writeFileBS "Cargo.toml" cargoToml
                        >> unless
                          release
                          ( createDirectory ".cargo"
                              >> writeFileBS ".cargo/config.toml" configToml
                          )
                        >> createDirectory "src"
                        >> writeFileBS "src/main.rs" contents
                        >> runProcess_
                          ( if release
                              then "cargo +nightly run --quiet --release"
                              else "cargo +nightly run --quiet"
                          )
                    )
              )
        )

compileFiles :: Prelude -> Bool -> [FilePath] -> IO ()
compileFiles prelude release relativePaths =
  forM relativePaths makeAbsolute
    >>= (runMlatu . compileWithPrelude prelude mainPermissions Nothing)
    >>= ( \case
            Left reports -> handleReports reports
            Right program ->
              ( Codegen.generate program Nothing >>= \contents ->
                  careTaken
                    "out"
                    ( writeFileBS "Cargo.toml" cargoToml
                        >> unless
                          release
                          ( createDirectory ".cargo"
                              >> writeFileBS ".cargo/config.toml" configToml
                          )
                        >> createDirectory "src"
                        >> writeFileBS "src/main.rs" contents
                        >> runProcess_
                          ( if release
                              then "cargo +nightly build --quiet -Z unstable-options --out-dir .. --release"
                              else "cargo +nightly build --quiet -Z unstable-options --out-dir .."
                          )
                    )
                    >> putStrLn "Produced the executable ./output"
              )
        )

benchFiles :: Prelude -> Bool -> [FilePath] -> IO ()
benchFiles prelude release relativePaths =
  forM relativePaths makeAbsolute
    >>= (runMlatu . compileWithPrelude prelude mainPermissions Nothing)
    >>= ( \case
            Left reports -> handleReports reports
            Right program ->
              ( Codegen.generate program Nothing >>= \contents ->
                  careTaken
                    "out"
                    ( writeFileBS "Cargo.toml" cargoToml
                        >> unless
                          release
                          ( createDirectory ".cargo"
                              >> writeFileBS ".cargo/config.toml" configToml
                          )
                        >> createDirectory "src"
                        >> writeFileBS "src/main.rs" contents
                        >> runProcess_
                          ( if release
                              then "cargo +nightly build --quiet -Z unstable-options --out-dir .. --release"
                              else "cargo +nightly build --quiet -Z unstable-options --out-dir .."
                          )
                    )
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

configToml :: ByteString
configToml =
  "[target.x86_64-unknown-linux-gnu]\n\
  \linker = \"/usr/bin/clang\"\n\
  \rustflags = [\"-Clink-arg=-fuse-ld=lld\", \"-Zshare-generics=y\"]\n\
  \[target.x86_64-apple-darwin]\n\
  \rustflags = [\"-C\", \"link-arg=-fuse-ld=/usr/local/bin/zld\", \"-Zshare-generics=y\", \"-Csplit-debuginfo=unpacked\"]\n\
  \[target.x86_64-pc-windows-msvc]\n\
  \linker = \"rust-lld.exe\"\n\
  \rustflags = [\"-Zshare-generics=y\"]"
