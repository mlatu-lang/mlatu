module Main where

import Arguments qualified
import Interact qualified
import Mlatu (Prelude (..), compileWithPrelude, fragmentFromSource, runMlatu)
import Mlatu.Erlang.Print qualified as Erlang
import Mlatu.Informer (Report, warnCheckpoint)
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
    Arguments.RunFiles False prelude files -> runFiles prelude files
    Arguments.RunFiles True prelude files -> benchFiles prelude files
    Arguments.CompileFiles prelude files -> compileFiles prelude files
  where
    opts =
      info (Arguments.options <**> helper) (header "The Mlatu programming language")

mainPermissions :: [GeneralName]
mainPermissions =
  [ QualifiedName $ Global "io",
    QualifiedName $ Global "fail",
    QualifiedName $ Global "otp"
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
  (result, reports) <- runMlatu $ fragmentFromSource mainPermissions Nothing 0 path text
  reportAll reports
  case result of
    Nothing -> exitFailure
    Just fragment -> do
      withFile path WriteMode (`renderIO` layoutSmart defaultLayoutOptions (printFragment fragment))
      putStrLn ("Formatted " <> path <> " successfully")

checkFiles :: Prelude -> [FilePath] -> IO ()
checkFiles prelude relativePaths =
  forM relativePaths makeAbsolute
    >>= ( \paths ->
            runMlatu
              ( do
                  dict <- compileWithPrelude prelude mainPermissions Nothing paths
                  warnCheckpoint
                  pure dict
              )
              >>= reportAll . snd
        )

base :: IO () -> Prelude -> [FilePath] -> IO ()
base after prelude relativePaths =
  forM relativePaths makeAbsolute
    >>= (runMlatu . compileWithPrelude prelude mainPermissions Nothing)
    >>= ( \(result, reports) ->
            reportAll reports >> case result of
              Nothing -> exitFailure
              Just program ->
                Erlang.generate program Nothing >>= \contents ->
                  writeFileText "mlatu.erl" contents
                    >> runProcess_ "erlc -W0 mlatu.erl"
                    -- >> removeFile "mlatu.erl"
                    >> after
        )

runFiles :: Prelude -> [FilePath] -> IO ()
runFiles = base (runProcess_ "escript mlatu.beam" >> removeFile "mlatu.beam")

compileFiles :: Prelude -> [FilePath] -> IO ()
compileFiles = base (putStrLn "Produced the BEAM bytecode file `mlatu.beam`.\nExecute it by running `escript mlatu.beam`")

benchFiles :: Prelude -> [FilePath] -> IO ()
benchFiles = base (runProcess_ "time escript mlatu.beam" >> removeFile "mlatu.beam")
