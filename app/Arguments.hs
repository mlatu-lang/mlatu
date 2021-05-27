module Arguments
  ( Options (..),
    options,
  )
where

import Mlatu (Prelude (..))
import Options.Applicative
import Relude

data Options
  = CheckFiles !Prelude ![FilePath]
  | Repl !Prelude
  | FormatFiles ![FilePath]
  | RunFiles !Prelude ![FilePath]
  | CompileFiles !Prelude ![FilePath]
  | BenchFiles !Prelude ![FilePath]

options :: Parser Options
options = subparser (replCommand <> checkFilesCommand <> formatFilesCommand <> runFilesCommand <> compileFilesCommand <> benchFilesCommand)

preludeFlag :: Parser Prelude
preludeFlag = (\b -> if b then Foundation else Common) <$> switch (long "foundation-only" <> short 'f' <> help "Include the foundation only as the prelude")

filesArgument :: Parser [FilePath]
filesArgument = some (argument str (metavar "FILES..."))

replCommand :: Mod CommandFields Options
replCommand = command "repl" (info (Repl <$> preludeFlag) (progDesc "Start the interactive REPL"))

formatFilesCommand :: Mod CommandFields Options
formatFilesCommand = command "fmt" (info (FormatFiles <$> filesArgument) (progDesc "Formats Mlatu files prettily"))

checkFilesCommand :: Mod CommandFields Options
checkFilesCommand = command "check" (info (CheckFiles <$> preludeFlag <*> filesArgument) (progDesc "Checks Mlatu files for correctness without running them"))

runFilesCommand :: Mod CommandFields Options
runFilesCommand = command "run" (info (RunFiles <$> preludeFlag <*> filesArgument) (progDesc "Runs Mlatu files"))

compileFilesCommand :: Mod CommandFields Options
compileFilesCommand = command "build" (info (CompileFiles <$> preludeFlag <*> filesArgument) (progDesc "Builds Mlatu files into an executable"))

benchFilesCommand :: Mod CommandFields Options
benchFilesCommand = command "bench" (info (BenchFiles <$> preludeFlag <*> filesArgument) (progDesc "Benchmarks the running of the produced Mlatu executable"))
