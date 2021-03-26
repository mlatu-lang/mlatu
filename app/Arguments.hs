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

options :: Parser Options
options = subparser (replCommand <> checkFilesCommand <> formatFilesCommand <> runFilesCommand <> compileFilesCommand)

preludeFlag :: Parser Prelude
preludeFlag = (\b -> if b then Foundation else Common) <$> switch (long "foundation-only" <> short 'f' <> help "Whether to only include foundation as the prelude")

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
compileFilesCommand = command "compile" (info (CompileFiles <$> preludeFlag <*> filesArgument) (progDesc "Compiles Mlatu files"))
