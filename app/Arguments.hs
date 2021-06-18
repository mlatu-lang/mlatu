module Arguments
  ( Options (..),
    options,
  )
where

import Options.Applicative

data Options
  = CheckFiles ![FilePath]
  | FormatFiles ![FilePath]
  | RunFiles !Bool ![FilePath]
  | CompileFiles ![FilePath]
  | Script !Text

options :: Parser Options
options = subparser (checkFilesCommand <> formatFilesCommand <> runFilesCommand <> compileFilesCommand <> scriptCommand)

benchFlag :: Parser Bool
benchFlag = switch (long "bench" <> short 'b' <> help "Benchmark the time taken to run")

filesArgument :: Parser [FilePath]
filesArgument = some (argument str (metavar "FILES..."))

formatFilesCommand :: Mod CommandFields Options
formatFilesCommand = command "fmt" (info (FormatFiles <$> filesArgument) (progDesc "Formats Mlatu files prettily"))

checkFilesCommand :: Mod CommandFields Options
checkFilesCommand = command "check" (info (CheckFiles <$> filesArgument) (progDesc "Checks Mlatu files for correctness without running them"))

runFilesCommand :: Mod CommandFields Options
runFilesCommand = command "run" (info (RunFiles <$> benchFlag <*> filesArgument) (progDesc "Runs Mlatu files"))

compileFilesCommand :: Mod CommandFields Options
compileFilesCommand = command "build" (info (CompileFiles <$> filesArgument) (progDesc "Builds Mlatu files into an executable"))

textArgument :: Parser Text
textArgument = argument str (metavar "INPUT")

scriptCommand :: Mod CommandFields Options
scriptCommand = command "script" (info (Script <$> textArgument) (progDesc "Runs and prints the output of a Mlatu script"))
