module Arguments
  ( Arguments (..),
    CompileMode (..),
    Prelude (..),
    parseArguments,
  )
where

import Mlatu (Prelude (..))
import Relude
import System.Console.CmdArgs.Explicit
  ( Arg,
    Flag,
    FlagHelp,
    Help,
    HelpFormat (HelpFormatDefault),
    Mode,
    Name,
    Update,
    flagArg,
    flagBool,
    flagHelpSimple,
    flagReq,
    flagVersion,
    helpText,
    mode,
    processArgs,
  )

data Arguments = Arguments
  { compileMode :: !CompileMode,
    inputPaths :: ![FilePath],
    showHelp :: !Bool,
    showVersion :: !Bool,
    prelude :: !Prelude
  }

data CompileMode
  = CheckMode
  | InterpretMode

parseArguments :: IO Arguments
parseArguments = do
  arguments <- processArgs argumentsMode
  when (showVersion arguments) $ do
    putStrLn "Mlatu version 0.1"
    exitSuccess
  when (showHelp arguments) $ do
    print $ helpText [] HelpFormatDefault argumentsMode
    exitSuccess
  return arguments

argumentsMode :: Mode Arguments
argumentsMode =
  mode
    "mlatu"
    defaultArguments
    "Interprets Mlatu code."
    bareArgument
    options

defaultArguments :: Arguments
defaultArguments =
  Arguments
    { compileMode = InterpretMode,
      inputPaths = [],
      showHelp = False,
      showVersion = False,
      prelude = Common
    }

bareArgument :: Arg Arguments
bareArgument = flagArg inputPathArgument "input-paths"

inputPathArgument ::
  FilePath -> Arguments -> Either e Arguments
inputPathArgument path acc =
  Right $
    acc {inputPaths = path : inputPaths acc}

options :: [Flag Arguments]
options =
  [ flagBool
      ["no-common", "foundation-only"]
      ( \flag acc ->
          if flag
            then acc {prelude = Foundation}
            else acc
      )
      "Compiles with the bare minimum prelude.",
    flagBool
      ["check"]
      ( \flag acc ->
          if flag
            then
              acc
                { compileMode = CheckMode
                }
            else acc
      )
      "Check syntax and types without compiling or running.",
    flagHelpSimple $ \acc -> acc {showHelp = True},
    flagVersion $ \acc -> acc {showVersion = True}
  ]