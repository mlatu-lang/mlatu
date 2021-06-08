module Interact
  ( run,
  )
where

import Mlatu (Prelude (..), compilePrelude)
import Mlatu qualified
import Mlatu.Erlang qualified as Erlang
import Mlatu.Dictionary (Dictionary)
import Mlatu.Informer (errorCheckpoint, runMlatu)
import Mlatu.Enter qualified as Enter
import Mlatu.Name
  ( GeneralName (QualifiedName),
    Qualified (Qualified),
    Qualifier (Qualifier),
    Root (Absolute),
    Unqualified (Unqualified),
  )
import Mlatu.Vocabulary
import Relude
import Report (reportAll)
import System.Console.Repline
import System.Directory (createDirectory, removeDirectoryRecursive, removeFile, withCurrentDirectory)
import System.Process.Typed (runProcess_)

type MRepl = HaskelineT (ReaderT Dictionary (StateT (Text, Int) IO))

cmd :: String -> MRepl ()
cmd input = do
  (text, lineNumber) <- get
  commonDictionary <- ask
  let entryNameUnqualified = toText $ "entry" ++ show lineNumber
      entryName =
        Qualified
          (Qualifier Absolute ["interactive"])
          $ Unqualified entryNameUnqualified
  update <- liftIO $ do
    writeFile "interactive.mlt" input
    (result, reports) <- runMlatu $ do
      fragment <-
        Mlatu.fragmentFromSource
          [QualifiedName $ Global "io"]
          (Just entryName)
          lineNumber
          "interactive.mlt"
          (text <> " " <> toText input)
      errorCheckpoint
      dictionary <- Enter.fragment fragment commonDictionary
      errorCheckpoint
      pure dictionary
    reportAll reports
    removeFile "interactive.mlt"
    case result of
      Nothing -> pure False
      Just dictionary -> do
        contents <- Erlang.generate dictionary (Just entryName)
        writeFileText "mlatu.erl" contents
        runProcess_ "escript mlatu.erl"
        pure True
  when update $ put (text <> " " <> toText input, lineNumber + 1)

-- TODO
completer :: String -> ReaderT Dictionary (StateT (Text, Int) IO) [String]
completer _ = pure []

helpCmd :: String -> MRepl ()
helpCmd s = liftIO $ case words (toText s) of
  ["help"] -> putStrLn helpHelp
  _ -> traverse_ putStrLn [helpHelp]
  where
    helpHelp = ":help - Show this help."

opts :: [(String, String -> MRepl ())]
opts = [("help", helpCmd)]

ini :: MRepl ()
ini = liftIO $ putStrLn "Welcome!"

final :: MRepl ExitDecision
final = liftIO (putStrLn "Bye!") >> pure Exit

run :: Prelude -> IO Int
run prelude = do
  (result, reports) <- runMlatu $ Mlatu.compilePrelude prelude [QualifiedName $ Global "io"] Nothing
  reportAll reports
  case result of
    Nothing -> pure 1
    Just commonDictionary -> do
      _ <- execStateT (runReaderT (evalReplOpts replOpts) commonDictionary) ("", 1)
      liftIO $ removeFile "mlatu.erl"
      pure 0
  where
    replOpts =
      ReplOpts
        { banner = \case
            SingleLine -> pure "> "
            MultiLine -> pure "| ",
          command = cmd,
          options = opts,
          prefix = Just ':',
          multilineCommand = Just "paste",
          tabComplete = Word completer,
          initialiser = ini,
          finaliser = final
        }
