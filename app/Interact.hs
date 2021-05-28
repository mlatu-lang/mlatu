module Interact
  ( run,
  )
where

import Mlatu (Prelude (..), compilePrelude)
import Mlatu qualified
import Mlatu.Codegen qualified as Codegen
import Mlatu.Dictionary (Dictionary)
import Mlatu.Enter qualified as Enter
import Mlatu.Infer (typeFromSignature, typecheck)
import Mlatu.Informer (errorCheckpoint, ice, runMlatu, warnCheckpoint)
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Kind (Kind (..))
import Mlatu.Name
  ( GeneralName (QualifiedName),
    Qualified (Qualified),
    Qualifier (Qualifier),
    Root (Absolute),
    Unqualified (Unqualified),
  )
import Mlatu.Origin qualified as Origin
import Mlatu.Pretty (printQualified, printType)
import Mlatu.Signature qualified as Signature
import Mlatu.Term qualified as Term
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Unify qualified as Unify
import Mlatu.Vocabulary
import Optics
import Prettyprinter (vcat)
import Relude
import Report (reportAll)
import System.Console.Repline
import System.Directory (createDirectory, removeDirectoryRecursive, withCurrentDirectory)
import System.IO (hPrint)
import System.Process.Typed (proc, runProcess_)

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
  mResults <- liftIO $
    runMlatu $ do
      fragment <-
        Mlatu.fragmentFromSource
          [QualifiedName $ Global "io"]
          (Just entryName)
          lineNumber
          "<interactive>"
          (text <> " " <> toText input)
      errorCheckpoint
      dictionary <- Enter.fragment fragment commonDictionary
      errorCheckpoint
      pure dictionary
  case mResults of
    Left reports -> do
      liftIO $ reportAll reports
    Right dictionary -> do
      put (text <> " " <> toText input, lineNumber + 1)
      liftIO
        ( Codegen.generate dictionary (Just entryName) >>= \contents ->
            writeFileBS "t/src/main.rs" contents
              >> withCurrentDirectory
                "t"
                ( runProcess_
                    (proc "cargo" ["+nightly", "run", "--quiet"])
                )
        )

-- TODO
completer :: String -> ReaderT Dictionary (StateT (Text, Int) IO) [String]
completer n = pure []

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
final =
  liftIO (putStrLn "Bye!" >> removeDirectoryRecursive "t")
    >> pure Exit

run :: Prelude -> IO Int
run prelude = do
  result <- runMlatu $ Mlatu.compilePrelude prelude [QualifiedName $ Global "io"] Nothing
  case result of
    Left reports -> do
      reportAll reports
      pure 1
    Right commonDictionary -> do
      liftIO $ do
        createDirectory "t"
        writeFileBS "t/Cargo.toml" cargoToml
        createDirectory "t/src"
      execStateT (runReaderT (evalReplOpts replOpts) commonDictionary) ("", 1)
      pure 0
  where
    cargoToml =
      "[package] \n \
      \ name = \"output\" \n \
      \ version = \"0.1.0\" "
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
