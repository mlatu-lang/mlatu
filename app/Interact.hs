module Interact
  ( run,
  )
where

import Control.Monad.Catch (catch)
import Mlatu (Prelude (..), compilePrelude)
import Mlatu qualified
import Mlatu.Definition qualified as Definition
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Enter qualified as Enter
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Parameter (Parameter (..))
import Mlatu.Fragment qualified as Fragment
import Mlatu.Ice (ice)
import Mlatu.Infer (typeFromSignature, typecheck)
import Mlatu.Informer (errorCheckpoint, warnCheckpoint)
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Interpret (Failure, Rep, interpret, printRep)
import Mlatu.Kind (Kind (..))
import Mlatu.Monad (runMlatuExceptT)
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
import Mlatu.Vocabulary qualified as Vocabulary
import Optics
import Prettyprinter (vcat)
import Relude
import Report (reportAll)
import System.Console.Repline
import System.IO (hPrint)

type MRepl a = HaskelineT (StateT Dictionary (StateT [Rep] (StateT Int IO))) a

cmd :: String -> MRepl ()
cmd input = do
  dictionary <- lift get
  lineNumber <- lift $ lift $ lift get
  let entryNameUnqualified = toText $ "entry" ++ show lineNumber
      entryName =
        Qualified
          (Qualifier Absolute ["interactive"])
          $ Unqualified entryNameUnqualified
  mResults <- liftIO $
    runMlatuExceptT $ do
      -- Each entry gets its own definition in the dictionary, so it can
      -- be executed individually, and later conveniently referred to.
      fragment <-
        Mlatu.fragmentFromSource
          [QualifiedName $ Qualified Vocabulary.global "IO"]
          (Just entryName)
          lineNumber
          "<interactive>"
          (toText input)
      dictionary' <- Enter.fragment fragment dictionary
      _ <- warnCheckpoint
      callFragment <-
        Mlatu.fragmentFromSource
          [QualifiedName $ Qualified Vocabulary.global "IO"]
          Nothing
          lineNumber
          "<interactive>"
          -- TODO: Avoid stringly typing.
          (show $ printQualified entryName)
      dictionary'' <- Enter.fragment callFragment dictionary'
      warnCheckpoint
      let tenv = TypeEnv.empty
          mainBody = case Dictionary.lookup
            (Instantiated Definition.mainName [])
            dictionary'' of
            Just (Entry.Word _ _ _ _ _ (Just body)) ->
              body
            _noEntryPoint -> ice "Interact.run - cannot get entry point"
      let currentOrigin = Origin.point "<interactive>" lineNumber 1
      stackScheme <-
        typeFromSignature tenv $
          Signature.Quantified
            [ Parameter currentOrigin "R" Stack Nothing,
              Parameter currentOrigin "E" Permission Nothing
            ]
            ( Signature.StackFunction
                (Signature.Bottom currentOrigin)
                []
                (Signature.Variable "R" currentOrigin)
                []
                ["E"]
                currentOrigin
            )
            currentOrigin
      -- Checking that the main definition is able to operate on an
      -- empty stack is the same as verifying the last entry against the
      -- current stack state, as long as the state was modified
      -- correctly by the interpreter.
      _ <- Unify.typ tenv stackScheme (Term.typ mainBody)
      warnCheckpoint
      pure (dictionary'', mainBody)
  case mResults of
    Left reports -> do
      liftIO $ reportAll reports
    Right (dictionary', mainBody) -> do
      lift $ put dictionary
      lift $ lift $ lift $ modify' (+ 1)
      -- HACK: Get the last entry from the main body so we have the
      -- right generic args.
      let lastEntry = viaNonEmpty last (Term.decompose mainBody)
      case lastEntry of
        Just (Term.Word _ _ args _) ->
          ( do
              oldStack <- lift $ lift get
              stack <-
                liftIO $
                  interpret
                    dictionary'
                    (Just entryName)
                    args
                    stdin
                    stdout
                    stderr
                    oldStack
              lift $ lift $ put stack
              liftIO $ renderStack stack
          )
            `catch` (\e -> liftIO $ hPrint stderr (e :: Failure))
        _noArgs -> error $ show lastEntry

completer :: String -> StateT Dictionary (StateT [Rep] (StateT Int IO)) [String]
completer n = do
  dictionary <- get
  let dictNames = show . fst <$> Dictionary.toList dictionary
  pure $ filter (\dictName -> n `isPrefixOf` dictName) dictNames

helpCmd :: String -> MRepl ()
helpCmd s = liftIO $ case words (toText s) of
  ["help"] -> putStrLn helpHelp
  ["stack"] -> putStrLn stackHelp
  ["dict"] -> putStrLn dictHelp
  ["type"] -> putStrLn typeHelp
  _ -> traverse_ putStrLn [dictHelp, stackHelp, helpHelp]
  where
    helpHelp = ":help - Show this help."
    stackHelp = ":stack - Show the current state of the stack."
    dictHelp = ":dict - Show the current state of the dictionary."
    typeHelp = ":type - Show the type of an expression."

stackCmd :: String -> MRepl ()
stackCmd =
  const $
    lift (lift get)
      >>= (liftIO . renderStack)

dictCmd :: String -> MRepl ()
dictCmd =
  const $
    lift get
      >>= (liftIO . renderDictionary)

typeCmd :: String -> MRepl ()
typeCmd expression = do
  dictionary <- lift get
  lineNumber <- lift $ lift $ lift get
  mResults <- liftIO $
    runMlatuExceptT $ do
      fragment <-
        Mlatu.fragmentFromSource
          [QualifiedName $ Qualified Vocabulary.global "IO"]
          Nothing
          lineNumber
          "<interactive>"
          (toText expression)
      errorCheckpoint
      case view Fragment.definitions fragment of
        [main] | view Definition.name main == Definition.mainName -> do
          resolved <- Enter.resolveAndDesugar dictionary main
          errorCheckpoint
          (_, typ) <- typecheck dictionary Nothing $ view Definition.body resolved
          errorCheckpoint
          pure (Just typ)
        _otherDefinition -> pure Nothing

  liftIO $ case mResults of
    Left reports -> reportAll reports
    Right (Just typ) -> print $ printType typ
    Right Nothing -> hPrint stderr ("That doesn't look like an expression" :: String)

opts :: [(String, String -> MRepl ())]
opts = [("help", helpCmd), ("stack", stackCmd), ("dict", dictCmd), ("type", typeCmd)]

ini :: MRepl ()
ini = liftIO $ putStrLn "Welcome!"

final :: MRepl ExitDecision
final = do
  liftIO $ putStrLn "Bye!"
  pure Exit

run :: Prelude -> IO Int
run prelude = do
  mResult <- runMlatuExceptT $ compilePrelude prelude [QualifiedName $ Qualified Vocabulary.global "IO"] Nothing
  case mResult of
    Left reports -> do
      reportAll reports
      exitFailure
    Right commonDictionary ->
      execStateT (execStateT (execStateT (evalReplOpts replOpts) commonDictionary) []) 1
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

renderStack :: [Rep] -> IO ()
renderStack stack = unless (null stack) (print $ vcat $ printRep <$> stack)

renderDictionary :: Dictionary -> IO ()
renderDictionary = print . Dictionary.printDictionary
