module Interact
  ( run,
  )
where

import Control.Exception (catch)
import Data.List (foldr1, partition, stripPrefix)
import Data.Text qualified as Text
import Mlatu (getCommonPaths, runMlatu)
import Mlatu qualified
import Mlatu.Definition qualified as Definition
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Enter qualified as Enter
import Mlatu.Entry (Entry)
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Parameter (Parameter (..))
import Mlatu.Fragment qualified as Fragment
import Mlatu.Infer (typeFromSignature, typecheck)
import Mlatu.Informer (errorCheckpoint)
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Instantiated qualified as Instantiated
import Mlatu.Interpret (Failure, interpret)
import Mlatu.Kind (Kind (..))
import Mlatu.Name
  ( GeneralName (LocalName, QualifiedName),
    Qualified (Qualified),
    Qualifier (Qualifier),
    Root (Absolute),
    Unqualified (Unqualified),
  )
import Mlatu.Name qualified as Name
import Mlatu.Origin qualified as Origin
import Mlatu.Parse qualified as Parse
import Mlatu.Pretty qualified as Pretty
import Mlatu.Report qualified as Report
import Mlatu.Resolve qualified as Resolve
import Mlatu.Signature qualified as Signature
import Mlatu.Term qualified as Term
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Unify qualified as Unify
import Mlatu.Vocabulary qualified as Vocabulary
import Paths_Mlatu (getDataDir)
import Relude
import Relude.Extra (next)
import Relude.Extra.Enum (prev)
import Report (reportAll)
import System.Console.Haskeline
  ( Completion (..),
    CompletionFunc,
    InputT,
    Settings (autoAddHistory, historyFile),
    completeWord,
    defaultSettings,
    getInputLine,
    runInputT,
    setComplete,
  )
import System.IO (hPrint, hPutStrLn)
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Text.Printf (printf)

run :: IO ()
run = do
  commonPaths <- getCommonPaths getDataDir
  commonDictionary <- runMlatu $ Mlatu.compile [QualifiedName $ Qualified Vocabulary.global "IO"] Nothing commonPaths
  dictionaryRef <-
    newIORef =<< case commonDictionary of
      Left reports -> do
        reportAll reports
        exitFailure
      Right result -> return result
  lineNumberRef <- newIORef (1 :: Int)
  stackRef <- newIORef []

  welcome
  runInputT (settings dictionaryRef) $
    let loop :: InputT IO ()
        loop = do
          lineNumber <- liftIO $ readIORef lineNumberRef
          let currentOrigin = Origin.point "<interactive>" lineNumber 1
          mLine <- runMaybeT $ getEntry lineNumber
          case mLine of
            Nothing -> liftIO $ putStrLn "" >> bye
            Just (line, lineNumber') -> case line of
              "//dict" -> do
                liftIO $ renderDictionary dictionaryRef
                loop
              "//help" -> do
                liftIO showHelp
                loop
              "//stack" -> do
                liftIO $ renderStack stackRef
                loop
              "//quit" -> liftIO bye
              -- Commands with arguments.
              _
                | "//" `Text.isPrefixOf` line ->
                  case Text.break (== ' ') $ Text.drop 2 line of
                    ("info", name) -> nameCommand lineNumber dictionaryRef name loop $
                      \_name' entry -> liftIO $ putStrLn $ Pretty.render $ pPrint entry
                    ("list", name) -> nameCommand lineNumber dictionaryRef name loop $
                      \name' entry -> case entry of
                        Entry.Word _ _ _ _ _ (Just body) ->
                          liftIO $ putStrLn $ Pretty.render $ pPrint body
                        _noBody ->
                          liftIO $
                            hPutStrLn stderr $
                              Pretty.render $
                                Pretty.hsep
                                  [ "I can't find a word entry called",
                                    Pretty.quote name',
                                    "with a body to list"
                                  ]
                    ("type", expression) -> do
                      dictionary <- liftIO $ readIORef dictionaryRef
                      mResults <- liftIO $
                        runMlatu $ do
                          fragment <-
                            Mlatu.fragmentFromSource
                              [QualifiedName $ Qualified Vocabulary.global "IO"]
                              Nothing
                              lineNumber
                              "<interactive>"
                              expression
                          errorCheckpoint
                          case Fragment.definitions fragment of
                            [main] | Definition.name main == Definition.mainName -> do
                              resolved <- Enter.resolveAndDesugar dictionary main
                              errorCheckpoint
                              (_, typ) <-
                                typecheck dictionary Nothing $
                                  Definition.body resolved
                              errorCheckpoint
                              return (Just typ)
                            _otherDefinition -> return Nothing

                      liftIO $ case mResults of
                        Left reports -> reportAll reports
                        Right (Just typ) -> putStrLn $ Pretty.render $ pPrint typ
                        Right Nothing ->
                          hPutStrLn stderr $
                            Pretty.render $
                              Pretty.hsep
                                [ "That doesn't look like an expression"
                                ]
                      loop
                    ("debug", expression) -> do
                      dictionary <- liftIO $ readIORef dictionaryRef
                      mResults <- liftIO $
                        runMlatu $ do
                          fragment <-
                            Mlatu.fragmentFromSource
                              [QualifiedName $ Qualified Vocabulary.global "IO"]
                              Nothing
                              lineNumber
                              "<interactive>"
                              expression
                          errorCheckpoint
                          case Fragment.definitions fragment of
                            [main] | Definition.name main == Definition.mainName -> do
                              resolved <- Enter.resolveAndDesugar dictionary main
                              errorCheckpoint
                              return $ Just $ Definition.body resolved
                            _otherDefinition -> return Nothing

                      liftIO $ case mResults of
                        Left reports -> reportAll reports
                        Right (Just to_show) -> print to_show
                        Right Nothing ->
                          hPutStrLn stderr $
                            Pretty.render $
                              Pretty.hsep
                                [ "That doesn't look like an expression"
                                ]
                      loop
                    (command, _) -> do
                      liftIO $
                        hPutStrLn stderr $
                          Pretty.render $
                            Pretty.hsep
                              [ "I don't know the command",
                                Pretty.quotes $ Pretty.text $ toString command
                              ]
                      loop

              -- Mlatu code.
              _ -> do
                dictionary <- liftIO $ readIORef dictionaryRef
                let entryNameUnqualified = toText $ "entry" ++ show lineNumber
                    entryName =
                      Qualified
                        (Qualifier Absolute ["interactive"])
                        $ Unqualified entryNameUnqualified
                mResults <- liftIO $
                  runMlatu $ do
                    -- Each entry gets its own definition in the dictionary, so it can
                    -- be executed individually, and later conveniently referred to.
                    fragment <-
                      Mlatu.fragmentFromSource
                        [QualifiedName $ Qualified Vocabulary.global "IO"]
                        (Just entryName)
                        lineNumber
                        "<interactive>"
                        line
                    dictionary' <- Enter.fragment fragment dictionary
                    _ <- errorCheckpoint
                    callFragment <-
                      Mlatu.fragmentFromSource
                        [QualifiedName $ Qualified Vocabulary.global "IO"]
                        Nothing
                        lineNumber
                        "<interactive>"
                        -- TODO: Avoid stringly typing.
                        (toText $ Pretty.render $ pPrint entryName)
                    dictionary'' <- Enter.fragment callFragment dictionary'
                    errorCheckpoint
                    let tenv = TypeEnv.empty
                        mainBody = case Dictionary.lookup
                          (Instantiated Definition.mainName [])
                          dictionary'' of
                          Just (Entry.Word _ _ _ _ _ (Just body)) ->
                            body
                          _noEntryPoint -> error "cannot get entry point"
                    stackScheme <-
                      typeFromSignature tenv $
                        Signature.Quantified
                          [ Parameter currentOrigin "R" Stack,
                            Parameter currentOrigin "E" Permission
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
                    errorCheckpoint
                    return (dictionary'', mainBody)
                case mResults of
                  Left reports -> do
                    liftIO $ reportAll reports
                    loop
                  Right (dictionary', mainBody) -> do
                    liftIO $ do
                      writeIORef dictionaryRef dictionary'
                      writeIORef lineNumberRef lineNumber'
                    -- HACK: Get the last entry from the main body so we have the
                    -- right generic args.
                    let lastEntry = viaNonEmpty last (Term.decompose mainBody)
                    case lastEntry of
                      Just (Term.Word _ _ _ args _) -> liftIO $
                        catch
                          ( do
                              stack <-
                                interpret
                                  dictionary'
                                  (Just entryName)
                                  args
                                  stdin
                                  stdout
                                  stderr
                                  =<< readIORef stackRef
                              writeIORef stackRef stack
                              renderStack stackRef
                          )
                          $ \e -> hPrint stderr (e :: Failure)
                      _noArgs -> error $ show lastEntry
                    loop
     in loop
  where
    welcome = putStrLn "Welcome to Mlatu! Type //help for help or //quit to quit"
    bye = do
      putStrLn "Bye!"
      pass

settings :: IORef Dictionary -> Settings IO
settings dictionaryRef =
  setComplete (completer dictionaryRef) $
    defaultSettings
      { autoAddHistory = True,
        historyFile = Nothing
      }

completer :: IORef Dictionary -> CompletionFunc IO
completer = completeWord Nothing "\t \"{}[]()\\" . completePrefix

completePrefix :: IORef Dictionary -> String -> IO [Completion]
completePrefix dictionaryRef prefix
  | Just rest <- Text.stripPrefix "//" (toText prefix) =
    let -- TODO: Factor out commands to a central location.
        matching =
          filter
            (rest `Text.isPrefixOf`)
            ["dict", "help", "info", "list", "quit", "stack", "type"]
        hasParams = [["info"], ["list"], ["type"]]
     in return $
          map
            ( toCompletion (small matching && matching `elem` hasParams)
                . toString
                . ("//" <>)
            )
            matching
  | otherwise = do
    dictionary <- readIORef dictionaryRef
    let matching =
          filter (prefix `isPrefixOf`) $
            map (completionFromName . fst) $ Dictionary.toList dictionary
    return $ map (toCompletion (small matching)) matching
  where
    completionFromName
      (Instantiated (Qualified (Qualifier _ parts) (Unqualified name)) _) =
        toString $ Text.intercalate "::" $ parts ++ [name]

small :: [a] -> Bool
small [] = True
small [_] = True
small _ = False

toCompletion :: Bool -> String -> Completion
toCompletion finished name =
  Completion
    { replacement = name,
      display = name,
      isFinished = finished
    }

renderDictionary :: IORef Dictionary -> IO ()
renderDictionary dictionaryRef = do
  names <-
    sort . map (Name.toParts . Instantiated.name . fst) . Dictionary.toList
      <$> readIORef dictionaryRef
  let loop :: Int -> [[Text]] -> IO ()
      loop depth acc = case foldr0 commonPrefix [] acc of
        [] -> mapM_ (putStrLn . prettyName depth) acc
        prefix ->
          let stripped = mapMaybe (stripPrefix prefix) acc
              (leaves, branches) = partition ((== 1) . length) stripped
           in do
                putStrLn $ prettyName depth prefix
                loop (depth + 4) branches
                mapM_ (putStrLn . prettyName (depth + 4)) leaves
  loop 0 names
  where
    -- TODO: Don't rely on name of global vocabulary.
    prettyName depth =
      Pretty.render . Pretty.nest depth . Pretty.text
        . toString
        . Text.intercalate "::"
        . (\x -> if x == [""] then ["_"] else x)

foldr0 :: (a -> a -> a) -> a -> [a] -> a
foldr0 _ x [] = x
foldr0 f _ xs = foldr1 f xs

commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix (x : xs) (y : ys)
  | x == y = x : commonPrefix xs ys
  | otherwise = []
commonPrefix xs@[] _ = xs
commonPrefix _ ys@[] = ys

nameCommand ::
  Int ->
  IORef Dictionary ->
  Text ->
  InputT IO () ->
  (Qualified -> Entry -> InputT IO ()) ->
  InputT IO ()
nameCommand lineNumber dictionaryRef name loop action = do
  result <-
    runMlatu $
      Parse.generalName
        lineNumber
        "<interactive>"
        name
  let currentOrigin = Origin.point "<interactive>" lineNumber 1
  case result of
    Right unresolved -> do
      dictionary <- liftIO $ readIORef dictionaryRef
      mResolved <-
        liftIO $
          runMlatu $
            Resolve.run $
              Resolve.generalName
                -- TODO: Use 'WordOrTypeName' or something as the category.
                Report.WordName
                (\_ index -> return $ LocalName index)
                (\name' -> Instantiated name' [] `Dictionary.member` dictionary)
                -- TODO: Keep a notion of current vocabulary?
                Vocabulary.global
                unresolved
                -- TODO: Get this from the parser.
                currentOrigin
      case mResolved of
        Left reports -> do
          liftIO $ reportAll reports
          loop
        Right (QualifiedName resolved)
          | Just entry <-
              Dictionary.lookup
                (Instantiated resolved [])
                dictionary ->
            do
              action resolved entry
              loop
        Right resolved -> do
          liftIO $
            hPutStrLn stderr $
              Pretty.render $
                Pretty.hsep
                  [ "I can't find an entry in the dictionary for",
                    Pretty.quote resolved
                  ]
          loop
    Left reports -> do
      liftIO $ reportAll reports
      loop

renderStack :: (Pretty a) => IORef [a] -> IO ()
renderStack stackRef = do
  stack <- readIORef stackRef
  case stack of
    [] -> pass
    _list -> putStrLn $ Pretty.render $ Pretty.vcat $ map pPrint stack

showHelp :: IO ()
showHelp =
  putStrLn
    "\
    \\n\
    \//help         - Show this help.\n\
    \//quit         - Quit interactive mode.\n\
    \\n\
    \//dict         - Show the contents of the dictionary.\n\
    \//info <name>  - Show information about <name>.\n\
    \//list <name>  - Show the desugared source of <name>.\n\
    \//stack        - Show the state of the stack.\n\
    \//type <expr>  - Show the type of some expression <expr>.\n\
    \\&"

data InString = Inside | Outside

getEntry :: Int -> MaybeT (InputT IO) (Text, Int)
getEntry lineNumber0 = do
  line <- MaybeT $ getInputLine $ printf "\n% 4d: " lineNumber0
  (result, lineNumber') <- check lineNumber0 line Nothing
  return (toText result, lineNumber' + 1)
  where
    check :: Int -> String -> Maybe String -> MaybeT (InputT IO) (String, Int)
    check lineNumber line acc
      | matched acc' = return (acc', lineNumber)
      | otherwise = continue (next lineNumber) $ Just acc'
      where
        acc' = case acc of
          Just previous -> concat [previous, "\n", line]
          Nothing -> line

    continue :: Int -> Maybe String -> MaybeT (InputT IO) (String, Int)
    continue lineNumber acc = do
      line <- MaybeT $ getInputLine $ printf "\n% 4d| " lineNumber
      check lineNumber line acc

    matched :: String -> Bool
    matched = go Outside (0 :: Int)
      where
        go :: InString -> Int -> String -> Bool
        go q n ('\\' : x : xs)
          | x `elem` ("'\"" :: String) = go q n xs
          | otherwise = go q n xs
        go q n ('"' : xs) = go (case q of Inside -> Outside; Outside -> Inside) n xs
        go Inside n (_ : xs) = go Inside n xs
        go Inside _ [] = True
        go Outside n (x : xs)
          | isOpen x = go Outside (next n) xs
          | isClose x = n <= 0 || go Outside (prev n) xs
          | otherwise = go Outside n xs
        go Outside n [] = n == 0
        isOpen = (`elem` ("([{" :: String))
        isClose = (`elem` ("}])" :: String))
