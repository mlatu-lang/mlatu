{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Interpret
-- Description : Simple interpreter
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Interpret
  ( Failure,
    Rep (..),
    interpret,
    printRep,
  )
where

import Control.Exception (catch, throwIO)
import Data.Text qualified as Text
import Mlatu.Definition (mainName)
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Entry qualified as Entry
import Mlatu.Ice (ice)
import Mlatu.Instantiate qualified as Instantiate
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Monad (runMlatuExceptT)
import Mlatu.Name
  ( ClosureIndex (ClosureIndex),
    ConstructorIndex (..),
    GeneralName (QualifiedName),
    LocalIndex (LocalIndex),
    Qualified (Qualified),
    Unqualified,
  )
import Mlatu.Pretty qualified as Pretty
import Mlatu.Report qualified as Report
import Mlatu.Stack (Stack ((:::)))
import Mlatu.Stack qualified as Stack
import Mlatu.Term (Case (..), Else (..), Term (..), Value, defaultElseBody)
import Mlatu.Term qualified as Term
import Mlatu.Type (Type (..))
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Vocabulary qualified as Vocabulary
import Optics
import Prettyprinter (Doc, Pretty (pretty), dquotes, hcat, hsep, nest, squotes, vcat)
import Relude hiding (Compose, Type, callStack)
import Relude.Unsafe qualified as Unsafe
import System.Exit (ExitCode (..))
import System.IO (hFlush, hGetLine, hPrint, hPutStr)
import System.IO.Error (ioeGetErrorType)
import Text.Show qualified

-- | Representation of a runtime value.
data Rep
  = Algebraic !ConstructorIndex ![Rep]
  | Character !Char
  | Closure !Qualified ![Rep]
  | Name !Qualified
  | Text !Text
  deriving (Eq, Show, Generic)

makePrisms ''Rep

repInt :: Int -> Rep
repInt 0 = Algebraic (ConstructorIndex 0) []
repInt n = Algebraic (ConstructorIndex 1) [repInt (n - 1)]

intRep :: Rep -> Int
intRep (Algebraic (ConstructorIndex 0) []) = 0
intRep (Algebraic (ConstructorIndex 1) [x]) = 1 + intRep x

repList :: [Rep] -> Rep
repList [] = Algebraic (ConstructorIndex 0) []
repList (x : xs) = Algebraic (ConstructorIndex 1) [x, repList xs]

listRep :: Rep -> [Rep]
listRep (Algebraic (ConstructorIndex 0) []) = []
listRep (Algebraic (ConstructorIndex 1) [x, xs]) = x : listRep xs

valueRep :: (Show a) => Value a -> Rep
valueRep (Term.Character c) = Character c
valueRep (Term.Name name) = Name name
valueRep (Term.Text text) = Text text
valueRep value = ice $ "Mlatu.Interpret.valueRep - cannot convert value to rep: " ++ show value

printRep :: Rep -> Doc a
printRep (Algebraic (ConstructorIndex index) values) =
  hsep $
    (printRep <$> values) ++ [hcat ["#", pretty index]]
printRep (Character c) = squotes $ pretty c
printRep (Closure name closure) =
  hsep $
    (printRep <$> closure) ++ [hcat ["#", Pretty.printQualified name]]
printRep (Name n) = hcat ["\\", Pretty.printQualified n]
printRep (Text t) = dquotes $ pretty t

-- | Interprets a program dictionary.
interpret ::
  -- | The program.
  Dictionary ->
  -- | The name of the entry point, if overriding @main@.
  Maybe Qualified ->
  -- | Arguments passed to @main@.
  [Type] ->
  -- | Standard input handle.
  Handle ->
  -- | Standard output handle.
  Handle ->
  -- | Standard error handle.
  Handle ->
  -- | Initial stack state.
  [Rep] ->
  -- | Final stack state.
  IO [Rep]
interpret dictionary mName mainArgs stdin' stdout' _stderr' initialStack = do
  -- TODO: Types.
  stackRef <- newIORef $ Stack.fromList initialStack
  localsRef <- newIORef []
  currentClosureRef <- newIORef []
  let word :: [Qualified] -> Qualified -> [Type] -> IO ()
      word callStack name args = do
        let mangled = Instantiated name args
        case Dictionary.lookupWord mangled dictionary of
          -- An entry in the dictionary should already be instantiated, so we
          -- shouldn't need to instantiate it again here.
          Just (Entry.WordEntry _ _ _ _ _ (Just body)) -> term (name : callStack) body
          _ -> do
            case Dictionary.lookupWord (Instantiated name []) dictionary of
              -- A regular word.
              Just (Entry.WordEntry _ _ _ _ _ (Just body)) -> do
                mBody' <- runMlatuExceptT $ Instantiate.term TypeEnv.empty body args
                case mBody' of
                  Right body' -> term (name : callStack) body'
                  Left reports ->
                    hPrint stdout' $
                      vcat $
                        hcat
                          [ "Could not instantiate generic word ",
                            dquotes $ Pretty.printQualified name,
                            ":"
                          ] :
                        (Report.human <$> reports)
              -- An intrinsic.
              Just (Entry.WordEntry _ _ _ _ _ Nothing) -> case name of
                Qualified v unqualified
                  | v == Vocabulary.intrinsic ->
                    intrinsic (name : callStack) unqualified
                _nonIntrinsic -> ice "Mlatu.Interpret.interpret - no such intrinsic"
              _ ->
                throwIO $
                  Failure $
                    hcat
                      [ "I can't find an instantiation of ",
                        dquotes $ Pretty.printQualified name,
                        ": ",
                        dquotes $ Pretty.printInstantiated mangled
                      ]

      term :: [Qualified] -> Term Type -> IO ()
      term callStack t = case t of
        Coercion {} -> pass
        Compose _ a b -> term callStack a >> term callStack b
        -- TODO: Verify that this is correct.
        Generic _name _ t' _ -> term callStack t'
        Group t' -> term callStack t'
        Lambda _ _name _ body _ -> do
          a ::: r <- readIORef stackRef
          ls <- readIORef localsRef
          writeIORef stackRef r
          writeIORef localsRef (a : ls)
          term callStack body
          modifyIORef' localsRef (Unsafe.fromJust . viaNonEmpty tail)
        Match _ _ cases else_ _ -> do
          -- We delay matching on the value here because it may not be an ADT at
          -- all. For example, "1 match else { 2 }" is perfectly valid,
          -- because we are matching on all (0) of Int64's constructors.
          x ::: r <- readIORef stackRef
          writeIORef stackRef r
          let go (Case (QualifiedName name) caseBody _ : _)
                -- FIXME: Embed this information during name resolution, rather than
                -- looking it up.
                | Just (Entry.WordEntry _ _ _ _ _ (Just ctorBody)) <-
                    Dictionary.lookupWord (Instantiated name []) dictionary,
                  [New _ (ConstructorIndex index') _ _ _] <- Term.decompose ctorBody,
                  Algebraic (ConstructorIndex index) fields <- x,
                  index == index' =
                  do
                    writeIORef stackRef $ Stack.pushes fields r
                    term callStack caseBody
              go (_ : rest) = go rest
              go [] = case else_ of
                DefaultElse m o -> term callStack (defaultElseBody m o)
                Else body _ -> term callStack body
          go cases
        New _ index size _ _ -> do
          r <- readIORef stackRef
          let (fields, r') = Stack.pops size r
          writeIORef stackRef $ Algebraic index fields ::: r'
        NewClosure _ size _ -> do
          r <- readIORef stackRef
          let (Name name : closure, r') = Stack.pops (size + 1) r
          writeIORef stackRef (Closure name (reverse closure) ::: r')
        Push _ value _ -> push value
        Word _ (QualifiedName name) args _ ->
          word callStack name args
        Word _ name _ _ ->
          ice $
            show $
              hsep
                ["Mlatu.Interpret.interpret - unresolved word name", Pretty.printGeneralName name]

      call :: [Qualified] -> IO ()
      call callStack = do
        Closure name closure ::: r <- readIORef stackRef
        writeIORef stackRef r
        modifyIORef' currentClosureRef (closure :)
        -- FIXME: Use right args.
        word (name : callStack) name []
        modifyIORef' currentClosureRef (Unsafe.fromJust . viaNonEmpty tail)

      push :: Value Type -> IO ()
      push value = case value of
        Term.Closed (ClosureIndex index) -> do
          (currentClosure : _) <- readIORef currentClosureRef
          modifyIORef' stackRef (Unsafe.fromJust (currentClosure !!? index) :::)
        Term.Local (LocalIndex index) -> do
          locals <- readIORef localsRef
          modifyIORef' stackRef (Unsafe.fromJust (locals !!? index) :::)
        Term.Text text ->
          modifyIORef'
            stackRef
            (Text text :::)
        _otherTerm -> modifyIORef' stackRef (valueRep value :::)

      intrinsic :: [Qualified] -> Unqualified -> IO ()
      intrinsic callStack name = case name of
        "abort" -> do
          Text txt ::: r <- readIORef stackRef
          writeIORef stackRef r
          throwIO $
            Failure $
              vcat $
                hsep
                  [ "Execution failure:",
                    pretty txt
                  ] :
                "Call stack:" :
                (nest 2 . Pretty.printQualified <$> callStack)
        "exit" -> do
          x ::: r <- readIORef stackRef
          let i = intRep x
          writeIORef stackRef r
          exitWith $
            if i == 0
              then ExitSuccess
              else ExitFailure $ fromIntegral i
        "call" -> call callStack
        "drop" -> modifyIORef' stackRef Stack.popNote
        "swap" -> do
          a ::: b ::: r <- readIORef stackRef
          writeIORef stackRef $ b ::: a ::: r
        "show-nat" -> do
          x ::: r <- readIORef stackRef
          let i = intRep x
          writeIORef stackRef (Text (show i) ::: r)
        "read-nat" -> do
          Text x ::: r <- readIORef stackRef
          let i = Unsafe.read $ toString x
          writeIORef stackRef (repInt i ::: r)
        "cmp-string" -> do
          Text b ::: Text a ::: r <- readIORef stackRef
          writeIORef stackRef r
          case compare a b of
            LT -> word callStack (Qualified Vocabulary.global "less") []
            GT -> word callStack (Qualified Vocabulary.global "more") []
            EQ -> word callStack (Qualified Vocabulary.global "equal") []
        "cmp-char" -> do
          Character b ::: Character a ::: r <- readIORef stackRef
          writeIORef stackRef r
          case compare a b of
            LT -> word callStack (Qualified Vocabulary.global "less") []
            GT -> word callStack (Qualified Vocabulary.global "more") []
            EQ -> word callStack (Qualified Vocabulary.global "equal") []
        "string-concat" -> do
          Text y ::: Text x ::: r <- readIORef stackRef
          writeIORef stackRef $ Text (Text.concat [x, y]) ::: r
        "string-from-list" -> do
          x ::: r <- readIORef stackRef
          let string = (\(Character c) -> c) <$> listRep x
          writeIORef stackRef $ Text (toText string) ::: r
        "string-to-list" -> do
          Text txt ::: r <- readIORef stackRef
          let string = repList (Character <$> toString txt)
          writeIORef stackRef $ string ::: r
        "print" -> do
          Text txt ::: r <- readIORef stackRef
          writeIORef stackRef r
          hPutStr stdout' $ toString txt
        "get-line" -> do
          line <- hGetLine stdin'
          modifyIORef' stackRef ((Text $ toText line) :::)
        "flush_stdout" -> hFlush stdout'
        "read-file" -> do
          Text txt ::: r <- readIORef stackRef
          contents <-
            catchFileAccessErrors $
              readFile $ toString txt
          writeIORef stackRef $ Text (toText contents) ::: r
        "write-file" -> do
          Text cs ::: Text bs ::: r <- readIORef stackRef
          writeIORef stackRef r
          catchFileAccessErrors $
            writeFile (toString cs) $
              toString bs
        "append-file" -> do
          Text cs ::: Text bs ::: r <- readIORef stackRef
          writeIORef stackRef r
          catchFileAccessErrors $
            appendFile (toString cs) $
              toString bs
        _nonIntrinsic -> ice "Mlatu.Interpret.interpret - no such intrinsic"
        where
          catchFileAccessErrors :: IO a -> IO a
          catchFileAccessErrors action =
            action `catch` \e ->
              throwIO $
                Failure $
                  vcat $
                    "Execution failure:" :
                    show (ioeGetErrorType e) :
                    "Call stack:" :
                    (nest 2 . Pretty.printQualified <$> callStack)

  let entryPointName = fromMaybe mainName mName
  word [entryPointName] entryPointName mainArgs
  toList <$> readIORef stackRef

newtype Failure = Failure (Doc ())
  deriving (Typeable)

instance Exception Failure

instance Show Failure where
  show (Failure message) = show message
