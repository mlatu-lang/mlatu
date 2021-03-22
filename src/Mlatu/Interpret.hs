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

import Control.Exception (ArithException (..), catch, throwIO)
import Data.Bits
  ( Bits (complement, (.&.), (.|.)),
  )
import Data.Fixed (mod')
import Data.Text qualified as Text
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import Mlatu.Definition (mainName)
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Entry qualified as Entry
import Mlatu.Ice (ice)
import Mlatu.Instantiate qualified as Instantiate
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Literal qualified as Literal
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
import Numeric (log)
import Optics
import Prettyprinter (Doc, Pretty (pretty), dquotes, hcat, hsep, list, nest, squotes, vcat)
import Relude hiding (Compose, Type, callStack)
import Relude.Unsafe qualified as Unsafe
import System.Exit (ExitCode (..))
import System.IO (hFlush, hGetLine, hPrint, hPutStr, readIO)
import System.IO.Error (IOError, ioeGetErrorType)
import Text.Show qualified

-- | Representation of a runtime value.
data Rep
  = Algebraic !ConstructorIndex ![Rep]
  | Array !(Vector Rep)
  | Character !Char
  | Closure !Qualified ![Rep]
  | Float64 !Double
  | Int64 !Int64
  | Name !Qualified
  | Text !Text
  deriving (Eq, Show, Generic)

makePrisms ''Rep

valueRep :: (Show a) => Value a -> Rep
valueRep (Term.Character c) = Character c
valueRep (Term.Float literal) = Float64 $ Literal.floatValue literal
valueRep (Term.Integer literal) = Int64 $ fromInteger $ view Literal.integerValue literal
valueRep (Term.Name name) = Name name
valueRep (Term.Text text) = Text text
valueRep value = ice $ "Mlatu.Interpret.valueRep - cannot convert value to rep: " ++ show value

printRep :: Rep -> Doc a
printRep (Algebraic (ConstructorIndex index) values) =
  hsep $
    fmap printRep values ++ [hcat ["#", pretty index]]
printRep (Array values) =
  list $
    Vector.toList $ fmap printRep values
printRep (Character c) = squotes $ pretty c
printRep (Closure name closure) =
  hsep $
    fmap printRep closure ++ [hcat ["#", Pretty.printQualified name]]
printRep (Float64 f) = pretty f
printRep (Int64 i) = pretty i
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
        case Dictionary.lookup mangled dictionary of
          -- An entry in the dictionary should already be instantiated, so we
          -- shouldn't need to instantiate it again here.
          Just (Entry.Word _ _ _ _ _ (Just body)) -> term (name : callStack) body
          _noBody -> case Dictionary.lookup (Instantiated name []) dictionary of
            -- A regular word.
            Just (Entry.Word _ _ _ _ _ (Just body)) -> do
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
                      fmap Report.human reports
            -- An intrinsic.
            Just (Entry.Word _ _ _ _ _ Nothing) -> case name of
              Qualified v unqualified
                | v == Vocabulary.intrinsic ->
                  intrinsic (name : callStack) unqualified
              _nonIntrinsic -> ice "Mlatu.Interpret.interpret - no such intrinsic"
            _noInstantiation ->
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
                | Just (Entry.Word _ _ _ _ _ (Just ctorBody)) <-
                    Dictionary.lookup (Instantiated name []) dictionary,
                  [New _ (ConstructorIndex index') _ _] <- Term.decompose ctorBody,
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
        New _ index size _ -> do
          r <- readIORef stackRef
          let (fields, r') = Stack.pops size r
          writeIORef stackRef $ Algebraic index fields ::: r'
        NewClosure _ size _ -> do
          r <- readIORef stackRef
          let (Name name : closure, r') = Stack.pops (size + 1) r
          writeIORef stackRef (Closure name (reverse closure) ::: r')
        NewVector _ size _ _ -> do
          r <- readIORef stackRef
          let (values, r') = Stack.pops size r
          writeIORef stackRef $
            Array (Vector.reverse $ Vector.fromList values) ::: r'
        Push _ value _ -> push value
        Word _ _ (QualifiedName name) args _ ->
          word callStack name args
        Word _ _ name _ _ ->
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
                fmap (nest 2 . Pretty.printQualified) callStack
        "exit" -> do
          Int64 i ::: r <- readIORef stackRef
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
        "add_int64" -> binaryInt64 (+)
        "sub_int64" -> binaryInt64 (-)
        "mul_int64" -> binaryInt64 (*)
        "div_int64" -> catchDivideByZero $ binaryInt64 div
        "mod_int64" -> catchDivideByZero $ binaryInt64 mod
        "not_int64" -> unaryInt64 complement
        "or_int64" -> binaryInt64 (.|.)
        "and_int64" -> binaryInt64 (.&.)
        "xor_int64" -> binaryInt64 xor
        "gt_int64" -> boolInt64 (>)
        "eq_int64" -> boolInt64 (==)
        "gt_char" -> boolChar (>)
        "eq_char" -> boolChar (==)
        "add_float64" -> binaryFloat64 (+)
        "sub_float64" -> binaryFloat64 (-)
        "mul_float64" -> binaryFloat64 (*)
        "div_float64" -> binaryFloat64 (/)
        "mod_float64" -> catchFloatModByZero $ binaryFloat64 mod'
        "exp_float64" -> unaryFloat64 exp
        "log_float64" -> unaryFloat64 log
        "sqrt_float64" -> unaryFloat64 sqrt
        "sin_float64" -> unaryFloat64 sin
        "cos_float64" -> unaryFloat64 cos
        "tan_float64" -> unaryFloat64 tan
        "asin_float64" -> unaryFloat64 asin
        "acos_float64" -> unaryFloat64 acos
        "atan_float64" -> unaryFloat64 atan
        "atan2_float64" -> binaryFloat64 atan2
        "sinh_float64" -> unaryFloat64 sinh
        "cosh_float64" -> unaryFloat64 cosh
        "tanh_float64" -> unaryFloat64 tanh
        "asinh_float64" -> unaryFloat64 asinh
        "acosh_float64" -> unaryFloat64 acosh
        "atanh_float64" -> unaryFloat64 atanh
        "trunc_float64" -> unaryFloat64 $ fromInteger . truncate
        "round_float64" -> unaryFloat64 $ fromInteger . round
        "ceil_float64" -> unaryFloat64 $ fromInteger . ceiling
        "floor_float64" -> unaryFloat64 $ fromInteger . floor
        "gt_float64" -> boolFloat64 (>)
        "eq_float64" -> boolFloat64 (==)
        "gt_string" -> boolString (>)
        "eq_string" -> boolString (==)
        "show_int64" -> showInteger (show @Text @Int64)
        "show_float64" -> showFloat (show :: Double -> Text)
        "read_int64" -> readInteger ((readIO :: String -> IO Int64) . toString) Int64
        "read_float64" -> readFloat ((readIO :: String -> IO Double) . toString) Float64
        "empty" -> do
          Array xs ::: r <- readIORef stackRef
          writeIORef stackRef r
          if Vector.null xs
            then word callStack (Qualified Vocabulary.global "true") []
            else word callStack (Qualified Vocabulary.global "false") []
        "head" -> do
          Array xs ::: r <- readIORef stackRef
          if Vector.null xs
            then do
              writeIORef stackRef r
              word callStack (Qualified Vocabulary.global "none") []
            else do
              let x = xs ! 0
              writeIORef stackRef $ x ::: r
              -- FIXME: Use right args.
              word callStack (Qualified Vocabulary.global "some") []
        "last" -> do
          Array xs ::: r <- readIORef stackRef
          if Vector.null xs
            then do
              writeIORef stackRef r
              word callStack (Qualified Vocabulary.global "none") []
            else do
              let x = xs ! (Vector.length xs - 1)
              writeIORef stackRef $ x ::: r
              -- FIXME: Use right args.
              word callStack (Qualified Vocabulary.global "some") []
        "cat" -> do
          Array ys ::: Array xs ::: r <- readIORef stackRef
          writeIORef stackRef $ Array (xs <> ys) ::: r
        "string_concat" -> do
          Text y ::: Text x ::: r <- readIORef stackRef
          writeIORef stackRef $ Text (Text.concat [x, y]) ::: r
        "string_from_list" -> do
          Array cs ::: r <- readIORef stackRef
          let string = (\(Character c) -> c) <$> Vector.toList cs
          writeIORef stackRef $ Text (toText string) ::: r
        "string_to_list" -> do
          Text txt ::: r <- readIORef stackRef
          let string = Array (Vector.fromList (Character <$> toString txt))
          writeIORef stackRef $ string ::: r
        "get" -> do
          Int64 i ::: Array xs ::: r <- readIORef stackRef
          if i < 0 || i >= fromIntegral (length xs)
            then do
              writeIORef stackRef r
              word callStack (Qualified Vocabulary.global "none") []
            else do
              writeIORef stackRef $ (xs ! fromIntegral i) ::: r
              -- FIXME: Use right args.
              word callStack (Qualified Vocabulary.global "some") []
        "set" -> do
          Int64 i ::: x ::: Array xs ::: r <- readIORef stackRef
          if i < 0 || i >= fromIntegral (length xs)
            then do
              writeIORef stackRef r
              word callStack (Qualified Vocabulary.global "none") []
            else do
              let (before, after) = Vector.splitAt (fromIntegral i) xs
              writeIORef stackRef $
                Array
                  (before <> Vector.singleton x <> Vector.tail after)
                  ::: r
              -- FIXME: Use right args.
              word callStack (Qualified Vocabulary.global "some") []
        "print" -> do
          Text txt ::: r <- readIORef stackRef
          writeIORef stackRef r
          hPutStr stdout' $ toString txt
        "get_line" -> do
          line <- hGetLine stdin'
          modifyIORef' stackRef ((Text $ toText line) :::)
        "flush_stdout" -> hFlush stdout'
        "read_file" -> do
          Text txt ::: r <- readIORef stackRef
          contents <-
            catchFileAccessErrors $
              readFile $ toString txt
          writeIORef stackRef $ Text (toText contents) ::: r
        "write_file" -> do
          Text cs ::: Text bs ::: r <- readIORef stackRef
          writeIORef stackRef r
          catchFileAccessErrors $
            writeFile (toString cs) $
              toString bs
        "append_file" -> do
          Text cs ::: Text bs ::: r <- readIORef stackRef
          writeIORef stackRef r
          catchFileAccessErrors $
            appendFile (toString cs) $
              toString bs
        "tail" -> do
          Array xs ::: r <- readIORef stackRef
          if Vector.null xs
            then do
              writeIORef stackRef r
              word callStack (Qualified Vocabulary.global "none") []
            else do
              let xs' = Vector.tail xs
              writeIORef stackRef $ Array xs' ::: r
              -- FIXME: Use right args.
              word callStack (Qualified Vocabulary.global "some") []
        "init" -> do
          Array xs ::: r <- readIORef stackRef
          if Vector.null xs
            then do
              writeIORef stackRef r
              word callStack (Qualified Vocabulary.global "none") []
            else do
              let xs' = Vector.init xs
              writeIORef stackRef $ Array xs' ::: r
              -- FIXME: Use right args.
              word callStack (Qualified Vocabulary.global "some") []
        _nonIntrinsic -> ice "Mlatu.Interpret.interpret - no such intrinsic"
        where
          unaryInt64 :: (Int64 -> Int64) -> IO ()
          unaryInt64 f = do
            Int64 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f $ fromIntegral x
            writeIORef stackRef $ Int64 result ::: r

          binaryInt64 :: (Int64 -> Int64 -> Int64) -> IO ()
          binaryInt64 f = do
            Int64 y ::: Int64 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Int64 result ::: r

          boolInt64 :: (Int64 -> Int64 -> Bool) -> IO ()
          boolInt64 f = do
            Int64 y ::: Int64 x ::: r <- readIORef stackRef
            let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          boolChar :: (Char -> Char -> Bool) -> IO ()
          boolChar f = do
            Character y ::: Character x ::: r <- readIORef stackRef
            let !result = fromEnum $ f x y
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          unaryFloat64 :: (Double -> Double) -> IO ()
          unaryFloat64 f = do
            Float64 x ::: r <- readIORef stackRef
            let !result = realToFrac $ f $ realToFrac x
            writeIORef stackRef $ Float64 result ::: r

          binaryFloat64 :: (Double -> Double -> Double) -> IO ()
          binaryFloat64 f = do
            Float64 y ::: Float64 x ::: r <- readIORef stackRef
            let !result = f x y
            writeIORef stackRef $ Float64 result ::: r

          boolFloat64 :: (Double -> Double -> Bool) -> IO ()
          boolFloat64 f = do
            Float64 y ::: Float64 x ::: r <- readIORef stackRef
            let !result = fromEnum $ f x y
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          boolString :: (Text -> Text -> Bool) -> IO ()
          boolString f = do
            Text y ::: Text x ::: r <- readIORef stackRef
            let !result = fromEnum $ f x y
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          showInteger :: (Int64 -> Text) -> IO ()
          showInteger f = modifyIORef' stackRef (\(Int64 n ::: r) -> Text (f n) ::: r)

          showFloat :: (Double -> Text) -> IO ()
          showFloat f = modifyIORef' stackRef (\(Float64 n ::: r) -> Text (f n) ::: r)

          readInteger :: Integral a => (Text -> IO a) -> (a -> Rep) -> IO ()
          readInteger f rep = do
            Text txt ::: r <- readIORef stackRef
            do
              result <- f txt
              writeIORef stackRef $ rep result ::: r
              -- FIXME: Use right args.
              word callStack (Qualified Vocabulary.global "some") []
              `catch` \(_ :: IOError) -> do
                writeIORef stackRef r
                word callStack (Qualified Vocabulary.global "none") []

          readFloat :: Real a => (Text -> IO a) -> (a -> Rep) -> IO ()
          readFloat f rep = do
            Text txt ::: r <- readIORef stackRef
            do
              result <- f txt
              writeIORef stackRef $ rep result ::: r
              -- FIXME: Use right args.
              word callStack (Qualified Vocabulary.global "some") []
              `catch` \(_ :: IOError) -> do
                writeIORef stackRef r
                word callStack (Qualified Vocabulary.global "none") []

          catchDivideByZero :: IO a -> IO a
          catchDivideByZero action =
            action `catch` \case
              DivideByZero ->
                throwIO $
                  Failure $
                    vcat $
                      "Execution failure: integer division by zero" :
                      "Call stack:" :
                      fmap (nest 2 . Pretty.printQualified) callStack
              unexpectedError -> throwIO unexpectedError

          catchFloatModByZero :: IO a -> IO a
          catchFloatModByZero action =
            action `catch` \case
              RatioZeroDenominator ->
                throwIO $
                  Failure $
                    vcat $
                      "Execution failure: float modulus by zero" :
                      "Call stack:" :
                      fmap (nest 2 . Pretty.printQualified) callStack
              unexpectedError -> throwIO unexpectedError

          catchFileAccessErrors :: IO a -> IO a
          catchFileAccessErrors action =
            action `catch` \e ->
              throwIO $
                Failure $
                  vcat $
                    "Execution failure:" :
                    show (ioeGetErrorType e) :
                    "Call stack:" :
                    fmap (nest 2 . Pretty.printQualified) callStack

  let entryPointName = fromMaybe mainName mName
  word [entryPointName] entryPointName mainArgs
  toList <$> readIORef stackRef

newtype Failure = Failure (Doc ())
  deriving (Typeable)

instance Exception Failure

instance Show Failure where
  show (Failure message) = show message
