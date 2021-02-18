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
  )
where

import Control.Exception (ArithException (..), catch, throwIO)
import Data.Bits
  ( Bits (complement, rotate, shift, (.&.), (.|.)),
  )
import Data.Fixed (mod')
import Data.Text qualified as Text
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import Mlatu.Bits qualified as Bits
import Mlatu.Definition (mainName)
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Entry qualified as Entry
import Mlatu.Instantiate qualified as Instantiate
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Literal qualified as Literal
import Mlatu.Monad (runMlatu)
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
import Mlatu.Term (Case (..), Else (..), Term (..), Value)
import Mlatu.Term qualified as Term
import Mlatu.Type (Type (..))
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Vocabulary qualified as Vocabulary
import Numeric (log)
import Relude hiding (Compose, Type, callStack)
import Relude.Unsafe qualified as Unsafe
import System.Exit (ExitCode (..))
import System.IO (hFlush, hGetLine, hPutStrLn, readIO, hPutStr)
import System.IO.Error (IOError, ioeGetErrorType)
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Text.Show qualified

-- | Representation of a runtime value.
data Rep
  = Algebraic !ConstructorIndex ![Rep]
  | Array !(Vector Rep)
  | Character !Char
  | Closure !Qualified ![Rep]
  | Float32 !Float
  | Float64 !Double
  | Int8 !Int8
  | Int16 !Int16
  | Int32 !Int32
  | Int64 !Int64
  | UInt8 !Word8
  | UInt16 !Word16
  | UInt32 !Word32
  | UInt64 !Word64
  | Name !Qualified
  | Text !Text
  deriving (Eq, Show)

valueRep :: (Show a) => Value a -> Rep
valueRep (Term.Character c) = Character c
valueRep (Term.Float literal) = case Literal.floatBits literal of
  Bits.Float32 -> Float32 $ Literal.floatValue literal
  Bits.Float64 -> Float64 $ Literal.floatValue literal
valueRep (Term.Integer literal) = rep $ Literal.integerValue literal
  where
    rep = case Literal.integerBits literal of
      Bits.Signed8 -> Int8 . fromInteger
      Bits.Signed16 -> Int16 . fromInteger
      Bits.Signed32 -> Int32 . fromInteger
      Bits.Signed64 -> Int64 . fromInteger
      Bits.Unsigned8 -> UInt8 . fromInteger
      Bits.Unsigned16 -> UInt16 . fromInteger
      Bits.Unsigned32 -> UInt32 . fromInteger
      Bits.Unsigned64 -> UInt64 . fromInteger
valueRep (Term.Name name) = Name name
valueRep (Term.Text text) = Text text
valueRep value = error $ toText ("cannot convert value to rep: " ++ show value)

instance Pretty Rep where
  pPrint (Algebraic (ConstructorIndex index) values) =
    Pretty.hsep $
      map pPrint values ++ [Pretty.hcat ["#", Pretty.int index]]
  pPrint (Array values) =
    Pretty.brackets $
      Pretty.list $
        Vector.toList $ fmap pPrint values
  pPrint (Character c) = Pretty.quotes $ Pretty.char c
  pPrint (Closure name closure) =
    Pretty.hsep $
      map pPrint closure ++ [Pretty.hcat ["#", pPrint name]]
  pPrint (Float32 f) = pPrint f
  pPrint (Float64 f) = pPrint f
  pPrint (Int8 i) = Pretty.hcat [Pretty.int $ fromIntegral i, pPrint Bits.Signed8]
  pPrint (Int16 i) = Pretty.hcat [Pretty.int $ fromIntegral i, pPrint Bits.Signed16]
  pPrint (Int32 i) = Pretty.hcat [Pretty.int $ fromIntegral i, pPrint Bits.Signed32]
  pPrint (Int64 i) = Pretty.hcat [Pretty.int $ fromIntegral i, pPrint Bits.Signed64]
  pPrint (UInt8 i) = Pretty.hcat [Pretty.int $ fromIntegral i, pPrint Bits.Unsigned8]
  pPrint (UInt16 i) = Pretty.hcat [Pretty.int $ fromIntegral i, pPrint Bits.Unsigned16]
  pPrint (UInt32 i) = Pretty.hcat [Pretty.int $ fromIntegral i, pPrint Bits.Unsigned32]
  pPrint (UInt64 i) = Pretty.hcat [Pretty.int $ fromIntegral i, pPrint Bits.Unsigned64]
  pPrint (Name n) = Pretty.hcat ["\\", pPrint n]
  pPrint (Text t) = Pretty.doubleQuotes $ Pretty.text $ toString t

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
              mBody' <- runMlatu $ Instantiate.term TypeEnv.empty body args
              case mBody' of
                Right body' -> term (name : callStack) body'
                Left reports ->
                  hPutStrLn stdout' $
                    Pretty.render $
                      Pretty.vcat $
                        Pretty.hcat
                          [ "Could not instantiate generic word ",
                            Pretty.quote name,
                            ":"
                          ] :
                        map Report.human reports
            -- An intrinsic.
            Just (Entry.Word _ _ _ _ _ Nothing) -> case name of
              Qualified v unqualified
                | v == Vocabulary.intrinsic ->
                  intrinsic (name : callStack) unqualified
              _nonIntrinsic -> error "no such intrinsic"
            _noInstantiation ->
              throwIO $
                Failure $
                  Pretty.hcat
                    [ "I can't find an instantiation of ",
                      Pretty.quote name,
                      ": ",
                      Pretty.quote mangled
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
          -- because we are matching on all (0) of Int32's constructors.
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
        -- FIXME: Use proper reporting. (Internal error?)
        Word _ _ name _ _ ->
          error $
            toText $
              Pretty.render $
                Pretty.hsep
                  ["unresolved word name", pPrint name]

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
              Pretty.vcat $
                Pretty.hsep
                  [ "Execution failure:",
                    Pretty.text $ toString txt
                  ] :
                "Call stack:" :
                map (Pretty.nest 4 . pPrint) callStack
        "exit" -> do
          Int32 i ::: r <- readIORef stackRef
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
        "add_int8" -> binaryInt8 (+)
        "sub_int8" -> binaryInt8 (-)
        "mul_int8" -> binaryInt8 (*)
        "div_int8" -> catchDivideByZero $ binaryInt8 div
        "mod_int8" -> catchDivideByZero $ binaryInt8 mod
        "add_int16" -> binaryInt16 (+)
        "sub_int16" -> binaryInt16 (-)
        "mul_int16" -> binaryInt16 (*)
        "div_int16" -> catchDivideByZero $ binaryInt16 div
        "mod_int16" -> catchDivideByZero $ binaryInt16 mod
        "add_int32" -> binaryInt32 (+)
        "sub_int32" -> binaryInt32 (-)
        "mul_int32" -> binaryInt32 (*)
        "div_int32" -> catchDivideByZero $ binaryInt32 div
        "mod_int32" -> catchDivideByZero $ binaryInt32 mod
        "add_int64" -> binaryInt64 (+)
        "sub_int64" -> binaryInt64 (-)
        "mul_int64" -> binaryInt64 (*)
        "div_int64" -> catchDivideByZero $ binaryInt64 div
        "mod_int64" -> catchDivideByZero $ binaryInt64 mod
        "not_int8" -> unaryInt8 complement
        "or_int8" -> binaryInt8 (.|.)
        "and_int8" -> binaryInt8 (.&.)
        "xor_int8" -> binaryInt8 xor
        "shl_int8" -> binaryInt8Int shift
        "rol_int8" -> binaryInt8Int rotate
        "not_int16" -> unaryInt16 complement
        "or_int16" -> binaryInt16 (.|.)
        "and_int16" -> binaryInt16 (.&.)
        "xor_int16" -> binaryInt16 xor
        "shl_int16" -> binaryInt16Int shift
        "rol_int16" -> binaryInt16Int rotate
        "not_int32" -> unaryInt32 complement
        "or_int32" -> binaryInt32 (.|.)
        "and_int32" -> binaryInt32 (.&.)
        "xor_int32" -> binaryInt32 xor
        "shl_int32" -> binaryInt32Int shift
        "rol_int32" -> binaryInt32Int rotate
        "not_int64" -> unaryInt64 complement
        "or_int64" -> binaryInt64 (.|.)
        "and_int64" -> binaryInt64 (.&.)
        "xor_int64" -> binaryInt64 xor
        "shl_int64" -> binaryInt64Int shift
        "rol_int64" -> binaryInt64Int rotate
        "gt_int8" -> boolInt8 (>)
        "eq_int8" -> boolInt8 (==)
        "gt_int16" -> boolInt16 (>)
        "eq_int16" -> boolInt16 (==)
        "gt_int32" -> boolInt32 (>)
        "eq_int32" -> boolInt32 (==)
        "gt_int64" -> boolInt64 (>)
        "eq_int64" -> boolInt64 (==)
        "add_uint8" -> binaryUInt8 (+)
        "sub_uint8" -> binaryUInt8 (-)
        "mul_uint8" -> binaryUInt8 (*)
        "div_uint8" -> catchDivideByZero $ binaryUInt8 div
        "mod_uint8" -> catchDivideByZero $ binaryUInt8 mod
        "add_uint16" -> binaryUInt16 (+)
        "sub_uint16" -> binaryUInt16 (-)
        "mul_uint16" -> binaryUInt16 (*)
        "div_uint16" -> catchDivideByZero $ binaryUInt16 div
        "mod_uint16" -> catchDivideByZero $ binaryUInt16 mod
        "add_uint32" -> binaryUInt32 (+)
        "sub_uint32" -> binaryUInt32 (-)
        "mul_uint32" -> binaryUInt32 (*)
        "div_uint32" -> catchDivideByZero $ binaryUInt32 div
        "mod_uint32" -> catchDivideByZero $ binaryUInt32 mod
        "add_uint64" -> binaryUInt64 (+)
        "sub_uint64" -> binaryUInt64 (-)
        "mul_uint64" -> binaryUInt64 (*)
        "div_uint64" -> catchDivideByZero $ binaryUInt64 div
        "mod_uint64" -> catchDivideByZero $ binaryUInt64 mod
        "not_uint8" -> unaryUInt8 complement
        "or_uint8" -> binaryUInt8 (.|.)
        "and_uint8" -> binaryUInt8 (.&.)
        "xor_uint8" -> binaryUInt8 xor
        "shl_uint8" -> binaryUInt8Int shift
        "rol_uint8" -> binaryUInt8Int rotate
        "not_uint16" -> unaryUInt16 complement
        "or_uint16" -> binaryUInt16 (.|.)
        "and_uint16" -> binaryUInt16 (.&.)
        "xor_uint16" -> binaryUInt16 xor
        "shl_uint16" -> binaryUInt16Int shift
        "rol_uint16" -> binaryUInt16Int rotate
        "not_uint32" -> unaryUInt32 complement
        "or_uint32" -> binaryUInt32 (.|.)
        "and_uint32" -> binaryUInt32 (.&.)
        "xor_uint32" -> binaryUInt32 xor
        "shl_uint32" -> binaryUInt32Int shift
        "rol_uint32" -> binaryUInt32Int rotate
        "not_uint64" -> unaryUInt64 complement
        "or_uint64" -> binaryUInt64 (.|.)
        "and_uint64" -> binaryUInt64 (.&.)
        "xor_uint64" -> binaryUInt64 xor
        "shl_uint64" -> binaryUInt64Int shift
        "rol_uint64" -> binaryUInt64Int rotate
        "gt_uint8" -> boolUInt8 (>)
        "eq_uint8" -> boolUInt8 (==)
        "gt_uint16" -> boolUInt16 (>)
        "eq_uint16" -> boolUInt16 (==)
        "gt_uint32" -> boolUInt32 (>)
        "eq_uint32" -> boolUInt32 (==)
        "gt_uint64" -> boolUInt64 (>)
        "eq_uint64" -> boolUInt64 (==)
        "gt_char" -> boolChar (>)
        "eq_char" -> boolChar (==)
        "add_float32" -> binaryFloat32 (+)
        "sub_float32" -> binaryFloat32 (-)
        "mul_float32" -> binaryFloat32 (*)
        "div_float32" -> binaryFloat32 (/)
        "mod_float32" -> catchFloatModByZero $ binaryFloat32 mod'
        "add_float64" -> binaryFloat64 (+)
        "sub_float64" -> binaryFloat64 (-)
        "mul_float64" -> binaryFloat64 (*)
        "div_float64" -> binaryFloat64 (/)
        "mod_float64" -> catchFloatModByZero $ binaryFloat64 mod'
        "exp_float32" -> unaryFloat32 exp
        "log_float32" -> unaryFloat32 log
        "sqrt_float32" -> unaryFloat32 sqrt
        "sin_float32" -> unaryFloat32 sin
        "cos_float32" -> unaryFloat32 cos
        "tan_float32" -> unaryFloat32 tan
        "asin_float32" -> unaryFloat32 asin
        "acos_float32" -> unaryFloat32 acos
        "atan_float32" -> unaryFloat32 atan
        "atan2_float32" -> binaryFloat32 atan2
        "sinh_float32" -> unaryFloat32 sinh
        "cosh_float32" -> unaryFloat32 cosh
        "tanh_float32" -> unaryFloat32 tanh
        "asinh_float32" -> unaryFloat32 asinh
        "acosh_float32" -> unaryFloat32 acosh
        "atanh_float32" -> unaryFloat32 atanh
        "trunc_float32" -> unaryFloat32 $ fromInteger . truncate
        "round_float32" -> unaryFloat32 $ fromInteger . round
        "ceil_float32" -> unaryFloat32 $ fromInteger . ceiling
        "floor_float32" -> unaryFloat32 $ fromInteger . floor
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
        "gt_float32" -> boolFloat32 (>)
        "eq_float32" -> boolFloat32 (==)
        "gt_float64" -> boolFloat64 (>)
        "eq_float64" -> boolFloat64 (==)
        "gt_string" -> boolString (>)
        "eq_string" -> boolString (==)
        "show_int8" -> showInteger (show @Text @Int8)
        "show_int16" -> showInteger (show @Text @Int16)
        "show_int32" -> showInteger (show @Text @Int32)
        "show_int64" -> showInteger (show @Text @Int64)
        "show_uint8" -> showInteger (show @Text @Word8)
        "show_uint16" -> showInteger (show :: Word16 -> Text)
        "show_uint32" -> showInteger (show :: Word32 -> Text)
        "show_uint64" -> showInteger (show :: Word64 -> Text)
        "show_float32" -> showFloat (show :: Float -> Text)
        "show_float64" -> showFloat (show :: Double -> Text)
        "read_int8" -> readInteger ((readIO :: String -> IO Int8) . toString) Int8
        "read_int16" -> readInteger ((readIO :: String -> IO Int16) . toString) Int16
        "read_int32" -> readInteger ((readIO :: String -> IO Int32) . toString) Int32
        "read_int64" -> readInteger ((readIO :: String -> IO Int64) . toString) Int64
        "read_uint8" -> readInteger ((readIO :: String -> IO Word8) . toString) UInt8
        "read_uint16" -> readInteger ((readIO :: String -> IO Word16) . toString) UInt16
        "read_uint32" -> readInteger ((readIO :: String -> IO Word32) . toString) UInt32
        "read_uint64" -> readInteger ((readIO :: String -> IO Word64) . toString) UInt64
        "read_float32" -> readFloat ((readIO :: String -> IO Float) . toString) Float32
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
          let string = map (\(Character c) -> c) $ Vector.toList cs
          writeIORef stackRef $ Text (toText string) ::: r
        "string_to_list" -> do
          Text txt ::: r <- readIORef stackRef
          let string = Array (Vector.fromList $ map Character $ toString txt)
          writeIORef stackRef $ string ::: r
        "get" -> do
          Int32 i ::: Array xs ::: r <- readIORef stackRef
          if i < 0 || i >= fromIntegral (length xs)
            then do
              writeIORef stackRef r
              word callStack (Qualified Vocabulary.global "none") []
            else do
              writeIORef stackRef $ (xs ! fromIntegral i) ::: r
              -- FIXME: Use right args.
              word callStack (Qualified Vocabulary.global "some") []
        "set" -> do
          Int32 i ::: x ::: Array xs ::: r <- readIORef stackRef
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
        _nonIntrinsic -> error "no such intrinsic"
        where
          unaryInt8 :: (Int8 -> Int8) -> IO ()
          unaryInt8 f = do
            Int8 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f $ fromIntegral x
            writeIORef stackRef $ Int8 result ::: r

          binaryInt8 :: (Int8 -> Int8 -> Int8) -> IO ()
          binaryInt8 f = do
            Int8 y ::: Int8 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Int8 result ::: r

          binaryInt8Int :: (Int8 -> Int -> Int8) -> IO ()
          binaryInt8Int f = do
            Int32 y ::: Int8 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Int8 result ::: r

          unaryInt16 :: (Int16 -> Int16) -> IO ()
          unaryInt16 f = do
            Int16 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f $ fromIntegral x
            writeIORef stackRef $ Int16 result ::: r

          binaryInt16 :: (Int16 -> Int16 -> Int16) -> IO ()
          binaryInt16 f = do
            Int16 y ::: Int16 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Int16 result ::: r

          binaryInt16Int :: (Int16 -> Int -> Int16) -> IO ()
          binaryInt16Int f = do
            Int32 y ::: Int16 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Int16 result ::: r

          unaryInt32 :: (Int32 -> Int32) -> IO ()
          unaryInt32 f = do
            Int32 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f $ fromIntegral x
            writeIORef stackRef $ Int32 result ::: r

          binaryInt32 :: (Int32 -> Int32 -> Int32) -> IO ()
          binaryInt32 f = do
            Int32 y ::: Int32 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Int32 result ::: r

          binaryInt32Int :: (Int32 -> Int -> Int32) -> IO ()
          binaryInt32Int f = do
            Int32 y ::: Int32 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Int32 result ::: r

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

          binaryInt64Int :: (Int64 -> Int -> Int64) -> IO ()
          binaryInt64Int f = do
            Int32 y ::: Int64 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Int64 result ::: r

          unaryUInt8 :: (Word8 -> Word8) -> IO ()
          unaryUInt8 f = do
            UInt8 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f $ fromIntegral x
            writeIORef stackRef $ UInt8 result ::: r

          binaryUInt8 :: (Word8 -> Word8 -> Word8) -> IO ()
          binaryUInt8 f = do
            UInt8 y ::: UInt8 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ UInt8 result ::: r

          binaryUInt8Int :: (Word8 -> Int -> Word8) -> IO ()
          binaryUInt8Int f = do
            Int32 y ::: UInt8 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ UInt8 result ::: r

          unaryUInt16 :: (Word16 -> Word16) -> IO ()
          unaryUInt16 f = do
            UInt16 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f $ fromIntegral x
            writeIORef stackRef $ UInt16 result ::: r

          binaryUInt16 :: (Word16 -> Word16 -> Word16) -> IO ()
          binaryUInt16 f = do
            UInt16 y ::: UInt16 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ UInt16 result ::: r

          binaryUInt16Int :: (Word16 -> Int -> Word16) -> IO ()
          binaryUInt16Int f = do
            Int32 y ::: UInt16 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ UInt16 result ::: r

          unaryUInt32 :: (Word32 -> Word32) -> IO ()
          unaryUInt32 f = do
            UInt32 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f $ fromIntegral x
            writeIORef stackRef $ UInt32 result ::: r

          binaryUInt32 :: (Word32 -> Word32 -> Word32) -> IO ()
          binaryUInt32 f = do
            UInt32 y ::: UInt32 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ UInt32 result ::: r

          binaryUInt32Int :: (Word32 -> Int -> Word32) -> IO ()
          binaryUInt32Int f = do
            Int32 y ::: UInt32 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ UInt32 result ::: r

          unaryUInt64 :: (Word64 -> Word64) -> IO ()
          unaryUInt64 f = do
            UInt64 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f $ fromIntegral x
            writeIORef stackRef $ UInt64 result ::: r

          binaryUInt64 :: (Word64 -> Word64 -> Word64) -> IO ()
          binaryUInt64 f = do
            UInt64 y ::: UInt64 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ UInt64 result ::: r

          binaryUInt64Int :: (Word64 -> Int -> Word64) -> IO ()
          binaryUInt64Int f = do
            Int32 y ::: UInt64 x ::: r <- readIORef stackRef
            let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ UInt64 result ::: r

          boolInt8 :: (Int8 -> Int8 -> Bool) -> IO ()
          boolInt8 f = do
            Int8 y ::: Int8 x ::: r <- readIORef stackRef
            let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          boolInt16 :: (Int16 -> Int16 -> Bool) -> IO ()
          boolInt16 f = do
            Int16 y ::: Int16 x ::: r <- readIORef stackRef
            let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          boolInt32 :: (Int32 -> Int32 -> Bool) -> IO ()
          boolInt32 f = do
            Int32 y ::: Int32 x ::: r <- readIORef stackRef
            let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          boolInt64 :: (Int64 -> Int64 -> Bool) -> IO ()
          boolInt64 f = do
            Int64 y ::: Int64 x ::: r <- readIORef stackRef
            let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          boolUInt8 :: (Word8 -> Word8 -> Bool) -> IO ()
          boolUInt8 f = do
            UInt8 y ::: UInt8 x ::: r <- readIORef stackRef
            let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          boolUInt16 :: (Word16 -> Word16 -> Bool) -> IO ()
          boolUInt16 f = do
            UInt16 y ::: UInt16 x ::: r <- readIORef stackRef
            let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          boolUInt32 :: (Word32 -> Word32 -> Bool) -> IO ()
          boolUInt32 f = do
            UInt32 y ::: UInt32 x ::: r <- readIORef stackRef
            let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          boolUInt64 :: (Word64 -> Word64 -> Bool) -> IO ()
          boolUInt64 f = do
            UInt64 y ::: UInt64 x ::: r <- readIORef stackRef
            let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          boolChar :: (Char -> Char -> Bool) -> IO ()
          boolChar f = do
            Character y ::: Character x ::: r <- readIORef stackRef
            let !result = fromEnum $ f x y
            writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

          unaryFloat32 :: (Float -> Float) -> IO ()
          unaryFloat32 f = do
            Float32 x ::: r <- readIORef stackRef
            let !result = realToFrac $ f $ realToFrac x
            writeIORef stackRef $ Float32 result ::: r

          binaryFloat32 :: (Float -> Float -> Float) -> IO ()
          binaryFloat32 f = do
            Float32 y ::: Float32 x ::: r <- readIORef stackRef
            let !result = realToFrac $ f (realToFrac x) (realToFrac y)
            writeIORef stackRef $ Float32 result ::: r

          boolFloat32 :: (Float -> Float -> Bool) -> IO ()
          boolFloat32 f = do
            Float32 y ::: Float32 x ::: r <- readIORef stackRef
            let !result = fromEnum $ f (realToFrac x) (realToFrac y)
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

          showInteger :: Num a => (a -> Text) -> IO ()
          showInteger f = do
            rep ::: r <- readIORef stackRef
            let x = case rep of
                  Int8 n -> toInteger n
                  Int16 n -> toInteger n
                  Int32 n -> toInteger n
                  Int64 n -> toInteger n
                  UInt8 n -> toInteger n
                  UInt16 n -> toInteger n
                  UInt32 n -> toInteger n
                  UInt64 n -> toInteger n
                  _nonInt -> error "the typechecker has failed us (show integer)"
            writeIORef stackRef $
              Text (f $ fromInteger x) ::: r

          showFloat :: Fractional a => (a -> Text) -> IO ()
          showFloat f = do
            rep ::: r <- readIORef stackRef
            let x = case rep of
                  Float32 n -> realToFrac n
                  Float64 n -> realToFrac n
                  _nonFloat -> error "the typechecker has failed us (show float)"
            writeIORef stackRef $
              Text (f x) ::: r

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
                    Pretty.vcat $
                      "Execution failure: integer division by zero" :
                      "Call stack:" :
                      map (Pretty.nest 4 . pPrint) callStack
              unexpectedError -> throwIO unexpectedError

          catchFloatModByZero :: IO a -> IO a
          catchFloatModByZero action =
            action `catch` \case
              RatioZeroDenominator ->
                throwIO $
                  Failure $
                    Pretty.vcat $
                      "Execution failure: float modulus by zero" :
                      "Call stack:" :
                      map (Pretty.nest 4 . pPrint) callStack
              unexpectedError -> throwIO unexpectedError

          catchFileAccessErrors :: IO a -> IO a
          catchFileAccessErrors action =
            action `catch` \e ->
              throwIO $
                Failure $
                  Pretty.vcat $
                    "Execution failure:" :
                    show (ioeGetErrorType e) :
                    "Call stack:" :
                    map (Pretty.nest 4 . pPrint) callStack

  let entryPointName = fromMaybe mainName mName
  word [entryPointName] entryPointName mainArgs
  toList <$> readIORef stackRef

newtype Failure = Failure Pretty.Doc
  deriving (Typeable)

instance Exception Failure

instance Show Failure where
  show (Failure message) = Pretty.render message
