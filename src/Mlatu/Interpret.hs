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

import Control.Exception (ArithException (..), throwIO)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM))
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
import Prettyprinter (Doc, Pretty (pretty), dquotes, hcat, hsep, list, nest, squotes, vcat)
import Relude hiding (Compose, Type, callStack, stderr, stdin, stdout)
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
  deriving (Eq, Show)

valueRep :: (Show a) => Value a -> Rep
valueRep (Term.Character c) = Character c
valueRep (Term.Float literal) = Float64 $ Literal.floatValue literal
valueRep (Term.Integer literal) = rep $ Literal.integerValue literal
  where
    rep = Int64 . fromInteger
valueRep (Term.Name name) = Name name
valueRep (Term.Text text) = Text text
valueRep value = error $ toText ("cannot convert value to rep: " ++ show value)

printRep :: Rep -> Doc a
printRep (Algebraic (ConstructorIndex index) values) =
  hsep $
    map printRep values ++ [hcat ["#", pretty index]]
printRep (Array values) =
  list $
    Vector.toList $ fmap printRep values
printRep (Character c) = squotes $ pretty c
printRep (Closure name closure) =
  hsep $
    map printRep closure ++ [hcat ["#", Pretty.printQualified name]]
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
  let entryPointName = fromMaybe mainName mName
  ((_, s), _) <- runInterp (word [entryPointName] entryPointName mainArgs) dictionary (Stack.fromList initialStack) [] [] stdin' stdout'
  return $ toList s

type Interp = ReaderT Dictionary (StateT (Stack Rep) (StateT [Rep] (StateT [[Rep]] (ReaderT Handle (ReaderT Handle IO)))))

runInterp :: Interp a -> Dictionary -> Stack Rep -> [Rep] -> [[Rep]] -> Handle -> Handle -> IO (((a, Stack Rep), [Rep]), [[Rep]])
runInterp = (((((((((runReaderT .) . runReaderT) .) . runStateT) .) . runStateT) .) . runStateT) .) . runReaderT

stdin :: ReaderT Handle (ReaderT Handle IO) a -> Interp a
stdin = lift . lift . lift . lift

stdout :: ReaderT Handle IO a -> Interp a
stdout = lift . lift . lift . lift . lift

stack :: StateT (Stack Rep) (StateT [Rep] (StateT [[Rep]] (ReaderT Handle (ReaderT Handle IO)))) a -> Interp a
stack = lift

locals :: StateT [Rep] (StateT [[Rep]] (ReaderT Handle (ReaderT Handle IO))) a -> Interp a
locals = lift . lift

closures :: StateT [[Rep]] (ReaderT Handle (ReaderT Handle IO)) a -> Interp a
closures = lift . lift . lift

word ::
  [Qualified] ->
  Qualified ->
  [Type] ->
  Interp
    ()
word
  callStack
  name
  args = do
    let mangled = Instantiated name args
    dictionary <- ask
    case Dictionary.lookup mangled dictionary of
      -- An entry in the dictionary should already be instantiated, so we
      -- shouldn't need to instantiate it again here.
      Just (Entry.Word _ _ _ _ _ (Just body)) -> term (name : callStack) body
      _noBody -> case Dictionary.lookup (Instantiated name []) dictionary of
        -- A regular word.
        Just (Entry.Word _ _ _ _ _ (Just body)) -> do
          mBody' <- liftIO $ runMlatuExceptT $ Instantiate.term TypeEnv.empty body args
          case mBody' of
            Right body' -> term (name : callStack) body'
            Left reports -> do
              stdout' <- stdout ask
              liftIO $
                hPrint stdout' $
                  vcat $
                    hcat
                      [ "Could not instantiate generic word ",
                        dquotes $ Pretty.printQualified name,
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
          throwM $
            Failure $
              hcat
                [ "I can't find an instantiation of ",
                  dquotes $ Pretty.printQualified name,
                  ": ",
                  dquotes $ Pretty.printInstantiated mangled
                ]

term :: [Qualified] -> Term Type -> Interp ()
term callStack t = case t of
  Coercion {} -> pass
  Compose _ a b -> term callStack a >> term callStack b
  -- TODO: Verify that this is correct.
  Generic _name _ t' _ -> term callStack t'
  Group t' -> term callStack t'
  Lambda _ _name _ body _ -> do
    a ::: r <- stack get
    stack $ put r
    ls <- locals get
    locals $ put (a : ls)
    term callStack body
    locals $ modify' (Unsafe.fromJust . viaNonEmpty tail)
  Match _ _ cases else_ _ -> do
    -- We delay matching on the value here because it may not be an ADT at
    -- all. For example, "1 match else { 2 }" is perfectly valid,
    -- because we are matching on all (0) of Int64's constructors.
    x ::: r <- stack get
    stack $ put r
    dictionary <- ask
    let go (Case (QualifiedName name) caseBody _ : _)
          -- FIXME: Embed this information during name resolution, rather than
          -- looking it up.
          | Just (Entry.Word _ _ _ _ _ (Just ctorBody)) <-
              Dictionary.lookup (Instantiated name []) dictionary,
            [New _ (ConstructorIndex index') _ _] <- Term.decompose ctorBody,
            Algebraic (ConstructorIndex index) fields <- x,
            index == index' =
            do
              stack $ put $ Stack.pushes fields r
              term callStack caseBody
        go (_ : rest) = go rest
        go [] = case else_ of
          DefaultElse m o -> term callStack (defaultElseBody m o)
          Else body _ -> term callStack body
    go cases
  New _ index size _ -> do
    r <- stack get
    let (fields, r') = Stack.pops size r
    stack $ put $ Algebraic index fields ::: r'
  NewClosure _ size _ -> do
    r <- stack get
    let (Name name : closure, r') = Stack.pops (size + 1) r
    stack $ put (Closure name (reverse closure) ::: r')
  NewVector _ size _ _ -> do
    r <- stack get
    let (values, r') = Stack.pops size r
    stack $
      put $
        Array (Vector.reverse $ Vector.fromList values) ::: r'
  Push _ value _ -> push value
  Word _ _ (QualifiedName name) args _ ->
    word callStack name args
  -- FIXME: Use proper reporting. (Internal error?)
  Word _ _ name _ _ ->
    error $
      show $
        hsep
          ["unresolved word name", Pretty.printGeneralName name]

call :: [Qualified] -> Interp ()
call callStack = do
  Closure name closure ::: r <- stack get
  stack $ put r
  closures $ modify' (closure :)
  -- FIXME: Use right args.
  word (name : callStack) name []
  closures $ modify' (Unsafe.fromJust . viaNonEmpty tail)

push :: Value Type -> Interp ()
push value = case value of
  Term.Closed (ClosureIndex index) -> do
    (currentClosure : _) <- closures get
    stack $ modify' (Unsafe.fromJust (currentClosure !!? index) :::)
  Term.Local (LocalIndex index) -> do
    ls <- locals get
    stack $ modify' (Unsafe.fromJust (ls !!? index) :::)
  Term.Text text ->
    stack $
      modify'
        (Text text :::)
  _otherTerm -> stack $ modify' (valueRep value :::)

intrinsic :: [Qualified] -> Unqualified -> Interp ()
intrinsic callStack name = case name of
  "abort" -> do
    Text txt ::: r <- stack get
    stack $ put r
    throwM $
      Failure $
        vcat $
          hsep
            [ "Execution failure:",
              pretty txt
            ] :
          "Call stack:" :
          map (nest 2 . Pretty.printQualified) callStack
  "exit" -> do
    Int64 i ::: r <- stack get
    stack $ put r
    exitWith $
      if i == 0
        then ExitSuccess
        else ExitFailure $ fromIntegral i
  "call" -> call callStack
  "drop" -> stack $ modify' Stack.popNote
  "swap" -> do
    a ::: b ::: r <- stack get
    stack $ put $ b ::: a ::: r
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
    Array xs ::: r <- stack get
    stack $ put r
    if Vector.null xs
      then word callStack (Qualified Vocabulary.global "true") []
      else word callStack (Qualified Vocabulary.global "false") []
  "head" -> do
    Array xs ::: r <- stack get
    if Vector.null xs
      then do
        stack $ put r
        word callStack (Qualified Vocabulary.global "none") []
      else do
        let x = xs ! 0
        stack $ put $ x ::: r
        -- FIXME: Use right args.
        word callStack (Qualified Vocabulary.global "some") []
  "last" -> do
    Array xs ::: r <- stack get
    if Vector.null xs
      then do
        stack $ put r
        word callStack (Qualified Vocabulary.global "none") []
      else do
        let x = xs ! (Vector.length xs - 1)
        stack $ put $ x ::: r
        -- FIXME: Use right args.
        word callStack (Qualified Vocabulary.global "some") []
  "cat" -> do
    Array ys ::: Array xs ::: r <- stack get
    stack $ put $ Array (xs <> ys) ::: r
  "string_concat" -> do
    Text y ::: Text x ::: r <- stack get
    stack $ put $ Text (Text.concat [x, y]) ::: r
  "string_from_list" -> do
    Array cs ::: r <- stack get
    let string = map (\(Character ch) -> ch) $ Vector.toList cs
    stack $ put $ Text (toText string) ::: r
  "string_to_list" -> do
    Text txt ::: r <- stack get
    let string = Array (Vector.fromList $ map Character $ toString txt)
    stack $ put $ string ::: r
  "get" -> do
    Int64 i ::: Array xs ::: r <- stack get
    if i < 0 || i >= fromIntegral (length xs)
      then do
        stack $ put r
        word callStack (Qualified Vocabulary.global "none") []
      else do
        stack $ put $ (xs ! fromIntegral i) ::: r
        -- FIXME: Use right args.
        word callStack (Qualified Vocabulary.global "some") []
  "set" -> do
    Int64 i ::: x ::: Array xs ::: r <- stack get
    if i < 0 || i >= fromIntegral (length xs)
      then do
        stack $ put r
        word callStack (Qualified Vocabulary.global "none") []
      else do
        let (before, after) = Vector.splitAt (fromIntegral i) xs
        stack $
          put $
            Array
              (before <> Vector.singleton x <> Vector.tail after)
              ::: r
        -- FIXME: Use right args.
        word callStack (Qualified Vocabulary.global "some") []
  "print" -> do
    Text txt ::: r <- stack get
    stack $ put r
    stdout ask >>= (\stdout' -> liftIO $ hPutStr stdout' $ toString txt)
  "get_line" -> do
    stdin' <- stdin ask
    line <- liftIO $ hGetLine stdin'
    stack $ modify' ((Text $ toText line) :::)
  "flush_stdout" -> stdout ask >>= (liftIO . hFlush)
  "read_file" -> do
    Text txt ::: r <- stack get
    catchFileAccessErrors (readFile $ toString txt)
      >>= (\contents -> stack $ put $ Text (toText contents) ::: r)
  "write_file" -> do
    Text cs ::: Text bs ::: r <- stack get
    stack $ put r
    catchFileAccessErrors $
      writeFile (toString cs) $
        toString bs
  "append_file" -> do
    Text cs ::: Text bs ::: r <- stack get
    stack $ put r
    catchFileAccessErrors $
      appendFile (toString cs) $
        toString bs
  "tail" -> do
    Array xs ::: r <- stack get
    if Vector.null xs
      then do
        stack $ put r
        word callStack (Qualified Vocabulary.global "none") []
      else do
        let xs' = Vector.tail xs
        stack $ put $ Array xs' ::: r
        -- FIXME: Use right args.
        word callStack (Qualified Vocabulary.global "some") []
  "init" -> do
    Array xs ::: r <- stack get
    if Vector.null xs
      then do
        stack $ put r
        word callStack (Qualified Vocabulary.global "none") []
      else do
        let xs' = Vector.init xs
        stack $ put $ Array xs' ::: r
        -- FIXME: Use right args.
        word callStack (Qualified Vocabulary.global "some") []
  _nonIntrinsic -> error "no such intrinsic"
  where
    unaryInt64 :: (Int64 -> Int64) -> Interp ()
    unaryInt64 f = stack $ modify' (\(Int64 x ::: r) -> Int64 (fromIntegral $ f $ fromIntegral x) ::: r)

    binaryInt64 :: (Int64 -> Int64 -> Int64) -> Interp ()
    binaryInt64 f = do
      Int64 y ::: Int64 x ::: r <- stack get
      let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
      stack $ put $ Int64 result ::: r

    boolInt64 :: (Int64 -> Int64 -> Bool) -> Interp ()
    boolInt64 f = do
      Int64 y ::: Int64 x ::: r <- stack get
      let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
      stack $ put $ Algebraic (ConstructorIndex result) [] ::: r

    boolChar :: (Char -> Char -> Bool) -> Interp ()
    boolChar f = do
      Character y ::: Character x ::: r <- stack get
      let !result = fromEnum $ f x y
      stack $ put $ Algebraic (ConstructorIndex result) [] ::: r

    unaryFloat64 :: (Double -> Double) -> Interp ()
    unaryFloat64 f = do
      Float64 x ::: r <- stack get
      let !result = realToFrac $ f $ realToFrac x
      stack $ put $ Float64 result ::: r

    binaryFloat64 :: (Double -> Double -> Double) -> Interp ()
    binaryFloat64 f = do
      Float64 y ::: Float64 x ::: r <- stack get
      let !result = f x y
      stack $ put $ Float64 result ::: r

    boolFloat64 :: (Double -> Double -> Bool) -> Interp ()
    boolFloat64 f = do
      Float64 y ::: Float64 x ::: r <- stack get
      let !result = fromEnum $ f x y
      stack $ put $ Algebraic (ConstructorIndex result) [] ::: r

    boolString :: (Text -> Text -> Bool) -> Interp ()
    boolString f = do
      Text y ::: Text x ::: r <- stack get
      let !result = fromEnum $ f x y
      stack $ put $ Algebraic (ConstructorIndex result) [] ::: r

    showInteger :: Num a => (a -> Text) -> Interp ()
    showInteger f = stack $ modify' (\(Int64 n ::: r) -> Text (f $ fromIntegral n) ::: r)

    showFloat :: Fractional a => (a -> Text) -> Interp ()
    showFloat f = stack $ modify' (\(Float64 n ::: r) -> Text (f (realToFrac n)) ::: r)

    readInteger :: Integral a => (Text -> IO a) -> (a -> Rep) -> Interp ()
    readInteger f rep = do
      Text txt ::: r <- stack get
      result <- liftIO $ f txt
      stack $ put $ rep result ::: r
      -- FIXME: Use right args.
      word callStack (Qualified Vocabulary.global "some") []
        `catch` ( \(_ :: IOError) -> do
                    stack $ put r
                    word callStack (Qualified Vocabulary.global "none") []
                )

    readFloat :: Real a => (Text -> IO a) -> (a -> Rep) -> Interp ()
    readFloat f rep = do
      Text txt ::: r <- stack get
      result <- liftIO $ f txt
      stack $ put $ rep result ::: r
      -- FIXME: Use right args.
      word callStack (Qualified Vocabulary.global "some") []
        `catch` ( \(_ :: IOError) -> do
                    stack $ put r
                    word callStack (Qualified Vocabulary.global "none") []
                )

    catchDivideByZero :: Interp a -> Interp a
    catchDivideByZero action =
      action `catch` \case
        DivideByZero ->
          throwM $
            Failure $
              vcat $
                "Execution failure: integer division by zero" :
                "Call stack:" :
                map (nest 2 . Pretty.printQualified) callStack
        unexpectedError -> liftIO $ throwIO unexpectedError

    catchFloatModByZero :: Interp a -> Interp a
    catchFloatModByZero action =
      action `catch` \case
        RatioZeroDenominator ->
          throwM $
            Failure $
              vcat $
                "Execution failure: float modulus by zero" :
                "Call stack:" :
                map (nest 2 . Pretty.printQualified) callStack
        unexpectedError -> liftIO $ throwIO unexpectedError

    catchFileAccessErrors :: Interp a -> Interp a
    catchFileAccessErrors action =
      action `catch` \e ->
        throwM $
          Failure $
            vcat $
              "Execution failure:" :
              show (ioeGetErrorType e) :
              "Call stack:" :
              map (nest 2 . Pretty.printQualified) callStack

newtype Failure = Failure (Doc ())
  deriving (Typeable)

instance Exception Failure

instance Show Failure where
  show (Failure message) = show message
