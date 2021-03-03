{-# OPTIONS_GHC -Wno-orphans #-}

module Mlatu.Codegen.Binary where

import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Mlatu.Codegen.Structure
import Relude hiding (Const, local)

unsigned :: (Integral a) => a -> Builder
unsigned n = case n `quot` 128 of
  0 -> word8 (fromIntegral r)
  q -> word8 (fromIntegral r + 128) <> unsigned q
  where
    r = n `rem` 128

signed :: (Integral a) => a -> Builder
signed n = case n `quot` 64 of
  0 -> word8 (fromIntegral n)
  _ -> case n `quot` 128 of
    0 -> word8 (fromIntegral n + 128)
    q -> word8 (fromIntegral n + 128) <> signed q

float :: Float -> Builder
float = floatLE

double :: Double -> Builder
double = doubleLE

vector :: (a -> Builder) -> [a] -> Builder
vector f v = unsigned u32Length <> foldMap f v
  where
    u32Length :: Word32
    u32Length = fromIntegral $ length v

name :: Text -> Builder
name t = vector charUtf8 (toString t)

valueType :: ValueType -> Builder
valueType I32 = word8 0x7F
valueType I64 = word8 0x7E
valueType F32 = word8 0x7D
valueType F64 = word8 0x7C

paramsType :: ParamsType -> Builder
paramsType (ParamsType ts) = vector valueType ts

resultType :: ResultType -> Builder
resultType (ResultType ts) = vector valueType ts

funcType :: FuncType -> Builder
funcType (FuncType ps rs) = word8 0x60 <> paramsType ps <> resultType rs

limit :: Limit -> Builder
limit (Limit minLimit Nothing) = word8 0x00 <> unsigned minLimit
limit (Limit minLimit (Just maxLimit)) = word8 0x01 <> unsigned minLimit <> unsigned maxLimit

memory :: Memory -> Builder
memory (Memory l) = limit l

tableType :: TableType -> Builder
tableType (TableType lim et) = elemType et <> limit lim

elemType :: ElemType -> Builder
elemType _ = word8 0x70

globalType :: GlobalType -> Builder
globalType (Const t) = word8 0x00 <> valueType t
globalType (Mut t) = word8 0x01 <> valueType t

typeIndex :: TypeIndex -> Builder
typeIndex (TypeIndex i) = signed i

labelIndex :: LabelIndex -> Builder
labelIndex (LabelIndex i) = unsigned i

funcIndex :: FuncIndex -> Builder
funcIndex (FuncIndex i) = unsigned i

localIndex :: LocalIndex -> Builder
localIndex (LocalIndex i) = unsigned i

globalIndex :: GlobalIndex -> Builder
globalIndex (GlobalIndex i) = unsigned i

tableIndex :: TableIndex -> Builder
tableIndex (TableIndex i) = unsigned i

memoryIndex :: MemoryIndex -> Builder
memoryIndex (MemoryIndex i) = unsigned i

memarg :: MemArg -> Builder
memarg (MemArg o a) = unsigned a <> unsigned o

blockType :: BlockType -> Builder
blockType (ValueBlock (Just vt)) = valueType vt
blockType (ValueBlock Nothing) = word8 0x40
blockType (IndexBlock t) = typeIndex t

expression :: Expression -> Builder
expression (Expression ins) = foldMap instruction ins <> word8 0x0B

instruction :: Instruction -> Builder
instruction = \case
  Unreachable -> word8 0x00
  Nop -> word8 0x01
  (Block bt expr) -> word8 0x02 <> blockType bt <> expression expr <> word8 0x0B
  (Loop bt expr) -> word8 0x03 <> blockType bt <> expression expr <> word8 0x0B
  (If bt true Nothing) -> word8 0x04 <> blockType bt <> expression true <> word8 0x0B
  (If bt true (Just false)) -> word8 0x04 <> blockType bt <> expression true <> word8 0x05 <> expression false <> word8 0x0B
  (Br l) -> word8 0x0C <> labelIndex l
  (BrIf l) -> word8 0x0D <> labelIndex l
  (BrTable ls l) -> word8 0x0E <> vector labelIndex ls <> labelIndex l
  Return -> word8 0x0F
  (Call f) -> word8 0x10 <> funcIndex f
  (CallIndirect f) -> word8 0x11 <> typeIndex f <> word8 0x00
  Drop -> word8 0x1A
  Select -> word8 0x1B
  (GetLocal l) -> word8 0x20 <> localIndex l
  (SetLocal l) -> word8 0x21 <> localIndex l
  (TeeLocal l) -> word8 0x22 <> localIndex l
  (GetGlobal l) -> word8 0x23 <> globalIndex l
  (SetGlobal l) -> word8 0x24 <> globalIndex l
  (I32Load m) -> word8 0x28 <> memarg m
  (I64Load m) -> word8 0x29 <> memarg m
  (F32Load m) -> word8 0x2A <> memarg m
  (F64Load m) -> word8 0x2B <> memarg m
  (I32Load8S m) -> word8 0x2C <> memarg m
  (I32Load8U m) -> word8 0x2D <> memarg m
  (I32Load16S m) -> word8 0x2E <> memarg m
  (I32Load16U m) -> word8 0x2F <> memarg m
  (I64Load8S m) -> word8 0x30 <> memarg m
  (I64Load8U m) -> word8 0x31 <> memarg m
  (I64Load16S m) -> word8 0x32 <> memarg m
  (I64Load16U m) -> word8 0x33 <> memarg m
  (I64Load32S m) -> word8 0x34 <> memarg m
  (I64Load32U m) -> word8 0x35 <> memarg m
  (I32Store m) -> word8 0x36 <> memarg m
  (I64Store m) -> word8 0x37 <> memarg m
  (F32Store m) -> word8 0x38 <> memarg m
  (F64Store m) -> word8 0x39 <> memarg m
  (I32Store8 m) -> word8 0x3A <> memarg m
  (I32Store16 m) -> word8 0x3B <> memarg m
  (I64Store8 m) -> word8 0x3C <> memarg m
  (I64Store16 m) -> word8 0x3D <> memarg m
  (I64Store32 m) -> word8 0x3E <> memarg m
  CurrentMemory -> word8 0x3F <> word8 0x00
  GrowMemory -> word8 0x40 <> word8 0x00
  (I32Const n) -> word8 0x41 <> signed n
  (I64Const n) -> word8 0x42 <> signed n
  (F32Const n) -> word8 0x43 <> float n
  (F64Const n) -> word8 0x44 <> double n
  I32Eqz -> word8 0x45
  (IRelOp BS32 IEq) -> word8 0x46
  (IRelOp BS32 INe) -> word8 0x47
  (IRelOp BS32 ILtS) -> word8 0x48
  (IRelOp BS32 ILtU) -> word8 0x49
  (IRelOp BS32 IGtS) -> word8 0x4A
  (IRelOp BS32 IGtU) -> word8 0x4B
  (IRelOp BS32 ILeS) -> word8 0x4C
  (IRelOp BS32 ILeU) -> word8 0x4D
  (IRelOp BS32 IGeS) -> word8 0x4E
  (IRelOp BS32 IGeU) -> word8 0x4F
  I64Eqz -> word8 0x50
  (IRelOp BS64 IEq) -> word8 0x51
  (IRelOp BS64 INe) -> word8 0x52
  (IRelOp BS64 ILtS) -> word8 0x53
  (IRelOp BS64 ILtU) -> word8 0x54
  (IRelOp BS64 IGtS) -> word8 0x55
  (IRelOp BS64 IGtU) -> word8 0x56
  (IRelOp BS64 ILeS) -> word8 0x57
  (IRelOp BS64 ILeU) -> word8 0x58
  (IRelOp BS64 IGeS) -> word8 0x59
  (IRelOp BS64 IGeU) -> word8 0x5A
  (FRelOp BS32 FEq) -> word8 0x5B
  (FRelOp BS32 FNe) -> word8 0x5C
  (FRelOp BS32 FLt) -> word8 0x5D
  (FRelOp BS32 FGt) -> word8 0x5E
  (FRelOp BS32 FLe) -> word8 0x5F
  (FRelOp BS32 FGe) -> word8 0x60
  (FRelOp BS64 FEq) -> word8 0x61
  (FRelOp BS64 FNe) -> word8 0x62
  (FRelOp BS64 FLt) -> word8 0x63
  (FRelOp BS64 FGt) -> word8 0x64
  (FRelOp BS64 FLe) -> word8 0x65
  (FRelOp BS64 FGe) -> word8 0x66
  (IUnOp BS32 IClz) -> word8 0x67
  (IUnOp BS32 ICtz) -> word8 0x68
  (IUnOp BS32 IPopcnt) -> word8 0x69
  (IBinOp BS32 IAdd) -> word8 0x6A
  (IBinOp BS32 ISub) -> word8 0x6B
  (IBinOp BS32 IMul) -> word8 0x6C
  (IBinOp BS32 IDivS) -> word8 0x6D
  (IBinOp BS32 IDivU) -> word8 0x6E
  (IBinOp BS32 IRemS) -> word8 0x6F
  (IBinOp BS32 IRemU) -> word8 0x70
  (IBinOp BS32 IAnd) -> word8 0x71
  (IBinOp BS32 IOr) -> word8 0x72
  (IBinOp BS32 IXor) -> word8 0x73
  (IBinOp BS32 IShl) -> word8 0x74
  (IBinOp BS32 IShrS) -> word8 0x75
  (IBinOp BS32 IShrU) -> word8 0x76
  (IBinOp BS32 IRotl) -> word8 0x77
  (IBinOp BS32 IRotr) -> word8 0x78
  (IUnOp BS64 IClz) -> word8 0x79
  (IUnOp BS64 ICtz) -> word8 0x7A
  (IUnOp BS64 IPopcnt) -> word8 0x7B
  (IBinOp BS64 IAdd) -> word8 0x7C
  (IBinOp BS64 ISub) -> word8 0x7D
  (IBinOp BS64 IMul) -> word8 0x7E
  (IBinOp BS64 IDivS) -> word8 0x7F
  (IBinOp BS64 IDivU) -> word8 0x80
  (IBinOp BS64 IRemS) -> word8 0x81
  (IBinOp BS64 IRemU) -> word8 0x82
  (IBinOp BS64 IAnd) -> word8 0x83
  (IBinOp BS64 IOr) -> word8 0x84
  (IBinOp BS64 IXor) -> word8 0x85
  (IBinOp BS64 IShl) -> word8 0x86
  (IBinOp BS64 IShrS) -> word8 0x87
  (IBinOp BS64 IShrU) -> word8 0x88
  (IBinOp BS64 IRotl) -> word8 0x89
  (IBinOp BS64 IRotr) -> word8 0x8A
  (FUnOp BS32 FAbs) -> word8 0x8B
  (FUnOp BS32 FNeg) -> word8 0x8C
  (FUnOp BS32 FCeil) -> word8 0x8D
  (FUnOp BS32 FFloor) -> word8 0x8E
  (FUnOp BS32 FTrunc) -> word8 0x8F
  (FUnOp BS32 FNearest) -> word8 0x90
  (FUnOp BS32 FSqrt) -> word8 0x91
  (FBinOp BS32 FAdd) -> word8 0x92
  (FBinOp BS32 FSub) -> word8 0x93
  (FBinOp BS32 FMul) -> word8 0x94
  (FBinOp BS32 FDiv) -> word8 0x95
  (FBinOp BS32 FMin) -> word8 0x96
  (FBinOp BS32 FMax) -> word8 0x97
  (FBinOp BS32 FCopySign) -> word8 0x98
  (FUnOp BS64 FAbs) -> word8 0x99
  (FUnOp BS64 FNeg) -> word8 0x9A
  (FUnOp BS64 FCeil) -> word8 0x9B
  (FUnOp BS64 FFloor) -> word8 0x9C
  (FUnOp BS64 FTrunc) -> word8 0x9D
  (FUnOp BS64 FNearest) -> word8 0x9E
  (FUnOp BS64 FSqrt) -> word8 0x9F
  (FBinOp BS64 FAdd) -> word8 0xA0
  (FBinOp BS64 FSub) -> word8 0xA1
  (FBinOp BS64 FMul) -> word8 0xA2
  (FBinOp BS64 FDiv) -> word8 0xA3
  (FBinOp BS64 FMin) -> word8 0xA4
  (FBinOp BS64 FMax) -> word8 0xA5
  (FBinOp BS64 FCopySign) -> word8 0xA6
  I32WrapI64 -> word8 0xA7
  (ITruncFS BS32 BS32) -> word8 0xA8
  (ITruncFU BS32 BS32) -> word8 0xA9
  (ITruncFS BS32 BS64) -> word8 0xAA
  (ITruncFU BS32 BS64) -> word8 0xAB
  I64ExtendSI32 -> word8 0xAC
  I64ExtendUI32 -> word8 0xAD
  (ITruncFS BS64 BS32) -> word8 0xAE
  (ITruncFU BS64 BS32) -> word8 0xAF
  (ITruncFS BS64 BS64) -> word8 0xB0
  (ITruncFU BS64 BS64) -> word8 0xB1
  (FConvertIS BS32 BS32) -> word8 0xB2
  (FConvertIU BS32 BS32) -> word8 0xB3
  (FConvertIS BS32 BS64) -> word8 0xB4
  (FConvertIU BS32 BS64) -> word8 0xB5
  F32DemoteF64 -> word8 0xB6
  (FConvertIS BS64 BS32) -> word8 0xB7
  (FConvertIU BS64 BS32) -> word8 0xB8
  (FConvertIS BS64 BS64) -> word8 0xB9
  (FConvertIU BS64 BS64) -> word8 0xBA
  F64PromoteF32 -> word8 0xBB
  (IReinterpretF b) -> case b of
    BS32 -> word8 0xBC
    BS64 -> word8 0xBD
  (FReinterpretI b) -> case b of
    BS32 -> word8 0xBE
    BS64 -> word8 0xBF
  I32Extend8S -> word8 0xC0
  I32Extend16S -> word8 0xC1
  I64Extend8S -> word8 0xC2
  I64Extend16S -> word8 0xC3
  I64Extend32S -> word8 0xC4
  (ITruncSatS b1 b2) ->
    word8 0xFC <> case (b1, b2) of
      (BS32, BS32) -> unsigned (0 :: Word32)
      (BS32, BS64) -> unsigned (2 :: Word32)
      (BS64, BS32) -> unsigned (4 :: Word32)
      (BS64, BS64) -> unsigned (6 :: Word32)
  (ITruncSatU b1 b2) ->
    word8 0xFC <> case (b1, b2) of
      (BS32, BS32) -> unsigned (1 :: Word32)
      (BS32, BS64) -> unsigned (3 :: Word32)
      (BS64, BS32) -> unsigned (5 :: Word32)
      (BS64, BS64) -> unsigned (7 :: Word32)

importDesc :: ImportDesc -> Builder
importDesc (ImportFunc x) = word8 0x00 <> typeIndex x
importDesc (ImportTable tt) = word8 0x01 <> tableType tt
importDesc (ImportMemory mt) = word8 0x02 <> memory mt
importDesc (ImportGlobal gt) = word8 0x03 <> globalType gt

import_ :: Import -> Builder
import_ (Import m n desc) = name m <> name n <> importDesc desc

table :: Table -> Builder
table (Table tt) = tableType tt

global :: Global -> Builder
global (Global gt e) = globalType gt <> expression e

export :: Export -> Builder
export (Export n d) = name n <> exportDesc d

exportDesc :: ExportDesc -> Builder
exportDesc (ExportFunc x) = word8 0x00 <> funcIndex x
exportDesc (ExportTable x) = word8 0x01 <> tableIndex x
exportDesc (ExportMemory x) = word8 0x02 <> memoryIndex x
exportDesc (ExportGlobal x) = word8 0x03 <> globalIndex x

start :: StartFunction -> Builder
start (StartFunction f) = funcIndex f

elemSegment :: ElemSegment -> Builder
elemSegment (ElemSegment x e ys) = tableIndex x <> expression e <> vector funcIndex ys

dataSegment :: DataSegment -> Builder
dataSegment (DataSegment x e bs) = memoryIndex x <> expression e <> vector word8 (BS.unpack bs)

local :: (Int, ValueType) -> Builder
local (n, vt) = unsigned (fromIntegral n :: Word32) <> valueType vt

code :: ([(Int, ValueType)], Expression) -> Builder
code (ls, e) = vector local ls <> expression e

section :: Int -> Builder -> Builder
section n contents = word8 (fromIntegral n) <> contents

typeSection :: [FuncType] -> Builder
typeSection ts = section 1 (vector funcType ts)

importSection :: [Import] -> Builder
importSection is = section 2 (vector import_ is)

functionSection :: [TypeIndex] -> Builder
functionSection tis = section 3 (vector typeIndex tis)

tableSection :: [Table] -> Builder
tableSection ts = section 4 (vector table ts)

memorySection :: [Memory] -> Builder
memorySection ms = section 5 (vector memory ms)

globalSection :: [Global] -> Builder
globalSection gs = section 6 (vector global gs)

exportSection :: [Export] -> Builder
exportSection es = section 7 (vector export es)

startSection :: Maybe StartFunction -> Builder
startSection Nothing = mempty
startSection (Just st) = section 8 (start st)

elemSection :: [ElemSegment] -> Builder
elemSection es = section 9 (vector elemSegment es)

codeSection :: [([ValueType], Expression)] -> Builder
codeSection cs = section 10 (vector code cs')
  where
    (vts, es) = unzip cs
    newVts = go 0 vts
    cs' = zip newVts es

    go _ [] = []
    go n (x : xs) = zip [n ..] x : go (n + length x) xs

dataSection :: [DataSegment] -> Builder
dataSection ds = section 11 (vector dataSegment ds)

module_ :: Module -> Builder
module_ (Module types functions tables mems globals elems datas s imports exports) =
  magic <> version
    <> typeSection types
    <> importSection imports
    <> functionSection fs
    <> tableSection tables
    <> memorySection mems
    <> globalSection globals
    <> exportSection exports
    <> startSection s
    <> elemSection elems
    <> codeSection codes
    <> dataSection datas
  where
    magic = foldMap word8 [0x00, 0x61, 0x73, 0x6D]
    version = foldMap word8 [0x01, 0x00, 0x00, 0x00]
    fs = map functionType functions
    codes = map (\(Function _ (LocalsType ls) e) -> (ls, e)) functions