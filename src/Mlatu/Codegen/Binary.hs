{-# OPTIONS_GHC -Wno-orphans #-}

module Mlatu.Codegen.Binary where

import Data.ByteString.Builder
import Mlatu.Codegen.Structure
import Relude hiding (Const)

unsigned :: (Integral a) => a -> Builder
unsigned n = case n `quot` 128 of
  0 -> word8 (fromIntegral r)
  q -> word8 (fromIntegral r + 128) <> unsigned q
  where
    r = n `rem` 128

signed :: (Integral a) => a -> Builder
signed _ = undefined

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

resultType :: ResultType -> Builder
resultType (ResultType ts) = vector valueType ts

funcType :: FuncType -> Builder
funcType (FuncType ps rs) = word8 0x60 <> resultType ps <> resultType rs

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

memarg :: MemArg -> Builder
memarg (MemArg o a) = unsigned a <> unsigned o

blockType :: BlockType -> Builder
blockType (ValueBlock (Just vt)) = valueType vt
blockType (ValueBlock Nothing) = word8 0x40
blockType (IndexBlock t) = typeIndex t

expression :: Expression -> Builder
expression (Expression ins) = foldMap instruction ins <> word8 0x0B

instruction :: Instruction -> Builder
instruction Unreachable = word8 0x00
instruction Nop = word8 0x01
instruction (Block bt expr) = word8 0x02 <> blockType bt <> expression expr <> word8 0x0B
instruction (Loop bt expr) = word8 0x03 <> blockType bt <> expression expr <> word8 0x0B
instruction (If bt true Nothing) = word8 0x04 <> blockType bt <> expression true <> word8 0x0B
instruction (If bt true (Just false)) = word8 0x04 <> blockType bt <> expression true <> word8 0x05 <> expression false <> word8 0x0B
instruction (Br l) = word8 0x0C <> labelIndex l
instruction (BrIf l) = word8 0x0D <> labelIndex l
instruction (BrTable ls l) = word8 0x0E <> vector labelIndex ls <> labelIndex l
instruction Return = word8 0x0F
instruction (Call f) = word8 0x10 <> funcIndex f
instruction (CallIndirect f) = word8 0x11 <> typeIndex f <> 0x00
instruction Drop = word8 0x1A
instruction Select = word8 0x1B
instruction (GetLocal l) = word8 0x20 <> localIndex l
instruction (SetLocal l) = word8 0x21 <> localIndex l
instruction (TeeLocal l) = word8 0x22 <> localIndex l
instruction (GetGlobal l) = word8 0x23 <> globalIndex l
instruction (SetGlobal l) = word8 0x24 <> globalIndex l
instruction (I32Load m) = word8 0x28 <> memarg m
instruction _ = error "Not supported yet"