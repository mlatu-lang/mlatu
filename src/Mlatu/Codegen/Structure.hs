{-# LANGUAGE DuplicateRecordFields #-}

module Mlatu.Codegen.Structure where

import Relude

data BitSize = BS32 | BS64

data IUnOp = IClz | ICtz | IPopcnt

data IBinOp
  = IAdd
  | ISub
  | IMul
  | IDivU
  | IDivS
  | IRemU
  | IRemS
  | IAnd
  | IOr
  | IXor
  | IShl
  | IShrU
  | IShrS
  | IRotl
  | IRotr

data IRelOp
  = IEq
  | INe
  | ILtU
  | ILtS
  | IGtU
  | IGtS
  | ILeU
  | ILeS
  | IGeU
  | IGeS

data FUnOp
  = FAbs
  | FNeg
  | FCeil
  | FFloor
  | FTrunc
  | FNearest
  | FSqrt

data FBinOp
  = FAdd
  | FSub
  | FMul
  | FDiv
  | FMin
  | FMax
  | FCopySign

data FRelOp
  = FEq
  | FNe
  | FLt
  | FGt
  | FLe
  | FGe

data MemArg = MemArg
  { offset :: Natural,
    align :: Natural
  }

type LabelIndex = Natural

type FuncIndex = Natural

type TypeIndex = Natural

type LocalIndex = Natural

type GlobalIndex = Natural

type MemoryIndex = Natural

type TableIndex = Natural

data ValueType
  = I32
  | I64
  | F32
  | F64

type ResultType = [ValueType]

type ParamsType = [ValueType]

type LocalsType = [ValueType]

data FuncType = FuncType
  { params :: ParamsType,
    results :: ResultType
  }

data Instruction index
  = Unreachable
  | Nop
  | Block
      { resultType :: ResultType,
        body :: Expression
      }
  | Loop
      { resultType :: ResultType,
        body :: Expression
      }
  | If
      { resultType :: ResultType,
        true :: Expression,
        false :: Expression
      }
  | Br index
  | BrIf index
  | BrTable [index] index
  | Return
  | Call index
  | CallIndirect index
  | Drop
  | Select
  | GetLocal index
  | SetLocal index
  | TeeLocal index
  | GetGlobal index
  | SetGlobal index
  | I32Load MemArg
  | I64Load MemArg
  | F32Load MemArg
  | F64Load MemArg
  | I32Load8S MemArg
  | I32Load8U MemArg
  | I32Load16S MemArg
  | I32Load16U MemArg
  | I64Load8S MemArg
  | I64Load8U MemArg
  | I64Load16S MemArg
  | I64Load16U MemArg
  | I64Load32S MemArg
  | I64Load32U MemArg
  | I32Store MemArg
  | I64Store MemArg
  | F32Store MemArg
  | F64Store MemArg
  | I32Store8 MemArg
  | I32Store16 MemArg
  | I64Store8 MemArg
  | I64Store16 MemArg
  | I64Store32 MemArg
  | CurrentMemory
  | GrowMemory
  | I32Const Word32
  | I64Const Word64
  | F32Const Float
  | F64Const Double
  | IUnOp BitSize IUnOp
  | IBinOp BitSize IBinOp
  | I32Eqz
  | I64Eqz
  | IRelOp BitSize IRelOp
  | FUnOp BitSize FUnOp
  | FBinOp BitSize FBinOp
  | FRelOp BitSize FRelOp
  | I32WrapI64
  | ITruncFU BitSize BitSize
  | ITruncFS BitSize BitSize
  | I64ExtendSI32
  | I64ExtendUI32
  | FConvertIU BitSize BitSize
  | FConvertIS BitSize BitSize
  | F32DemoteF64
  | F64PromoteF32
  | IReinterpretF BitSize
  | FReinterpretI BitSize

type Expression = [Instruction Natural]

data Function = Function
  { funcType :: TypeIndex,
    localTypes :: LocalsType,
    body :: Expression
  }

data Limit = Limit Natural (Maybe Natural)

data ElemType = AnyFunc

data TableType = TableType Limit ElemType

newtype Table = Table TableType

newtype Memory = Memory Limit

data GlobalType
  = Const ValueType
  | Mut ValueType

data Global = Global
  { globalType :: GlobalType,
    initializer :: Expression
  }

data ElemSegment = ElemSegment
  { tableIndex :: TableIndex,
    offset :: Expression,
    funcIndexes :: [FuncIndex]
  }

data DataSegment = DataSegment
  { memIndex :: MemoryIndex,
    offset :: Expression,
    chunck :: ByteString
  }

newtype StartFunction = StartFunction FuncIndex

data ExportDesc
  = ExportFunc FuncIndex
  | ExportTable TableIndex
  | ExportMemory MemoryIndex
  | ExportGlobal GlobalIndex

data Export = Export
  { name :: Text,
    desc :: ExportDesc
  }

data ImportDesc
  = ImportFunc TypeIndex
  | ImportTable TableType
  | ImportMemory Limit
  | ImportGlobal GlobalType

data Import = Import
  { sourceModule :: Text,
    name :: Text,
    desc :: ImportDesc
  }

data Module = Module
  { types :: [FuncType],
    functions :: [Function],
    tables :: [Table],
    mems :: [Memory],
    globals :: [Global],
    elems :: [ElemSegment],
    datas :: [DataSegment],
    start :: Maybe StartFunction,
    imports :: [Import],
    exports :: [Export]
  }