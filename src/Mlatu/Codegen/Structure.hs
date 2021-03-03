{-# LANGUAGE DuplicateRecordFields #-}

module Mlatu.Codegen.Structure where

import Relude

data BitSize = BS32 | BS64
  deriving (Show, Eq)

data IUnOp = IClz | ICtz | IPopcnt
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data FUnOp
  = FAbs
  | FNeg
  | FCeil
  | FFloor
  | FTrunc
  | FNearest
  | FSqrt
  deriving (Show, Eq)

data FBinOp
  = FAdd
  | FSub
  | FMul
  | FDiv
  | FMin
  | FMax
  | FCopySign
  deriving (Show, Eq)

data FRelOp
  = FEq
  | FNe
  | FLt
  | FGt
  | FLe
  | FGe
  deriving (Show, Eq)

data MemArg = MemArg
  { offset :: Word32,
    align :: Word32
  }
  deriving (Show, Eq)

newtype LabelIndex = LabelIndex Word32
  deriving (Show, Eq)

newtype FuncIndex = FuncIndex Word32
  deriving (Show, Eq)

newtype TypeIndex = TypeIndex Word32
  deriving (Show, Eq)

newtype LocalIndex = LocalIndex Word32
  deriving (Show, Eq)

newtype GlobalIndex = GlobalIndex Word32
  deriving (Show, Eq)

newtype MemoryIndex = MemoryIndex Word32
  deriving (Show, Eq)

newtype TableIndex = TableIndex Word32
  deriving (Show, Eq)

data ValueType
  = I32
  | I64
  | F32
  | F64
  deriving (Show, Eq)

newtype ResultType = ResultType [ValueType]
  deriving (Show, Eq)

newtype ParamsType = ParamsType [ValueType]
  deriving (Show, Eq)

newtype LocalsType = LocalsType [ValueType]
  deriving (Show, Eq)

data FuncType = FuncType
  { params :: ParamsType,
    results :: ResultType
  }
  deriving (Show, Eq)

data BlockType
  = IndexBlock TypeIndex
  | ValueBlock (Maybe ValueType)
  deriving (Show, Eq)

data Instruction
  = Unreachable
  | Nop
  | Block
      { typ :: BlockType,
        body :: Expression
      }
  | Loop
      { typ :: BlockType,
        body :: Expression
      }
  | If
      { typ :: BlockType,
        true :: Expression,
        false :: Maybe Expression
      }
  | Br LabelIndex
  | BrIf LabelIndex
  | BrTable [LabelIndex] LabelIndex
  | Return
  | Call FuncIndex
  | CallIndirect TypeIndex
  | Drop
  | Select
  | GetLocal LocalIndex
  | SetLocal LocalIndex
  | TeeLocal LocalIndex
  | GetGlobal GlobalIndex
  | SetGlobal GlobalIndex
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
  | I32Const Int32
  | I64Const Int64
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
  | I32Extend8S
  | I32Extend16S
  | I64Extend8S
  | I64Extend16S
  | I64Extend32S
  | ITruncSatS BitSize BitSize
  | ITruncSatU BitSize BitSize
  deriving (Show, Eq)

newtype Expression = Expression [Instruction]
  deriving (Show, Eq)

data Function = Function
  { functionType :: TypeIndex,
    localTypes :: LocalsType,
    functionBody :: Expression
  }
  deriving (Show, Eq)

data Limit = Limit Word32 (Maybe Word32)
  deriving (Show, Eq)

data ElemType = AnyFunc
  deriving (Show, Eq)

data TableType = TableType Limit ElemType
  deriving (Show, Eq)

newtype Table = Table TableType
  deriving (Show, Eq)

newtype Memory = Memory Limit
  deriving (Show, Eq)

data GlobalType
  = Const ValueType
  | Mut ValueType
  deriving (Show, Eq)

data Global = Global
  { mutability :: GlobalType,
    initializer :: Expression
  }
  deriving (Show, Eq)

data ElemSegment = ElemSegment
  { tableIdx :: TableIndex,
    elemOffset :: Expression,
    funcIndexes :: [FuncIndex]
  }
  deriving (Show, Eq)

data DataSegment = DataSegment
  { memIndex :: MemoryIndex,
    dataOffset :: Expression,
    chunck :: ByteString
  }
  deriving (Show, Eq)

newtype StartFunction = StartFunction FuncIndex
  deriving (Show, Eq)

data ExportDesc
  = ExportFunc FuncIndex
  | ExportTable TableIndex
  | ExportMemory MemoryIndex
  | ExportGlobal GlobalIndex
  deriving (Show, Eq)

data Export = Export
  { exportName :: Text,
    desc :: ExportDesc
  }
  deriving (Show, Eq)

data ImportDesc
  = ImportFunc TypeIndex
  | ImportTable TableType
  | ImportMemory Memory
  | ImportGlobal GlobalType
  deriving (Show, Eq)

data Import = Import
  { sourceModule :: Text,
    importName :: Text,
    desc :: ImportDesc
  }
  deriving (Show, Eq)

data Module = Module
  { types :: [FuncType],
    functions :: [Function],
    tables :: [Table],
    mems :: [Memory],
    globals :: [Global],
    elems :: [ElemSegment],
    datas :: [DataSegment],
    startFun :: Maybe StartFunction,
    imports :: [Import],
    exports :: [Export]
  }
  deriving (Show, Eq)

emptyModule :: Module
emptyModule =
  Module
    { types = [],
      functions = [],
      tables = [],
      mems = [],
      globals = [],
      elems = [],
      datas = [],
      startFun = Nothing,
      imports = [],
      exports = []
    }
