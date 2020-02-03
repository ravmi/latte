module QuadData where
import AbsLatte
data QArgument = QaVar Var | QaConst Int | QaEmpty | QaList [QArgument]
    deriving (Eq, Ord, Show, Read)

data QBlock = QBlock Ident [Quad] Int Int
    deriving (Eq, Ord, Show, Read)

data Op
    = OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpMod

    | OpCmpIntLt
    | OpCmpIntLe
    | OpCmpIntGt
    | OpCmpIntGe
    | OpCmpIntEq
    | OpCmpIntNe

    | OpNeg
    | OpAnd
    | OpOr
    | OpNot

    | OpAssVar

    | OpRet
    | OpRetV
    | OpLab
    | OpJmp String
    | OpGoToIfFalse String
    | OpAllocString Int Int
    | OpLabel String
    | OpCall String

    deriving (Eq, Ord, Show, Read)

data Quad = Quad4 Var Op QArgument QArgument | QuadNoAssign Op QArgument QArgument
    deriving (Eq, Ord, Show, Read)

data Reg = Rax | Rbx | Rcx | Rdx
    deriving (Eq, Ord, Show, Read)

data Location = LocReg Reg | LocVar Int | LocConst Int
    deriving (Eq, Ord, Show, Read)

data AmdArg = AAReg Reg | AAMem Int | AAConst Int
    deriving (Eq, Ord, Show, Read)

allRegisters = [Rax, Rbx, Rcx, Rdx]

type Var = Int

data ASM = APush AmdArg
         | APop AmdArg
         | ANeg AmdArg
         | ASub AmdArg AmdArg
         | AAdd AmdArg AmdArg
         | AMul AmdArg AmdArg
         | ADiv AmdArg
         | AAnd AmdArg AmdArg
         | AOr AmdArg AmdArg
         | AMov AmdArg AmdArg
         | ACdq
          deriving (Eq, Ord, Show, Read)
