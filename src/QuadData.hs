module QuadData where
import AbsLatte
import qualified Data.Map as Map
import qualified Data.Set as Set
data QArgument = QaVar Var | QaConst Int | QaEmpty | QaList [QArgument]
    deriving (Eq, Ord, Show, Read)

data QuadFunction = QuadFunction Ident [Quad] Int Int (Map.Map Int (Int, Type))
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
    | OpJmp String
    | OpGoToIfFalse String
    | OpAllocString Int Int
    | OpLabel String
    | OpCall String

    deriving (Eq, Ord, Show, Read)

data Quad = Quad4 Var Op QArgument QArgument | QuadNoAssign Op QArgument QArgument
    deriving (Eq, Ord, Show, Read)

data Reg = Rax | Rbx | Rcx | Rdx | Rsi | Rdi | R8 | R9 | R10 | R11 | Rsp | Rbp
    deriving (Eq, Ord, Show, Read)

data Location = LocReg Reg | LocVar Int | LocConst Int
    deriving (Eq, Ord, Show, Read)

data AmdArg = AAReg Reg | AAMem Int | AAConst Int
    deriving (Eq, Ord, Show, Read)

workingRegisters = [Rbx, Rcx, Rsi, Rdi, R8, R9, R10, R11]

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
         | ARet
         | ALab String
         | AJmp String

         | ALeave
          deriving (Eq, Ord, Show, Read)

type AddressDescriptions = Map.Map Int (Set.Set Reg, Set.Set Var)
