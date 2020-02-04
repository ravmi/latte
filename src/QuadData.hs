module QuadData where
import AbsLatte
import ASM
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



data Location = LocReg Reg | LocVar Int | LocConst Int
    deriving (Eq, Ord, Show, Read)



type Var = Int



type AddressDescriptions = Map.Map Int (Set.Set Reg, Set.Set Var)
