module QuadData where
import AbsLatte
import ASM
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.List (intercalate)


data QArgument = QaVar Var | QaConst Int | QaEmpty | QaList [QArgument] | QaConstStr StaticLocation
    deriving (Eq, Ord, Read, Show)

printQa (QaVar v) = "v" ++ (show v)
printQa (QaConst i) = "$" ++ (show i)
printQa (QaEmpty) = ""
printQa (QaList l) = show l
printQa (QaConstStr x) = "string:" ++ (show x)

data QuadFunction = QuadFunction Ident [Quad] Int Int (Map.Map Int (Int, Type))
    deriving (Eq, Ord, Show, Read)

printQuadFunction (QuadFunction (Ident name) qs a b mem) = (name ++ "\n" ++ intercalate "\n" (map printQuad qs)) ++ "\n" ++ (show mem)

data Quad = Quad4 Var Op QArgument QArgument | QuadNoAssign Op QArgument QArgument
    deriving (Eq, Ord, Read, Show)

printQuad (Quad4 v op a1 a2) = "v" ++ (show v) ++ " = " ++ (printQa a1) ++ " " ++ (printOp op) ++ " "  ++ (printQa a2)
printQuad (QuadNoAssign op a1 a2) = (printQa a1) ++ " " ++ (printOp op) ++ " " ++ (printQa a2)

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
    | OpGoToIfTrue String
    | OpAllocString Int Int
    | OpLabel String
    | OpCall String

    -- x (OpLoadFromHeap size) where _: loads *where to a temporary value (doesn't change memory)
    | OpLoadFromHeap --usefful (%rax)

    -- x OpWhereOnHeap t i: calculates where t[i] is in the memory
    -- | OpWhereOnHeap

    -- OpSaveToHeap where what: saves what to *where (remember that all are ints/indexes)
    | OpSaveToHeap

    -- x (OpAssFromHeap size) where _: loads *where or t.i from memory and saves it to memory where x is
    -- | OpAssFromHeap -- probably unnecessary and can use assignment for nomal variables
    --
    -- alloc
    deriving (Eq, Ord, Read, Show)

printOp (OpAdd) = "+"
printOp (OpSub) = "-"
printOp OpMul = "*"
printOp OpDiv = "/"
printOp OpMod = "%"
printOp OpCmpIntLt = "<"
printOp OpCmpIntLe = "<="
printOp OpCmpIntGt = ">"
printOp OpCmpIntGe = ">="
printOp OpCmpIntEq = "=="
printOp OpCmpIntNe = "!="
printOp OpNeg = "not"
printOp OpAnd = "and"
printOp OpOr = "or"
printOp OpAssVar = "."
printOp OpRet = "return"
printOp (OpJmp s) = "jmp " ++ s
printOp (OpCall s) = "call " ++ s
printOp (OpGoToIfFalse str) = "gotoiffalse " ++ str
printOp (OpLabel str) = "label: " ++ str
printOp _ = "not implemented"

type Var = Int
type AddressDescriptions = Map.Map Int (Set.Set Reg, Set.Set Var)
