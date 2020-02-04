module QuadData where
import AbsLatte
import ASM
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.List (intercalate)

data QArgument = QaVar Var | QaConst Int | QaEmpty | QaList [QArgument] | QaConstStr String
    deriving (Eq, Ord, Read, Show)

printQa (QaVar v) = "v" ++ (show v)
printQa (QaConst i) = "$" ++ (show i)
printQa (QaEmpty) = ""
printQa (QaList l) = show l

data QuadFunction = QuadFunction Ident [Quad] Int Int (Map.Map Int (Int, Type))
    deriving (Eq, Ord, Show, Read)

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
    | OpAllocString Int Int
    | OpLabel String
    | OpCall String
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
printOp _ = "not implemented"

type Var = Int
type AddressDescriptions = Map.Map Int (Set.Set Reg, Set.Set Var)
