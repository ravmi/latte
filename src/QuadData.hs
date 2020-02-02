module QuadData where
import AbsLatte
data QArgument = QaVar Var | QaConst Int | QaEmpty | QaList [QArgument]
--Reg1 | Reg2 | Reg3 | Reg4 | Reg5
  deriving (Eq, Ord, Show, Read)


data QBlock = QBlock Ident [Quadruple] Int Int
  deriving (Eq, Ord, Show, Read)


type QCode = [Quadruple]

-- niech drugi argument to bedzie zawsze register (nie pamiec)

data Quadruple
    = QAdd QArgument QArgument
    | QSub QArgument QArgument
    | QMul QArgument QArgument
    | QDiv QArgument QArgument
    | QMod QArgument QArgument
    | QAss QArgument QArgument
    | QInc QArgument
    | QDec QArgument
    | QRet QArgument
    | QRetV
    | QLab Ident
    | QJmp Ident

    | QGoToIfEqual Ident QArgument QArgument
    | QGoToIfNotEqual Ident QArgument QArgument
    | QGoToIfGreater Ident QArgument QArgument
    | QGoToIfGreaterEqual Ident QArgument QArgument
    | QGoToIfLesser Ident QArgument QArgument
    | QGoToIfLesserEqual Ident QArgument QArgument


    | QNeg QArgument
    | QAnd QArgument QArgument
    | QOr QArgument QArgument
    | QXor QArgument QArgument
    | QNot QArgument
    | QSwap QArgument QArgument

    | QCmpIntLt QArgument QArgument
    | QCmpIntLe QArgument QArgument
    | QCmpIntGt QArgument QArgument
    | QCmpIntGe QArgument QArgument
    | QCmpIntEq QArgument QArgument
    | QCmpIntNe QArgument QArgument

    | QAlloc QArgument Int
    | QConcat QArgument QArgument

    | QCall Ident [QArgument]
    | QLoad QArgument QArgument
    | QStore QArgument QArgument
    deriving (Eq, Ord, Show, Read)


-- najpierw zaloz, ze nie uzywasz rejestrow ponownie, wszystko zapisuj w zmiennych lokalnych (tu bedzie problem)
-- rozbij na bloki, lista w postaci blok, zmienne lokalne

type BasicBlock = [Quadruple]











data Op
    = OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpMod
    | OpAss
    | OpInc
    | OpDec
    | OpRet
    | OpRetV

    | OpLab
    | OpJmp

    | OpGoToIfFalse

    | OpNeg
    | OpAnd
    | OpOr
    | OpXor
    | OpNot
    | OpSwap

    | OpCmpIntLt
    | OpCmpIntLe
    | OpCmpIntGt
    | OpCmpIntGe
    | OpCmpIntEq
    | OpCmpIntNe

    | OpLabel String

    | OpCall String



--- dla specjanych komen (assign, funcall) odzielne implementacje
--- nie wiem jeszcze co dla
    | OpConcat

    | OpLoad
    | OpStore
    deriving (Eq, Ord, Show, Read)

data Quad = Quad4 Var Op QArgument QArgument | QuadNoAssign Op QArgument QArgument
    deriving (Eq, Ord, Show, Read)

data Reg = Rax | Rbx | Rcx | Rdx
    deriving (Eq, Ord, Show, Read)

data Location = LocReg Reg | LocVar Int | LocConst Int
    deriving (Eq, Ord, Show, Read)

data AmdArg = AAReg Reg | AAVar Int | AAConst Int
    deriving (Eq, Ord, Show, Read)

allRegisters = [Rax, Rbx, Rcx, Rdx]

type Var = Int

