module ImdLatte where

import AbsLatte

-- Haskell module generated by the BNF converter

data Register = Mem Int | RegInt Integer | RegBool Bool | Reg Int
--Reg1 | Reg2 | Reg3 | Reg4 | Reg5
  deriving (Eq, Ord, Show, Read)


data QBlock = QBlock Ident [Quadruple] Int
  deriving (Eq, Ord, Show, Read)


type QCode = [Quadruple]

data Quadruple
    = QAdd Register Register
    | QSub Register Register
    | QMul Register Register
    | QDiv Register Register
    | QMod Register Register
    | QAss Register Register
    | QInc Register
    | QDec Register
    | QRet Register
    | QRetV
    | QLab Ident
    | QJmp Ident

    | QCmpJe Ident Register Register
    | QCmpJne Ident Register Register
    | QCmpJg Ident Register Register
    | QCmpJge Ident Register Register
    | QCmpJL Ident Register Register
    | QCmpJLe Ident Register Register

    | QNeg Register
    | QAnd Register Register
    | QOr Register Register
    | QXor Register Register
    | QNot Register
    | QSwap Register Register

    | QCmpGt Register Register
    | QCmpNe Register Register

    | QAlloc Register Int
    | QConcat Register Register

    | QCall Ident
    | QPutArg Int Register
    | QPopArg Int
    | QLoad Register Register
    | QStore Register Register
    deriving (Eq, Ord, Show, Read)


-- najpierw zaloz, ze nie uzywasz rejestrow ponownie, wszystko zapisuj w zmiennych lokalnych (tu bedzie problem)
-- rozbij na bloki, lista w postaci blok, zmienne lokalne

