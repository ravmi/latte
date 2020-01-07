module Asm64 where

import Control.Category
import Data.Label hiding (get)
import Prelude hiding ((.), id)
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.IO
import LexLatte
import ParLatte
import SkelLatte
import PrintLatte
import AbsLatte
import ErrM
import ImdLatte
--rdx, rsi, rsi, rdx <- better don't use it yet

translateRegister :: Register -> String

translateRegister (Mem i) = case i < 0 of
    True -> (show ((1 - i) * 8)) ++ "(%rbp)"
    False -> (show ((-i - 1) * 8)) ++ "(%rbp)"
translateRegister (RegInt i) = "$" ++ (show i)

translateRegister (RegBool True) = "$-1"

translateRegister (RegBool False) = "$0"

translateRegister (Reg 1) = "%r12"
translateRegister (Reg 2) = "%r13"
translateRegister (Reg 3) = "%r14"
translateRegister (Reg 4) = "%r15"
translateRegister (Reg 5) = "%rbx"

--rax, rcx, rdx, rsi, rdi, r8-r11 are caller saved
--RDI,RSI,RDX,RCX,R8,R9 - argumenty funkcji

translateQuadruple :: Quadruple -> [String]
translateQuadruple (QAdd r1 r2) = ["addq " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QSub r1 r2) = ["subq " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QMul r1 r2) = ["imulq " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QDiv r1 r2) = ["movq " ++ tr1 ++ ", %rax", "idivq " ++ tr2, "movq %rax, " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QMod r1 r2) = ["movq " ++ tr1 ++ ", %rax", "idivq " ++ tr2, "movq %rdx, " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QAss r1 r2) = ["movq " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QInc r) = ["incq " ++ tr] where
    tr = translateRegister r

translateQuadruple (QDec r) = ["decq " ++ tr] where
    tr = translateRegister r

translateQuadruple (QRet r) = [copyToEax, "leave", "ret"] where
    copyToEax = "movq " ++ translateRegister r ++ ", %rax"

translateQuadruple (QRetV) = ["leave", "ret"]

translateQuadruple (QLab (Ident name)) = [name ++ ":"]

translateQuadruple (QJmp (Ident name)) = ["jmp " ++ name]

translateQuadruple (QCmpJe (Ident name) r1 r2) = ["cmp " ++ tr2 ++ ", " ++ tr1, "je " ++ name] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QCmpJne (Ident name) r1 r2) = ["cmp " ++ tr2 ++ ", " ++ tr1, "jne " ++ name] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QCmpJg (Ident name) r1 r2) = ["cmp " ++ tr2 ++ ", " ++ tr1, "jg " ++ name] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QCmpJge (Ident name) r1 r2) = ["cmp " ++ tr2 ++ ", " ++ tr1, "jge " ++ name] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QCmpJL (Ident name) r1 r2) = ["cmp " ++ tr2 ++ ", " ++ tr1, "jl " ++ name] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QNeg r) = ["negq " ++ (translateRegister r)]

translateQuadruple (QAnd r1 r2) = ["andq " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QOr r1 r2) = ["orq " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QNot r) = ["notq " ++ (translateRegister r)]

translateQuadruple (QSwap r1 r2) = ["xchgq " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QXor r1 r2) = ["xorq " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QCmpGt r1 r2) = ["subq " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QCmpNe r1 r2) = ["subq " ++ tr1 ++ ", " ++ tr2, "fabs " ++ tr2, "neg " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QLoad r1 r2) = ["movq " ++ (translateRegister r1) ++ ", " ++ (translateRegister r2)]

translateQuadruple (QAlloc r i) = ["movq $" ++ num ++ ", %rdi", "call malloc", "movq %rax, " ++ tr] where
    num = show i
    tr = translateRegister r

--putFunctionArgument :: (Register, Int) -> String
--putFunctionArgument r i = let tr = translateRegister r in case i of
--    1 -> ["movq " ++ tr ++ ", rdi"
--    2 -> ["movq " ++ tr ++ ", rsi"
--    3 -> ["movq " ++ tr ++ ", rdx"
--    4 -> ["movq " ++ tr ++ ", rcx"
--    5 -> ["movq " ++ tr ++ ", r8"
--    6 -> ["movq " ++ tr ++ ", r9"
--    _ -> ["pushq " ++ tr"

--translateQuadruple (QCall function) = []


translateFunction :: QBlock -> String
translateFunction (QBlock (Ident fname) quads locals) = intercalate "\n" allCode where
    prolog = ["push %ebp", "movq %rsp, %rbp", "subq $" ++ show(locals * 4) ++ ", %rsp"]
    content = concat $ map translateQuadruple quads
    code = prolog ++ content
    allCode = [fname ++ ":"] ++ map ("    "++) code

translateProgram :: [QBlock] -> String
translateProgram code = intercalate "\n" (map translateFunction code)
