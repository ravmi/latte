module Asm32 where

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

-- ecx, edx - general use
-- eax - pomocniczy w tej bibliotece

translateRegister :: Register -> String
translateRegister (Mem i) = if i < 0 then (show ((i - 2) * 4)) ++ "(%ebp)" else show (i * 4) ++ "(%ebp)"
translateRegister (RegInt i) = "$" ++ (show i)
translateRegister (RegBool True) = "1"
translateRegister (RegBool False) = "0"
translateRegister (Reg 1) = "%r1"
translateRegister (Reg 2) = "%r2"
--translateRegister Reg3 = "%eax edx tez nie moze byc"


translateQuadruple :: Quadruple -> [String]
translateQuadruple (QAdd r1 r2) = ["addl " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QSub r1 r2) = ["subl " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QMul r1 r2) = ["imull " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QDiv r1 r2) = ["movl " ++ tr1 ++ ", %eax", "idivl " ++ tr2, "movl %eax, " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QMod r1 r2) = ["movl " ++ tr1 ++ ", %eax", "idivl " ++ tr2, "movl %edx, " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QAss r1 r2) = ["movl " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QInc r) = ["incl " ++ tr] where
    tr = translateRegister r

translateQuadruple (QDec r) = ["decl " ++ tr] where
    tr = translateRegister r

translateQuadruple (QRet r) = [copyToEax, "leave", "ret"] where
    copyToEax = "movl " ++ translateRegister r ++ ", %eax"

translateQuadruple (QRetV) = ["leave", "ret"]

translateQuadruple (QLab (Ident name)) = ["name" ++ ":"]

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

translateQuadruple (QNeg r) = ["negl " ++ (translateRegister r)]

translateQuadruple (QAnd r1 r2) = ["andl " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QOr r1 r2) = ["orl " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QNot r) = ["notl " ++ (translateRegister r)]

translateQuadruple (QSwap r1 r2) = ["xchgl " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QXor r1 r2) = ["xorl " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QCmpGt r1 r2) = ["subl " ++ tr1 ++ ", " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QCmpNe r1 r2) = ["subl " ++ tr1 ++ ", " ++ tr2, "fabs " ++ tr2, "neg " ++ tr2] where
    tr1 = translateRegister r1
    tr2 = translateRegister r2

translateQuadruple (QLoad r1 r2) = ["movl " ++ (translateRegister r1) ++ ", " ++ (translateRegister r2)]

translateQuadruple (QAlloc r i) = ["movl $" ++ num ++ ", %edi", "call malloc", "movl %rax, " ++ tr]
    num = show i
    tr = translateRegister r


translateFunction :: QBlock -> String
translateFunction (QBlock (Ident fname) quads locals) = intercalate "\n" allCode where
    prolog = ["push %ebp", "movl %esp, %ebp", "subl $" ++ show(locals * 4) ++ ", %esp"]
    content = concat $ map translateQuadruple quads
    code = prolog ++ content
    allCode = [fname ++ ":"] ++ map ("    "++) code

translateProgram :: [QBlock] -> String
translateProgram code = intercalate "\n" (map translateFunction code)
