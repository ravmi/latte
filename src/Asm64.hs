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
import Control.Monad.Reader

import QuadData
--rdx, rsi, rsi, rdx <- better don't use it yet

type Ev2 = Reader Int

translateRegister :: QArgument -> Ev2 String

--negative means that it has to be taken from the saved arguments
translateRegister (Mem i) = do
    locs <- ask
    case i < 0 of
        True -> return $ (show ((-locs - 1 + i) * 8)) ++ "(%rbp)"
        False -> return $ (show ((-i - 1) * 8)) ++ "(%rbp)"

translateRegister (RegInt i) = return $ "$" ++ (show i)

translateRegister (RegBool True) = return $ "$-1"

translateRegister (RegBool False) = return $ "$0"

translateRegister (Reg 1) = return $ "%r12"
translateRegister (Reg 2) = return $ "%r13"
translateRegister (Reg 3) = return $ "%r14"
translateRegister (Reg 4) = return $ "%r15"
translateRegister (Reg 5) = return $ "%rbx"

--rax, rcx, rdx, rsi, rdi, r8-r11 are caller saved
--RDI,RSI,RDX,RCX,R8,R9 - argumenty funkcji

translateQuadruple :: Quadruple -> Ev2 [String]
translateQuadruple (QAdd r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["addq " ++ tr1 ++ ", " ++ tr2]

translateQuadruple (QSub r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["subq " ++ tr1 ++ ", " ++ tr2]

translateQuadruple (QMul r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["imulq " ++ tr1 ++ ", " ++ tr2]

translateQuadruple (QDiv r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["movq " ++ tr1 ++ ", %rax", "idivq " ++ tr2, "movq %rax, " ++ tr2]

translateQuadruple (QMod r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["movq " ++ tr1 ++ ", %rax", "idivq " ++ tr2, "movq %rdx, " ++ tr2]

translateQuadruple (QAss r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["movq " ++ tr1 ++ ", " ++ tr2]

translateQuadruple (QInc r) = do
    tr <- translateRegister r
    return $ ["incq " ++ tr]

translateQuadruple (QDec r) = do
    tr <- translateRegister r
    return $ ["decq " ++ tr]

translateQuadruple (QRet r) = do
    tr <- translateRegister r
    return $ ["movq " ++ tr ++ ", %rax", "leave", "ret"]

translateQuadruple (QRetV) = return $ ["leave", "ret"]

translateQuadruple (QLab (Ident name)) = return $ ["." ++ name ++ ":"]

translateQuadruple (QJmp (Ident name)) = return $ ["jmp " ++ name]

translateQuadruple (QGoToIfEqual (Ident name) r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["cmp " ++ tr2 ++ ", " ++ tr1, "je " ++ name]

translateQuadruple (QGoToIfNotEqual (Ident name) r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["cmp " ++ tr2 ++ ", " ++ tr1, "jne " ++ name]

translateQuadruple (QGoToIfGreater (Ident name) r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["cmp " ++ tr2 ++ ", " ++ tr1, "jg " ++ name]

translateQuadruple (QGoToIfGreaterEqual (Ident name) r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["cmp " ++ tr2 ++ ", " ++ tr1, "jge " ++ name]

translateQuadruple (QGoToIfLesser (Ident name) r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["cmp " ++ tr2 ++ ", " ++ tr1, "jl " ++ name]

translateQuadruple (QNeg r) = do
    tr <- translateRegister r
    return $ ["negq " ++ tr]

translateQuadruple (QAnd r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["andq " ++ tr1 ++ ", " ++ tr2]

translateQuadruple (QOr r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["orq " ++ tr1 ++ ", " ++ tr2]

translateQuadruple (QNot r) = do
    tr <- translateRegister r
    return $ ["notq " ++ tr]

translateQuadruple (QSwap r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["xchgq " ++ tr1 ++ ", " ++ tr2]

translateQuadruple (QXor r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["xorq " ++ tr1 ++ ", " ++ tr2]

translateQuadruple (QLoad r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["movq " ++ tr1 ++ ", " ++ tr2]

translateQuadruple (QAlloc r i) = do
    tr <- translateRegister r
    return $ ["movq $" ++ (show i) ++ ", %rdi", "call malloc", "movq %rax, " ++ tr]

translateQuadruple (QCall (Ident fName) rs) = do
    pushArgs <- mapM pushArgument (zip rs [1..])
    popArgs <- mapM popArgument (zip rs [1..])
    return $ (concat pushArgs) ++ ["call " ++ fName] ++ (concat popArgs)

translateQuadruple (QConcat r1 r2) = translateQuadruple (QCall (Ident "_concat") [r1, r2])

translateQuadruple (QCmpIntLt r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["mov $-1, %rdx", "cmpq " ++ tr2 ++ ", " ++ tr1, "movq $0, " ++ tr2, "cmovlq %rdx, " ++ tr2]

translateQuadruple (QCmpIntLe r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["mov $-1, %rdx", "cmpq " ++ tr2 ++ ", " ++ tr1, "movq $0, " ++ tr2, "cmovleq %rdx, " ++ tr2]

translateQuadruple (QCmpIntGt r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["mov $-1, %rdx", "cmpq " ++ tr2 ++ ", " ++ tr1, "movq $0, " ++ tr2, "cmovgq %rdx, " ++ tr2]

translateQuadruple (QCmpIntGe r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["mov $-1, %rdx", "cmpq " ++ tr2 ++ ", " ++ tr1, "movq $0, " ++ tr2, "cmovgeq %rdx, " ++ tr2]

translateQuadruple (QCmpIntEq r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["mov $-1, %rdx", "cmpq " ++ tr2 ++ ", " ++ tr1, "movq $0, " ++ tr2, "cmoveq %rdx, " ++ tr2]

translateQuadruple (QCmpIntNe r1 r2) = do
    tr1 <- translateRegister r1
    tr2 <- translateRegister r2
    return $ ["mov $-1, %rdx", "cmpq " ++ tr2 ++ ", " ++ tr1, "movq $0, " ++ tr2, "cmovneq %rdx, " ++ tr2]

pushArgument :: (QArgument, Int) -> Ev2 [String]
pushArgument (r, i) = do
    tr <- translateRegister r
    case i of
        1 -> return ["movq " ++ tr ++ ", %rdi"]
        2 -> return ["movq " ++ tr ++ ", %rsi"]
        3 -> return ["movq " ++ tr ++ ", %rdx"]
        4 -> return ["movq " ++ tr ++ ", %rcx"]
        5 -> return ["movq " ++ tr ++ ", %r8"]
        6 -> return ["movq " ++ tr ++ ", %r9"]
        _ -> return ["pushq " ++ tr]

popArgument :: (QArgument, Int) -> Ev2 [String]
popArgument (r, i) = case i <= 6 of
    True -> return []
    False -> return ["pop %rdi"]

copyArgument :: Int -> [String]
copyArgument i = do
    case i of
        1 -> ["push %rdi"]
        2 ->  ["push %rsi"]
        3 ->  ["push %rdx"]
        4 ->  ["push %rcx"]
        5 ->  ["push %r8"]
        6 ->  ["push %r9"]
        _ ->  ["push " ++ show (-8 * 6) ++ "(%rbp)"]
--cleanup?

translateFunction :: QBlock -> String
translateFunction (QBlock (Ident fname) quads locals args) = intercalate "\n" allCode where
    prolog = ["push %rbp", "movq %rsp, %rbp", "subq $" ++ show((locals) * 8) ++ ", %rsp"]
    transl = ((flip runReader) locals) .  translateQuadruple
    content = concat $ map transl quads
    copyArgs = concat (mapM copyArgument [1..args])
    code = prolog ++ copyArgs ++ content
    allCode = [fname ++ ":"] ++ map ("    "++) code

translateProgram :: [QBlock] -> String
translateProgram code = "    .globl main\n" ++ intercalate "\n" (map translateFunction code) ++ "\n"
