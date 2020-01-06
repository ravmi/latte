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



translateRegister :: Register -> String
translateRegister (Mem i) = (show ((i - 2) * 4)) ++ "(%ebp)"
translateRegister (RegInt i) = "$" ++ (show i)
translateRegister (RegBool True) = "1"
translateRegister (RegBool False) = "0"
translateRegister Reg1 = "%eax"
translateRegister Reg2 = "%ebx"
translateRegister Reg3 = "%edx"


translateQuadruple :: Quadruple -> String
translateQuadruple (QAdd r1 r2) = unwords ["addl", (translateRegister r1), (translateRegister r2)]
translateQuadruple (QSub r1 r2) = unwords ["subl", (translateRegister r1), (translateRegister r2)]
translateQuadruple (QMul r1 r2) = unwords ["mull", (translateRegister r1), (translateRegister r2)]
translateQuadruple (QAss r1 r2) = unwords ["movl", (translateRegister r1), (translateRegister r2)]
translateQuadruple (QRetV) = "ret"
translateQuadruple (QRet r) = (unwords ["movl", (translateRegister r), "%eax"]) ++ "\nret"
translateQuadruple (QMov r1 r2) = unwords ["movl", (translateRegister r1), (translateRegister r2)]


translateFunction :: QBlock -> String
translateFunction (QBlock (Ident fname) quads locals) = intercalate "\n" allCode where
    prolog = ["push %ebp", "movl %esp, %ebp", "subl $" ++ show(locals * 4) ++ ", %esp"]
    content = map translateQuadruple quads
    epilogue = ["leave", "ret"]
    code = prolog ++ content ++ epilogue
    allCode = [fname ++ ":"] ++ map ("    "++) code

translateProgram :: [QBlock] -> String
translateProgram code = intercalate "\n" (map translateFunction code)
