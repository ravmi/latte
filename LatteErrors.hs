module LatteErrors where
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

varUninitialized :: Ident -> String
varUninitialized (Ident varName) = "`" ++ varName ++  "`" ++ " uninitialized"

alreadyDeclared :: Ident -> String
alreadyDeclared (Ident varName) = "`" ++ varName ++  "`" ++ " declared before in the same scope"

unexpected :: String
unexpected = "Something very bad happened"

badApply :: Ident -> [Type] -> [Type] -> String
badApply (Ident funName) okTypes badTypes = errLog where
    why = "Wrong usage of function: `" ++ funName ++ "`"
    expected = "expected arguments: " ++ (show okTypes)
    used = "used types: " ++ (show badTypes)
    errLog = why ++ "; " ++ expected ++ "; " ++ used

badTypesSuggestion :: (Show a, Show b) => String -> a -> b -> String
badTypesSuggestion opName okTypes badTypes = errLog where
    why = "Wrong usage of " ++ opName
    expected = "expected type: " ++ (show okTypes)
    used = "used types: " ++ (show badTypes)
    errLog = why ++ "; " ++ expected ++ "; " ++ used

badTypes :: (Show a) => String -> a -> String
badTypes opName badTypes = errLog where
    why = "Wrong usage of " ++ opName
    used = "used types: " ++ (show badTypes)
    errLog = why ++ "; " ++ used

badReturn :: Ident -> Type -> String
badReturn (Ident funName) retType = errLog where
    why = "Function `" ++ funName ++ "` doesn't return correct type in all possible runs"
    expected = "expected type: " ++ (show retType)
    errLog = why ++ "; " ++ expected

repeatingArgs :: [Arg] -> String
repeatingArgs arguments = errLog where
    why = "Some of the argument names repeat"
    showArg (Arg varType (Ident varName)) = show varType ++ " " ++ varName
    argList = map showArg arguments
    errLog = why ++ ": " ++ "[" ++ (intercalate ", " argList) ++ "]"