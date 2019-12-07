module Main where
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
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import System.IO
import LexLatte
import ParLatte
import SkelLatte
import PrintLatte
import AbsLatte
import ErrM


type Location = Integer
-- describes where are the variables located
type Locations = Map.Map Ident Location
-- describes what memory looks like (only types for now)
type Memory = Map.Map Location Type


-- in current state we want to keep locations of variables, state of the memory,
-- current free avaliable memory location, location of beginning of the block
-- and expected return type
data ProgState = ProgState { memory :: Memory
                 , locations :: Locations
                 , blockStart :: Location
                 , freeLocation :: Location
                 , expectRetType :: Type
                 }

type Eval ev = StateT ProgState (WriterT String (ExceptT String Identity)) ev
runEval mem ev = runIdentity $ runExceptT (runWriterT (evalStateT ev mem))

putMemory :: Location -> Type -> Eval ()
putMemory key val = do
    ProgState memory locs bs fl ert <- get
    put $ ProgState (Map.insert key val memory) locs bs fl ert

putLocations :: Ident -> Location -> Eval ()
putLocations key val = do
    ProgState memory locs bs fl ert <- get
    put $ ProgState memory (Map.insert key val locs) bs fl ert

putBlockStart :: Location -> Eval ()
putBlockStart new = do
    ProgState memory locs bs fl ert <- get
    put $ ProgState memory locs new fl ert

putFreeLocation :: Location -> Eval ()
putFreeLocation new = do
    ProgState memory locs bs fl ert <- get
    put $ ProgState memory locs bs new ert

putExpectRetType :: Type -> Eval ()
putExpectRetType new = do
    ProgState memory locs bs fl ert <- get
    put $ ProgState memory locs bs fl new



-- this allows to run block without affecting environument outisde of the block.
constState :: Eval a -> Eval a
constState p = do
    s <- get
    x <- p
    put s
    return x

-- @TODO
varUnitialized :: Ident -> String
varUnitialized x = ""
alreadyDeclared :: Ident -> String
alreadyDeclared x = ""

allocateVar :: Ident -> Type -> Eval ()
allocateVar varName varType = do
    freeLoc <- gets freeLocation
    locs <- gets locations
    putMemory freeLoc varType
    putLocations varName freeLoc
    return ()

lookupType :: Ident -> Eval Type
lookupType name = do
    locs <- gets locations
    mem <- gets memory
    case Map.lookup name locs of
        Just location -> case Map.lookup location mem of
            Just varType -> return varType
            _ -> throwError "something very bad"
        _ -> throwError $ varUnitialized name


declare :: Ident -> Type -> Eval ()
declare varName varType = do
    locs <- gets locations
    startLoc <- gets blockStart
    case Map.lookup varName locs of
        Just location -> case location < startLoc of
            True -> allocateVar varName varType
            False -> throwError $ alreadyDeclared varName
        _ -> allocateVar varName varType

deduceType :: Expr -> Eval (Type)
deduceType (EVar name) = lookupType name
deduceType (ELitInt _) = return Int
deduceType (ELitTrue) = return Bool
deduceType (ELitFalse) = return Bool
deduceType (EApp funName funArgs) = do
    Fun expectedRet expectedArgTypes  <- lookupType funName
    argTypes <- mapM deduceType funArgs
    case argTypes == expectedArgTypes of
        True -> return expectedRet
        False -> throwError "wrong arguments types applied"


deduceType (EString _) = return Str
deduceType (Neg expr) = do
    expType <- deduceType expr
    case expType of
        Int -> return $ Int
        _ -> throwError "Incorrect minus"
deduceType (Not expr) = do
    expType <- deduceType expr
    case expType of
        Bool -> return $ Bool
        _ -> throwError "Incorrect not"
deduceType (EMul e1 _ e2) = do
    ev1 <- deduceType e1
    ev2 <- deduceType e2
    case ev1 == Int && ev2 == Int of
        True -> return $ ev1
        False -> throwError "bad multiply"
deduceType (EAdd exp1 Plus exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    case ev1 == ev2 of
        True -> case ev1 == Str || ev1 == Int of
            True -> return ev1
            False -> throwError "bad types"
        False -> throwError "bad types"

deduceType (EAdd exp1 _ exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    case ev1 == Int && ev2 == Int of
        True -> return $ ev1
        False -> throwError "bad add"

deduceType (ERel exp1 _ exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    case ev1 == Bool && ev2 == Bool of
        True -> return Bool
        False -> throwError "wrong types in bool"

deduceType (EAnd exp1 exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    case ev1 == Bool && ev2 == Bool of
        True -> return $ ev1
        False -> throwError "bad and"

deduceType (EOr exp1 exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    case ev1 == Bool && ev2 == Bool of
        True -> return $ ev1
        False -> throwError "bad or"


runStmt :: Stmt -> Eval ()

