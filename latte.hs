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
preserveState :: Eval a -> Eval a
preserveState p = do
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
    putFreeLocation $ freeLoc + 1
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

runBlock :: Block -> Eval ()
runBlock (Block statements) = do
    freeLoc <- gets freeLocation
    putBlockStart freeLoc
    mapM_ runStmt statements


declare :: Ident -> Type -> Eval ()
declare varName varType = do
    locs <- gets locations
    startLoc <- gets blockStart
    case Map.lookup varName locs of
        Just location -> case location < startLoc of
            True -> allocateVar varName varType
            False -> throwError $ alreadyDeclared varName
        _ -> allocateVar varName varType

declareItem :: Type -> Item -> Eval ()
declareItem expectedType (NoInit varName) = do
    declare varName expectedType

declareItem expectedType (Init varName expr) = do
    expressionType <- deduceType expr
    case expressionType == expectedType of
        True -> declare varName expressionType
        False -> throwError "trying to init with wrong type"

runStmt :: Stmt -> Eval ()
runStmt Empty = return ()
runStmt (BStmt block) = preserveState (runBlock block)
runStmt (Decl varType varInits) = mapM_ (declareItem varType) varInits
runStmt (Ass varName expr) = do
    expressionType <- deduceType expr
    varType <- deduceType (EVar varName)
    case expressionType == varType of
        True -> return ()
        False -> throwError "trying to assign to wrong type"

runStmt (Incr varName) = do
    varType <- deduceType (EVar varName)
    case varType == Int of
        True -> return ()
        False -> throwError "incrementing not an integer"

runStmt (Decr varName) = do
    varType <- deduceType (EVar varName)
    case varType == Int of
        True -> return ()
        False -> throwError "decrementing not an integer"

runStmt (Ret expr) = do
    expressionType <- deduceType expr
    expectedType <- gets expectRetType
    case expressionType == expectedType of
        True -> return ()
        False -> throwError "Inoorrect return type or double return"

runStmt (Cond expr stmt) = do
    expressionType <- deduceType expr
    case expressionType == Bool of
        True -> return ()
        False -> throwError "not a bool in if"
    runStmt stmt

runStmt (CondElse expr stmt1 stmt2) = do
    expressionType <- deduceType expr
    case expressionType == Bool of
        True -> return ()
        False -> throwError "not a bool in if"
    runStmt stmt1
    runStmt stmt2

runStmt (While expr stmt) = do
    expressionType <- deduceType expr
    case expressionType == Bool of
        True -> return ()
        False -> throwError "not a bool in if"
    runStmt stmt

runStmt (SExp expr) = do
    deduceType expr
    return ()

runTopDef :: TopDef -> Eval ()
runTopDef (FnDef retType funName arguments block) = do
    putExpectRetType retType

getArgNames :: [Arg] -> Eval [Ident]
getArgNames arguments = do
    let
        getName (Arg _ argName) = argName
        argNames = map getName arguments in
        case (length (nub argNames)) == (length argNames) of
            True -> return argNames
            False -> throwError "repeating argnames"

getArgTypes :: [Arg] -> Eval [Type]
getArgTypes arguments = do
    let
        getType (Arg argType _) = argType
        argTypes = map getType arguments in
        return argTypes

declareFun :: TopDef -> Eval ()
declareFun (FnDef retType funName arguments _) = do
    argTypes <- getArgTypes arguments
    declare funName (Fun retType argTypes)

defineFun :: TopDef -> Eval ()
defineFun (FnDef retType funName arguments block) = do
    argNames <- getArgNames arguments
    argTypes <- getArgTypes arguments
    --save state here
    mapM_ (uncurry declare) (zip argNames argTypes)
    runBlock block

runProgram :: Program -> Eval ()
runProgram (Program defList) = do
    mapM_ declareFun defList
    mapM_ (preserveState . defineFun) defList


runText :: String -> IO String
runText s = let ts = myLexer s in case pProgram ts of
    Bad s -> do hPutStrLn stderr "\nParse              Failed...\n"
                hPutStrLn stderr s
                exitFailure
    Ok tree -> case runEval (ProgState Map.empty Map.empty 0 0 Void) (runProgram tree) of
        Right ((), writ) -> return $ "All is OK"
        Left errMessage -> do hPutStr stderr errMessage
                              exitFailure


main :: IO ()
main = do
    args <- getArgs
    case args of
        [s] -> do
            code <- readFile s
            runText code
            return ()
        _ -> hPutStrLn stderr  "Only one argument!"

