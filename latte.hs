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

-- this allows to run block without affecting environment outside of the block.
preserveState :: Eval a -> Eval a
preserveState p = do
    s <- get
    x <- p
    put s
    return x

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
    used = "used: " ++ (show badTypes)
    errLog = why ++ "; " ++ expected ++ "; " ++ used

badTypesSuggestion :: (Show a, Show b) => String -> a -> b -> String
badTypesSuggestion opName okTypes badTypes = errLog where
    why = "Wrong usage of " ++ opName
    expected = "expected type: " ++ (show okTypes)
    used = "used: " ++ (show badTypes)
    errLog = why ++ "; " ++ expected ++ "; " ++ used

badTypes :: (Show a) => String -> a -> String
badTypes opName badTypes = errLog where
    why = "Wrong usage of " ++ opName
    used = "used: " ++ (show badTypes)
    errLog = why ++ "; " ++ "; " ++ used

repeatingArgs :: [Arg] -> String
repeatingArgs arguments = errLog where
    why = "Some of the argument names repeat"
    showArg (Arg varType (Ident varName)) = show varType ++ " " ++ varName
    argList = map showArg arguments
    errLog = why ++ ": " ++ "[" ++ (intercalate ", " argList) ++ "]"

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
            _ -> throwError unexpected
        _ -> throwError $ varUninitialized name

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
        False -> throwError (badApply funName expectedArgTypes argTypes)

deduceType (EString _) = return Str
deduceType (Neg expr) = do
    expType <- deduceType expr
    case expType of
        Int -> return $ Int
        _ -> throwError $ badTypesSuggestion "`-`" Int expType

deduceType (Not expr) = do
    expType <- deduceType expr
    case expType of
        Bool -> return $ Bool
        _ -> throwError $ badTypesSuggestion "`!`" Bool expType

deduceType (EMul e1 _ e2) = do
    ev1 <- deduceType e1
    ev2 <- deduceType e2
    case ev1 == Int && ev2 == Int of
        True -> return $ ev1
        False -> throwError $ badTypesSuggestion "`*`" [Int, Int] [ev1, ev2]

deduceType (EAdd exp1 Plus exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    case ev1 == ev2 && (ev1 == Str || ev1 == Int) of
        True -> return ev1
        False -> throwError $ badTypes "`+`" [ev1, ev2]

deduceType (EAdd exp1 _ exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    case ev1 == Int && ev2 == Int of
        True -> return $ ev1
        False -> throwError $ badTypes "`+`" [ev1, ev2]

deduceType (ERel exp1 EQU exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    case (ev1 == ev2) && (ev1 == Int || ev1 == Str || ev1 == Bool) of
        True -> return Bool
        False -> throwError $ badTypes "`==`" [ev1, ev2]

deduceType (ERel exp1 _ exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    case ev1 == Int && ev2 == Int of
        True -> return Bool
        False -> throwError $ badTypesSuggestion "comparison operator" [Int, Int] [ev1, ev2]

deduceType (EAnd exp1 exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    case ev1 == Bool && ev2 == Bool of
        True -> return $ ev1
        False -> throwError $ badTypesSuggestion "&&" [Bool, Bool] [ev1, ev2]

deduceType (EOr exp1 exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    case ev1 == Bool && ev2 == Bool of
        True -> return $ ev1
        False -> throwError $ badTypesSuggestion "||" [Bool, Bool] [ev1, ev2]

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
        False -> throwError $ badTypesSuggestion "assignment" expectedType expressionType

runStmt :: Stmt -> Eval ()
runStmt Empty = return ()
runStmt (BStmt block) = preserveState (runBlock block)
runStmt (Decl varType varInits) = mapM_ (declareItem varType) varInits
runStmt (Ass varName expr) = do
    expressionType <- deduceType expr
    varType <- deduceType (EVar varName)
    case expressionType == varType of
        True -> return ()
        False -> throwError $ badTypesSuggestion "assignment" varType expressionType

runStmt (Incr varName) = do
    varType <- deduceType (EVar varName)
    case varType == Int of
        True -> return ()
        False -> throwError $ badTypesSuggestion "++" Int varType

runStmt (Decr varName) = do
    varType <- deduceType (EVar varName)
    case varType == Int of
        True -> return ()
        False -> throwError $ badTypesSuggestion "--" Int varType

runStmt (Ret expr) = do
    expressionType <- deduceType expr
    expectedType <- gets expectRetType
    case expressionType == expectedType of
        True -> return ()
        False -> throwError $ badTypesSuggestion "return statement" expectedType expressionType

runStmt (VRet) = do
    expectedType <- gets expectRetType
    case expectedType == Void of
        True -> return ()
        False -> throwError $ badTypesSuggestion "return statement" expectedType Void

runStmt (Cond expr stmt) = do
    expressionType <- deduceType expr
    case expressionType == Bool of
        True -> return ()
        False -> throwError $ badTypesSuggestion "condition statement" Bool expressionType
    runStmt stmt

runStmt (CondElse expr stmt1 stmt2) = do
    expressionType <- deduceType expr
    case expressionType == Bool of
        True -> return ()
        False -> throwError $ badTypesSuggestion "condition statement" Bool expressionType
    runStmt stmt1
    runStmt stmt2

runStmt (While expr stmt) = do
    expressionType <- deduceType expr
    case expressionType == Bool of
        True -> return ()
        False -> throwError $ badTypesSuggestion "while statement" Bool expressionType
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
            False -> throwError $ repeatingArgs arguments

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
    putExpectRetType retType
    mapM_ (uncurry declare) (zip argNames argTypes)
    runBlock block

declareNativeFunctions :: Eval ()
declareNativeFunctions = do
    declare (Ident "printInt") (Fun Void [Int])
    declare (Ident "printString") (Fun Void [Str])
    declare (Ident "readInt") (Fun Int [])
    declare (Ident "readString") (Fun Str [])

runProgram :: Program -> Eval ()
runProgram (Program defList) = do
    declareNativeFunctions
    mapM_ declareFun defList
    freeLoc <- gets freeLocation
    putBlockStart freeLoc
    mapM_ (preserveState . defineFun) defList

runText :: String -> IO String
runText s = let ts = myLexer s in case pProgram ts of
    Bad s -> do hPutStrLn stderr "\nParse              Failed...\n"
                hPutStrLn stderr s
                exitFailure
    Ok tree -> case runEval (ProgState Map.empty Map.empty 0 0 Void) (runProgram tree) of
        Right ((), writ) -> return $ "All is OK"
        Left errMessage -> do hPutStrLn stderr errMessage
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
