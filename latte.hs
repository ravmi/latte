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
                 , caughtRetType :: (Type, Bool)
                 }

type Eval ev = StateT ProgState (WriterT String (ExceptT String Identity)) ev
runEval mem ev = runIdentity $ runExceptT (runWriterT (evalStateT ev mem))

putMemory :: Location -> Type -> Eval ()
putMemory key val = do
    ProgState memory locs bs fl ert crt <- get
    put $ ProgState (Map.insert key val memory) locs bs fl ert crt

putLocations :: Ident -> Location -> Eval ()
putLocations key val = do
    ProgState memory locs bs fl ert crt <- get
    put $ ProgState memory (Map.insert key val locs) bs fl ert crt

putBlockStart :: Location -> Eval ()
putBlockStart new = do
    ProgState memory locs bs fl ert crt <- get
    put $ ProgState memory locs new fl ert crt

putFreeLocation :: Location -> Eval ()
putFreeLocation new = do
    ProgState memory locs bs fl ert crt <- get
    put $ ProgState memory locs bs new ert crt

putExpectRetType :: Type -> Eval ()
putExpectRetType new = do
    ProgState memory locs bs fl ert crt <- get
    put $ ProgState memory locs bs fl new crt

putCaughtRetType :: (Type, Bool) -> Eval ()
putCaughtRetType new = do
    ProgState memory locs bs fl ert crt <- get
    put $ ProgState memory locs bs fl ert new

-- this allows to run block without affecting environment outside of the block.
preserveState :: Eval a -> Eval a
preserveState p = do
    s <- get
    x <- p
    put s
    return x

catchRet :: Eval a -> Eval (Type, Bool)
catchRet ev = do
    oldRet <- gets caughtRetType
    ev
    newRet <- gets caughtRetType
    putCaughtRetType oldRet
    return newRet

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
    if argTypes /= expectedArgTypes
        then throwError (badApply funName expectedArgTypes argTypes)
        else return expectedRet

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
    if ev1 /= Int || ev2 /= Int
        then throwError $ badTypesSuggestion "`*`" [Int, Int] [ev1, ev2]
        else return $ ev1

deduceType (EAdd exp1 Plus exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    if ev1 /= ev2 || (ev1 /= Str && ev1 /= Int)
        then throwError $ badTypes "`+`" [ev1, ev2]
        else return ev1

deduceType (EAdd exp1 _ exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    if ev1 /= Int || ev2 /= Int
        then throwError $ badTypes "`+`" [ev1, ev2]
        else return $ ev1

deduceType (ERel exp1 EQU exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    if (ev1 /= ev2) || (ev1 /= Int && ev1 /= Str && ev1 /= Bool)
        then throwError $ badTypes "`==`" [ev1, ev2]
        else return Bool

deduceType (ERel exp1 _ exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    if ev1 /= Int || ev2 /= Int
        then throwError $ badTypesSuggestion "comparison operator" [Int, Int] [ev1, ev2]
        else return Bool

deduceType (EAnd exp1 exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    if ev1 /= Bool || ev2 /= Bool
        then throwError $ badTypesSuggestion "&&" [Bool, Bool] [ev1, ev2]
        else return $ ev1

deduceType (EOr exp1 exp2) = do
    ev1 <- deduceType exp1
    ev2 <- deduceType exp2
    if ev1 /= Bool || ev2 /= Bool
        then throwError $ badTypesSuggestion "||" [Bool, Bool] [ev1, ev2]
        else return $ ev1

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
        Just location -> if location >= startLoc
            then throwError $ alreadyDeclared varName
            else allocateVar varName varType
        _ -> allocateVar varName varType

declareItem :: Type -> Item -> Eval ()
declareItem expectedType (NoInit varName) = do
    declare varName expectedType

declareItem expectedType (Init varName expr) = do
    expressionType <- deduceType expr
    if expressionType /= expectedType
        then throwError $ badTypesSuggestion "assignment" expectedType expressionType
        else declare varName expressionType

runStmt :: Stmt -> Eval ()
runStmt Empty = return ()

runStmt (BStmt block) = do
    ret <- preserveState $ catchRet (runBlock block)
    putCaughtRetType ret

runStmt (Decl varType varInits) = mapM_ (declareItem varType) varInits
runStmt (Ass varName expr) = do
    expressionType <- deduceType expr
    varType <- deduceType (EVar varName)
    if expressionType /= varType
        then throwError $ badTypesSuggestion "assignment" varType expressionType
        else return ()

runStmt (Incr varName) = do
    varType <- deduceType (EVar varName)
    if varType /= Int
        then throwError $ badTypesSuggestion "++" Int varType
        else return ()

runStmt (Decr varName) = do
    varType <- deduceType (EVar varName)
    if varType /= Int
        then throwError $ badTypesSuggestion "--" Int varType
        else return ()

runStmt (Ret expr) = do
    expressionType <- deduceType expr
    expectedType <- gets expectRetType
    if expressionType /= expectedType
        then throwError $ badTypesSuggestion "return statement" expectedType expressionType
        else putCaughtRetType (expressionType, True)

runStmt (VRet) = do
    expectedType <- gets expectRetType
    if expectedType /= Void
        then throwError $ badTypesSuggestion "return statement" expectedType Void
        else putCaughtRetType (Void, True)

runStmt (Cond ELitTrue stmt) = runStmt stmt

runStmt (Cond ELitFalse stmt) = catchRet (runStmt stmt) >> return ()

runStmt (Cond expr stmt) = do
    expressionType <- deduceType expr
    if expressionType /= Bool
        then throwError $ badTypesSuggestion "condition statement" Bool expressionType
        else catchRet $ runStmt stmt
    return ()

runStmt (CondElse ELitTrue stmt1 stmt2) = do
    retOld <- catchRet $ runStmt stmt1
    runStmt stmt2
    putCaughtRetType retOld

runStmt (CondElse ELitFalse stmt1 stmt2) = do
    catchRet $ runStmt stmt1
    runStmt stmt2

runStmt (CondElse expr stmt1 stmt2) = do
    expressionType <- deduceType expr
    when (expressionType /= Bool) (throwError $ badTypesSuggestion "condition statement" Bool expressionType)
    (_, oldWasCaught) <- gets caughtRetType
    (ret1, wasCaught1) <- catchRet $ runStmt stmt1
    (ret2, wasCaught2) <- catchRet $ runStmt stmt2
    when (not oldWasCaught && wasCaught1 && wasCaught2 && (ret1 == ret2)) (putCaughtRetType (ret1, True))

runStmt (While expr stmt) = do
    expressionType <- deduceType expr
    if expressionType /= Bool
        then throwError $ badTypesSuggestion "while statement" Bool expressionType
        else return ()
    (retOld, oldWasCaught) <- gets caughtRetType
    (newRet, newWasCaught) <- catchRet $ runStmt stmt
    if (not oldWasCaught) && newWasCaught
        then putCaughtRetType (newRet, True)
        else return ()

runStmt (SExp expr) = do
    deduceType expr
    return ()

getArgNames :: [Arg] -> Eval [Ident]
getArgNames arguments = do
    let
        getName (Arg _ argName) = argName
        argNames = map getName arguments in
        if (length (nub argNames)) /= (length argNames)
            then throwError $ repeatingArgs arguments
            else return argNames

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
    (caughtRet, _) <- catchRet $ runBlock block
    if caughtRet /= retType
        then throwError $ badReturn funName retType
        else return ()

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
    Ok tree -> case runEval (ProgState Map.empty Map.empty 0 0 Void (Void, False)) (runProgram tree) of
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
