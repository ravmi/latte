
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Main where

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



type Location = Integer
-- describes where are the variables located
type Locations = Map.Map Ident Location
-- describes what memory looks like (only types for now)
type Memory = Map.Map Location (Register, Type)

-- in current state we want to keep locations of variables, state of the memory,
-- current free avaliable memory location, location of beginning of the block
-- and expected return type
data ProgState = ProgState { memory :: Memory
                 , locations :: Locations
                 , blockStart :: Location
                 , freeLocation :: Location
                 , expectRetType :: Type
                 , caughtRetType :: (Type, Bool)
                 , freeRegister :: Location
                 , code :: QCode
                 , freeLabel :: Integer
                 }
mkLabels [''ProgState]

type Eval ev = StateT ProgState (WriterT String (ExceptT String Identity)) ev
runEval mem ev = runIdentity $ runExceptT (runWriterT (evalStateT ev mem))

emit :: Quadruple -> Eval ()
emit q = do
    ProgState memory locs bs fl ert crt fr cd flab <- get
    put $ ProgState memory locs bs fl ert crt fr (cd ++ [q]) flab

newReg :: Eval (Register)
newReg = do
    ProgState memory locs bs fl ert crt fr cd flab <- get
    put $ ProgState memory locs bs fl ert crt (fr + 1) cd flab
    return $ Reg fr

newLabel :: Eval (Quadruple, Integer)
newLabel = do
    ProgState memory locs bs fl ert crt fr cd flab <- get
    put $ ProgState memory locs bs fl ert crt fr cd (flab + 1)
    return $ (QLab (Ident ("L" ++ (show flab))), flab)

makeLabelN :: Integer -> Eval Ident
makeLabelN i = do
    return $ Ident ("L" ++ (show i))

makeLabelQ :: Integer -> Eval Quadruple
makeLabelQ i = do
    return $ QLab (Ident ("L" ++ (show i)))

putMemory :: Location -> (Register, Type) -> Eval ()
putMemory key val = do
    ProgState memory locs bs fl ert crt fr cd flab <- get
    put $ ProgState (Map.insert key val memory) locs bs fl ert crt fr cd flab

putLocations :: Ident -> Location -> Eval ()
putLocations key val = do
    ProgState memory locs bs fl ert crt fr cd flab <- get
    put $ ProgState memory (Map.insert key val locs) bs fl ert crt fr cd flab

putBlockStart :: Location -> Eval ()
putBlockStart new = do
    ProgState memory locs bs fl ert crt fr cd flab <- get
    put $ ProgState memory locs new fl ert crt fr cd flab

putFreeLocation :: Location -> Eval ()
putFreeLocation new = do
    ProgState memory locs bs fl ert crt fr cd flab <- get
    put $ ProgState memory locs bs new ert crt fr cd flab

putExpectRetType :: Type -> Eval ()
putExpectRetType new = do
    ProgState memory locs bs fl ert crt fr cd flab <- get
    put $ ProgState memory locs bs fl new crt fr cd flab

putCaughtRetType :: (Type, Bool) -> Eval ()
putCaughtRetType new = do
    ProgState memory locs bs fl ert crt fr cd flab <- get
    put $ ProgState memory locs bs fl ert new fr cd flab

clearMemory :: Eval ()
clearMemory = do
    ProgState memory locs bs fl ert crt fr cd flab <- get
    put $ ProgState Map.empty Map.empty 0 0 ert crt fr cd flab


-- this allows to run block without affecting environment outside of the block.
preserveState :: Eval a -> Eval a
preserveState p = do
    ProgState memory1 locs1 bs1 fl1 ert1 crt1 fr1 cd1 flab1 <- get
    x <- p
    ProgState memory2 locs2 bs2 fl2 ert2 crt2 fr2 cd2 flab2 <- get
    put $ ProgState memory1 locs1 bs1 fl1 ert1 crt1 fr1 cd2 flab2
    return x

--preserve :: Eval a -> (ProgState -> b) -> Eval a
--preserve eval getter = do
--    state <- get
--    oldVal <- gets getter
--    x <- eval
--    put $ state { getter = oldVal}
--    return $ x


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

allocateVar :: Ident -> Type -> Eval Integer
allocateVar varName varType = do
    freeLoc <- gets freeLocation
    locs <- gets locations
    putMemory freeLoc (Mem freeLoc, varType)
    putLocations varName freeLoc
    putFreeLocation $ freeLoc + 1
    return freeLoc

lookupName :: Ident -> Eval (Register, Type)
lookupName name = do
    locs <- gets locations
    mem <- gets memory
    case Map.lookup name locs of
        Just location -> case Map.lookup location mem of
            Just varInfo -> return varInfo
            _ -> throwError unexpected
        _ -> throwError $ varUninitialized name

getLoc :: Ident -> Eval (Register)
getLoc name = do
    locs <- gets locations
    mem <- gets memory
    case Map.lookup name locs of
        Just location -> return $ Mem location
        _ -> throwError $ varUninitialized name

dedType :: Expr -> Eval Type
dedType expr = do
    (r, t) <- deduceType expr
    return t
-- @TODO odzielna mapa na funkcja
deduceType :: Expr -> Eval (Register, Type)
deduceType (EVar name) = lookupName name
deduceType (ELitInt i) = return (RegInt i, Int)
deduceType (ELitTrue) = return (RegBool True, Bool)
deduceType (ELitFalse) = return (RegBool False, Bool)
deduceType (EApp funName funArgs) = do
    (hehe, Fun expectedRet expectedArgTypes)  <- lookupName funName
    argTypes <- mapM dedType funArgs
    when (argTypes /= expectedArgTypes)
       (throwError $ badApply funName expectedArgTypes argTypes)
    emit $ QFunc funName
    return (Mem 0, expectedRet)

deduceType (EString str) = do
    nReg <- newReg
    emit $ QString nReg str
    return $ (nReg, Str)

deduceType (Neg expr) = do
    (r, t) <- deduceType expr
    when (t /= Bool)
        (throwError $ badTypesSuggestion "`-`" Int t)
    r2 <- newReg
    emit $ QNeg r r2
    return (r2, Int)

deduceType (Not expr) = do
    (r, t) <- deduceType expr
    when (t /= Bool)
        (throwError $ badTypesSuggestion "`!`" Bool t)
    r2 <- newReg
    emit $ QNot r r2
    return (r2, Bool)

deduceType (EMul e1 op e2) = do
    (r1, t1) <- deduceType e1
    (r2, t2) <- deduceType e2
    when (t1 /= Int || t2 /= Int)
        (throwError $ badTypesSuggestion "`*`" [Int, Int] [t1, t2])
    r3 <- newReg
    case op of
        Times -> emit $ QMul r1 r2 r3
        Mod -> emit $ QMod r1 r2 r3
        Div -> emit $ QDiv r1 r2 r3
    return (r3, t1)

deduceType (EAdd exp1 Plus exp2) = do
    (r1, t1) <- deduceType exp1
    (r2, t2) <- deduceType exp2
    when(t1 /= t2 || (t1 /= Str && t1 /= Int))
        (throwError $ badTypes "`+`" [t1, t2])
    r3 <- newReg
    emit $ QAdd r1 r2 r3
    return (r3, t1)

deduceType (EAdd exp1 Minus exp2) = do
    (r1, t1) <- deduceType exp1
    (r2, t2) <- deduceType exp2
    when (t1 /= Int || t2 /= Int)
        (throwError $ badTypes "`+`" [t1, t2])
    r3 <- newReg
    emit $ QAdd r1 r2 r3
    return (r3, t1)

deduceType (ERel exp1 EQU exp2) = do
    (r1, t1) <- deduceType exp1
    (r2, t2) <- deduceType exp2
    when ((t1 /= t2) || (t1 /= Int && t1 /= Str && t1 /= Bool))
        (throwError $ badTypes "`==`" [t1, t2])
    r3 <- newReg
    emit $ QCmpEq r1 r2
    return (r3, Bool)

deduceType (ERel exp1 op exp2) = do
    (r1, t1) <- deduceType exp1
    (r2, t2) <- deduceType exp2
    when (t1 /= Int || t2 /= Int)
        (throwError $ badTypesSuggestion "comparison operator" [Int, Int] [t1, t2])
    r3 <- newReg
    case op of
        LTH -> emit $ QCmpLt r1 r2
        LE -> emit $ QCmpLe r1 r2
        GTH -> emit $ QCmpGt r1 r2
        GE -> emit $ QCmpGe r1 r2
        EQU -> emit $ QCmpEq r1 r2
        NE -> emit $ QCmpNe r1 r2
    return (r3, Bool)

deduceType (EAnd exp1 exp2) = do
    (r1, t1) <- deduceType exp1
    (r2, t2) <- deduceType exp2
    when (t1 /= Bool || t2 /= Bool)
        (throwError $ badTypesSuggestion "&&" [Bool, Bool] [t1, t2])
    r3 <- newReg
    emit $ QAnd r1 r2 r3
    return (r3, Bool)

deduceType (EOr exp1 exp2) = do
    (r1, t1) <- deduceType exp1
    (r2, t2) <- deduceType exp2
    when (t1 /= Bool || t2 /= Bool)
        (throwError $ badTypesSuggestion "||" [Bool, Bool] [t1, t2])
    r3 <- newReg
    emit $ QOr r1 r2 r3
    return (r3, Bool)

runBlock :: Block -> Eval ()
runBlock (Block statements) = do
    freeLoc <- gets freeLocation
    putBlockStart freeLoc
    mapM_ runStmt statements

declare :: Ident -> Type -> Eval (Integer)
declare varName varType = do
    locs <- gets locations
    startLoc <- gets blockStart
    case Map.lookup varName locs of
        Just location -> if (location >= startLoc)
            then throwError $ alreadyDeclared varName
            else allocateVar varName varType
        _ -> allocateVar varName varType

declareItem :: Type -> Item -> Eval ()
declareItem expectedType (NoInit varName) = do
    declare varName expectedType
    return ()

declareItem expectedType (Init varName expr) = do
    (r1, t1) <- deduceType expr
    when (t1 /= expectedType)
        (throwError $ badTypesSuggestion "assignment" expectedType t1)
    loc <- declare varName t1
    emit $ QAss r1 (Mem loc)

runStmt :: Stmt -> Eval ()
runStmt Empty = return ()

runStmt (BStmt block) = do
    ret <- preserveState $ catchRet (runBlock block)
    putCaughtRetType ret

runStmt (Decl varType varInits) = mapM_ (declareItem varType) varInits
runStmt (Ass varName expr) = do
    (r1, t1) <- deduceType expr
    (r2, t2) <- deduceType (EVar varName)
    when (t1 /= t2)
        (throwError $ badTypesSuggestion "assignment" t2 t1)
    l1 <- getLoc varName
    emit $ QAss r1 l1


runStmt (Incr varName) = do
    (r1, t1) <- deduceType (EVar varName)
    when (t1 /= Int)
        (throwError $ badTypesSuggestion "++" Int t1)
    l1 <- getLoc varName
    emit $ QInc l1

runStmt (Decr varName) = do
    (r1, t1) <- deduceType (EVar varName)
    when (t1 /= Int)
        (throwError $ badTypesSuggestion "--" Int t1)
    l1 <- getLoc varName
    emit $ QDec l1

runStmt (Ret expr) = do
    (r1, t1) <- deduceType expr
    expectedType <- gets expectRetType
    when (t1 /= expectedType)
        (throwError $ badTypesSuggestion "return statement" expectedType t1)
    putCaughtRetType (t1, True)
    emit $ QRet r1

runStmt (VRet) = do
    expectedType <- gets expectRetType
    when (expectedType /= Void)
        (throwError $ badTypesSuggestion "return statement" expectedType Void)
    putCaughtRetType (Void, True)
    emit $ QRetV

runStmt (Cond ELitTrue stmt) = runStmt stmt

runStmt (Cond ELitFalse stmt) = catchRet (runStmt stmt) >> return ()

runStmt (Cond expr stmt) = do
    (r1, t1) <- deduceType expr
    (lab1, lname1) <- newLabel
    emit $ QCmpNe r1 (RegInt 0)
    lName <- makeLabelN lname1
    lQuad <- makeLabelQ lname1
    emit $ QJne lName
    when (t1 /= Bool)
        (throwError $ badTypesSuggestion "condition statement" Bool t1)
    catchRet $ runStmt stmt
    emit lQuad
    return ()

runStmt (CondElse ELitTrue stmt1 stmt2) = do
    retOld <- catchRet $ runStmt stmt1
    runStmt stmt2
    putCaughtRetType retOld

runStmt (CondElse ELitFalse stmt1 stmt2) = do
    catchRet $ runStmt stmt1
    runStmt stmt2

runStmt (CondElse expr stmt1 stmt2) = do
    (lab1, lname1) <- newLabel
    (lab2, lname2) <- newLabel
    (r1, t1) <- deduceType expr
    lName1 <- makeLabelN lname1
    lQuad1 <- makeLabelQ lname1
    lName2 <- makeLabelN lname2
    lQuad2 <- makeLabelQ lname2
    emit $ QCmpNe r1 (RegInt 0)
    emit $ QJne lName1
    when (t1 /= Bool)
        (throwError $ badTypesSuggestion "condition statement" Bool t1)
    (_, oldWasCaught) <- gets caughtRetType
    (ret1, wasCaught1) <- catchRet $ runStmt stmt1
    emit $ QJmp lName2
    emit $ lab1
    (ret2, wasCaught2) <- catchRet $ runStmt stmt2
    emit $ lab2
    when (not oldWasCaught && wasCaught1 && wasCaught2 && (ret1 == ret2))
        (putCaughtRetType (ret1, True))

runStmt (While expr stmt) = do
    (lab1, lname1) <- newLabel
    (lab2, lname2) <- newLabel
    lName1 <- makeLabelN lname1
    lQuad1 <- makeLabelQ lname1
    lName2 <- makeLabelN lname2
    lQuad2 <- makeLabelQ lname2
    emit lab1
    (r1, t1) <- deduceType expr
    emit $ QCmpNe r1 (RegInt 0)
    emit $ QJne lName2
    when (t1 /= Bool)
        (throwError $ badTypesSuggestion "while statement" Bool t1)
    (retOld, oldWasCaught) <- gets caughtRetType
    (newRet, newWasCaught) <- catchRet $ runStmt stmt
    when ((not oldWasCaught) && newWasCaught)
        (putCaughtRetType (newRet, True))
    emit $ QJmp lName1
    emit lab2

runStmt (SExp expr) = do
    deduceType expr
    return ()

countLocals2 :: [Stmt] -> Int
countLocals2 [] = 0
countLocals2 (Decl _ items : rest) = (length items) + (countLocals2 rest)
countLocals2 ((BStmt (Block b)) : rest) = (countLocals2 b) + (countLocals2 rest)
countLocals2 (_:rest) = countLocals2 rest

countLocals :: Block -> Int
countLocals (Block stmts) = countLocals2 stmts

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
    return ()

defineFun :: TopDef -> Eval (QBlock)
defineFun (FnDef retType funName arguments block) = do
    argNames <- getArgNames arguments
    argTypes <- getArgTypes arguments
    putExpectRetType retType
    mapM_ (uncurry declare) (zip argNames argTypes)
    (caughtRet, _) <- catchRet $ runBlock block
    when (caughtRet /= retType)
        (throwError $ badReturn funName retType)
    cod <- gets code
    ProgState memory locs bs fl ert crt fr cd flab <- get
    put $ ProgState memory locs bs fl ert crt fr [] flab

    return $ QBlock funName cod (countLocals block)

declareNativeFunctions :: Eval ()
declareNativeFunctions = do
    declare (Ident "printInt") (Fun Void [Int])
    declare (Ident "printString") (Fun Void [Str])
    declare (Ident "readInt") (Fun Int [])
    declare (Ident "readString") (Fun Str [])
    return ()

runProgram :: Program -> Eval [QBlock]
runProgram (Program defList) = do
    declareNativeFunctions
    mapM_ declareFun defList
    freeLoc <- gets freeLocation
    putBlockStart freeLoc
    clearMemory
    mapM (preserveState . defineFun) defList

runText :: String -> IO [QBlock]
runText s = let ts = myLexer s in case pProgram ts of
    Bad s -> do hPutStrLn stderr "\nParse              Failed...\n"
                hPutStrLn stderr s
                exitFailure
    Ok tree -> case runEval (ProgState Map.empty Map.empty 0 0 Void (Void, False) 0 [] 0) (runProgram tree) of
        Right (code, w) -> return $ code
        Left errMessage -> do hPutStrLn stderr errMessage
                              exitFailure

---        Right ((), (a, b)) -> return $ "All is OK"
main :: IO ()
main = do
    args <- getArgs
    case args of
        [s] -> do
            code <- readFile s
            ps <- runText code
            print $ ps
            return ()
        _ -> hPutStrLn stderr  "Only one argument!"
