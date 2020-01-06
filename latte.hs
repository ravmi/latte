{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Main where

import Control.Category
import Data.Label hiding (get)
import Prelude hiding ((.), id)

import LatteErrors

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
import qualified Data.Label as L
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
data ProgState = ProgState { _memory :: Memory
                 , _locations :: Locations
                 , _blockStart :: Location
                 , _freeLocation :: Location
                 , _expectRetType :: Type
                 , _caughtRetType :: (Type, Bool)
                 , _freeRegisters :: [Register]
                 , _code :: QCode
                 , _freeLabel :: Integer
                 , _functions :: Map.Map Ident Type
                 }
                 deriving (Eq, Ord, Show, Read)
mkLabels [''ProgState]

type Eval ev = StateT ProgState (WriterT String (ExceptT String Identity)) ev
runEval mem ev = runIdentity $ runExceptT (runWriterT (evalStateT ev mem))

emit :: Quadruple -> Eval ()
emit q = do
    ProgState memory locs bs fl ert crt fr cd flab funs <- get
    put $ ProgState memory locs bs fl ert crt fr (cd ++ [q]) flab funs

--example = ProgState Map.empty Map.empty 0 0 Void (Void, False) 0 [] 0

--newReg :: Eval (Register)
--newReg = do
--    ProgState memory locs bs fl ert crt fr cd flab <- get
--    put $ ProgState memory locs bs fl ert crt (fr + 1) cd flab
--    return $ Reg fr

getRegister :: Eval (Register)
getRegister = do
    ProgState memory locs bs fl ert crt freeRegisters cd flab funs <- get
    freeReg <- return $ head freeRegisters
    put $ ProgState memory locs bs fl ert crt (tail freeRegisters) cd flab funs
    return $ freeReg


putRegister :: Register -> Eval ()
putRegister reg = do
    freeRegs <- lgets freeRegisters
    case reg of
        Reg1 -> update freeRegisters (Reg1:freeRegs)
        Reg2 -> update freeRegisters (Reg2:freeRegs)
        Reg3 -> update freeRegisters (Reg3:freeRegs)
        _ -> return ()

update :: ProgState :-> a -> a -> Eval ()
update fun val = do
    state <- get
    put $ set fun val state

lgets :: MonadState f m => f :-> a -> m a
lgets f = gets (L.get f)

newLabel :: Eval (Quadruple, Integer)
newLabel = do
    flab <- lgets freeLabel
    return $ (QLab (Ident ("L" ++ (show flab))), flab)

makeLabelN :: Integer -> Eval Ident
makeLabelN i = do
    return $ Ident ("L" ++ (show i))

makeLabelQ :: Integer -> Eval Quadruple
makeLabelQ i = do
    return $ QLab (Ident ("L" ++ (show i)))

insertMemory :: Location -> (Register, Type) -> Eval ()
insertMemory key val = do
    mem <- lgets memory
    update memory (Map.insert key val mem)

insertLocations :: Ident -> Location -> Eval ()
insertLocations key val = do
    ProgState memory locs bs fl ert crt fr cd flab funs <- get
    locs <- lgets locations
    update locations (Map.insert key val locs)

updateBlockStart :: Location -> Eval ()
updateBlockStart new = do
    ProgState memory locs bs fl ert crt fr cd flab funs <- get
    put $ ProgState memory locs new fl ert crt fr cd flab funs

updateFreeLocation :: Location -> Eval ()
updateFreeLocation new = do
    ProgState memory locs bs fl ert crt fr cd flab funs <- get
    put $ ProgState memory locs bs new ert crt fr cd flab funs

updateExpectRetType :: Type -> Eval ()
updateExpectRetType new = do
    ProgState memory locs bs fl ert crt fr cd flab funs <- get
    put $ ProgState memory locs bs fl new crt fr cd flab funs

updateCaughtRetType :: (Type, Bool) -> Eval ()
updateCaughtRetType new = do
    ProgState memory locs bs fl ert crt fr cd flab funs <- get
    put $ ProgState memory locs bs fl ert new fr cd flab funs

clearMemory :: Eval ()
clearMemory = do
    ProgState memory locs bs fl ert crt fr cd flab funs <- get
    put $ ProgState Map.empty Map.empty 0 0 ert crt fr cd flab funs


-- this allows to run block without affecting environment outside of the block.
preserveState :: Eval a -> Eval a
preserveState p = do
    ProgState memory1 locs1 bs1 fl1 ert1 crt1 fr1 cd1 flab1 funs1 <- get
    x <- p
    ProgState memory2 locs2 bs2 fl2 ert2 crt2 fr2 cd2 flab2 funs2 <- get
    put $ ProgState memory1 locs1 bs1 fl1 ert1 crt1 fr1 cd2 flab2 funs2
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
    oldRet <- lgets caughtRetType
    ev
    newRet <- lgets caughtRetType
    updateCaughtRetType oldRet
    return newRet

allocateVar :: Ident -> Type -> Eval Integer
allocateVar varName varType = do
    freeLoc <- lgets freeLocation
    locs <- lgets locations
    insertMemory freeLoc (Mem freeLoc, varType)
    insertLocations varName freeLoc
    updateFreeLocation $ freeLoc + 1
    return freeLoc

lookupName :: Ident -> Eval (Register, Type)
lookupName name = do
    locs <- lgets locations
    mem <- lgets memory
    case Map.lookup name locs of
        Just location -> case Map.lookup location mem of
            Just varInfo -> return varInfo
            _ -> throwError unexpected
        _ -> throwError $ varUninitialized name

lookupFunction :: Ident -> Eval (Type)
lookupFunction name = do
    funs <- lgets functions
    case Map.lookup name funs of
        Just fun -> return fun
        _ -> throwError $ varUninitialized name

getLoc :: Ident -> Eval (Register)
getLoc name = do
    locs <- lgets locations
    mem <- lgets memory
    case Map.lookup name locs of
        Just location -> return $ Mem location
        _ -> throwError $ varUninitialized name

copyToRegister :: Register -> Eval Register
copyToRegister pos = do
    freeReg <- getRegister
    case pos of
        Mem i -> do emit $ QMov pos freeReg
                    return freeReg
        RegInt i ->  do emit $ QMov pos freeReg
                        return freeReg
        RegBool b -> do emit $ QMov pos freeReg
                        return freeReg
        _ -> do putRegister freeReg
                return pos

dedType :: Expr -> Eval Type
dedType expr = do
    (r, t) <- translateExpression expr
    return t
-- @TODO odzielna mapa na funkcja









translateExpression :: Expr -> Eval (Register, Type)
translateExpression (EVar name) = lookupName name
translateExpression (ELitInt i) = return (RegInt i, Int)
translateExpression (ELitTrue) = return (RegBool True, Bool)
translateExpression (ELitFalse) = return (RegBool False, Bool)
translateExpression (EApp funName funArgs) = do
    (hehe, Fun expectedRet expectedArgTypes)  <- lookupName funName
    argTypes <- mapM dedType funArgs
    when (argTypes /= expectedArgTypes)
       (throwError $ badApply funName expectedArgTypes argTypes)
    emit $ QFunc funName
    return (Mem 0, expectedRet)

translateExpression (EString str) = do
    nReg <- getRegister
    emit $ QAlloc nReg (length str)
    return $ (nReg, Str)

translateExpression (Neg expr) = do
    (r, t) <- translateExpression expr
    when (t /= Bool)
        (throwError $ badTypesSuggestion "`-`" Int t)
    emit $ QNeg r
    return (r, Int)

translateExpression (Not expr) = do
    (r, t) <- translateExpression expr
    when (t /= Bool)
        (throwError $ badTypesSuggestion "`!`" Bool t)
    emit $ QNot r
    return (r, Bool)

translateExpression (EMul e1 op e2) = do
    (r1, t1) <- translateExpression e1
    (r2, t2) <- translateExpression e2
    when (t1 /= Int || t2 /= Int)
        (throwError $ badTypesSuggestion "`*`" [Int, Int] [t1, t2])
    r2 <- copyToRegister r2
    case op of
        Times -> emit $ QMul r1 r2
        Mod -> emit $ QMod r1 r2
        Div -> emit $ QDiv r1 r2
    putRegister r1
    return (r2, t1)

translateExpression (EAdd exp1 Plus exp2) = do
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
--    r2 <- copyToRegister r2
    when(t1 /= t2 || (t1 /= Str && t1 /= Int))
        (throwError $ badTypes "`+`" [t1, t2])
    r2 <- copyToRegister r2
    case t1 of
        Int -> do emit $ QAdd r1 r2
                  putRegister r1
        Str -> do r3 <- getRegister
                  emit $ QConcat r1 r2 r3
                  putRegister r1
                  putRegister r2
    return (r2, t1)

translateExpression (EAdd exp1 Minus exp2) = do
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when (t1 /= Int || t2 /= Int)
        (throwError $ badTypes "`+`" [t1, t2])
    r2 <- copyToRegister r2
    emit $ QAdd r1 r2
    putRegister r1
    return (r2, t1)

translateExpression (ERel exp1 EQU exp2) = do
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when ((t1 /= t2) || (t1 /= Int && t1 /= Str && t1 /= Bool))
        (throwError $ badTypes "`==`" [t1, t2])
    r2 <- copyToRegister r2
    emit $ QCmpEq r1 r2
    putRegister r1
    return (r2, Bool)

translateExpression (ERel exp1 op exp2) = do
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when (t1 /= Int || t2 /= Int)
        (throwError $ badTypesSuggestion "comparison operator" [Int, Int] [t1, t2])
    r2 <- copyToRegister r2
    case op of
        LTH -> emit $ QCmpLt r1 r2
        LE -> emit $ QCmpLe r1 r2
        GTH -> emit $ QCmpGt r1 r2
        GE -> emit $ QCmpGe r1 r2
        EQU -> emit $ QCmpEq r1 r2
        NE -> emit $ QCmpNe r1 r2
    putRegister r1
    return (r2, Bool)

translateExpression (EAnd exp1 exp2) = do
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when (t1 /= Bool || t2 /= Bool)
        (throwError $ badTypesSuggestion "&&" [Bool, Bool] [t1, t2])
    r2 <- copyToRegister r2
    emit $ QAnd r1 r2
    putRegister r1
    return (r2, Bool)

translateExpression (EOr exp1 exp2) = do
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when (t1 /= Bool || t2 /= Bool)
        (throwError $ badTypesSuggestion "||" [Bool, Bool] [t1, t2])
    r2 <- copyToRegister r2
    emit $ QOr r1 r2
    putRegister r1
    return (r2, Bool)





runBlock :: Block -> Eval ()
runBlock (Block statements) = do
    freeLoc <- lgets freeLocation
    updateBlockStart freeLoc
    mapM_ runStmt statements

declare :: Ident -> Type -> Eval (Integer)
declare varName varType = do
    locs <- lgets locations
    startLoc <- lgets blockStart
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
    (r1, t1) <- translateExpression expr
    when (t1 /= expectedType)
        (throwError $ badTypesSuggestion "assignment" expectedType t1)
    r1 <- copyToRegister r1
    loc <- declare varName t1
    emit $ QAss r1 (Mem loc)

runStmt :: Stmt -> Eval ()
runStmt Empty = return ()

runStmt (BStmt block) = do
    ret <- preserveState $ catchRet (runBlock block)
    updateCaughtRetType ret

runStmt (Decl varType varInits) = mapM_ (declareItem varType) varInits
runStmt (Ass varName expr) = do
    (r1, t1) <- translateExpression expr
    (r2, t2) <- translateExpression (EVar varName)
    when (t1 /= t2)
        (throwError $ badTypesSuggestion "assignment" t2 t1)
    r1 <- copyToRegister r1
    emit $ QAss r1 r2


runStmt (Incr varName) = do
    (r1, t1) <- translateExpression (EVar varName)
    when (t1 /= Int)
        (throwError $ badTypesSuggestion "++" Int t1)
    l1 <- getLoc varName
    emit $ QInc l1

runStmt (Decr varName) = do
    (r1, t1) <- translateExpression (EVar varName)
    when (t1 /= Int)
        (throwError $ badTypesSuggestion "--" Int t1)
    l1 <- getLoc varName
    emit $ QDec l1

runStmt (Ret expr) = do
    (r1, t1) <- translateExpression expr
    expectedType <- lgets expectRetType
    when (t1 /= expectedType)
        (throwError $ badTypesSuggestion "return statement" expectedType t1)
    updateCaughtRetType (t1, True)
    emit $ QRet r1

runStmt (VRet) = do
    expectedType <- lgets expectRetType
    when (expectedType /= Void)
        (throwError $ badTypesSuggestion "return statement" expectedType Void)
    updateCaughtRetType (Void, True)
    emit $ QRetV

runStmt (Cond ELitTrue stmt) = runStmt stmt

runStmt (Cond ELitFalse stmt) = catchRet (runStmt stmt) >> return ()

runStmt (Cond expr stmt) = do
    (r1, t1) <- translateExpression expr
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
    updateCaughtRetType retOld

runStmt (CondElse ELitFalse stmt1 stmt2) = do
    catchRet $ runStmt stmt1
    runStmt stmt2

runStmt (CondElse expr stmt1 stmt2) = do
    (lab1, lname1) <- newLabel
    (lab2, lname2) <- newLabel
    (r1, t1) <- translateExpression expr
    lName1 <- makeLabelN lname1
    lQuad1 <- makeLabelQ lname1
    lName2 <- makeLabelN lname2
    lQuad2 <- makeLabelQ lname2
    emit $ QCmpNe r1 (RegInt 0)
    emit $ QJne lName1
    when (t1 /= Bool)
        (throwError $ badTypesSuggestion "condition statement" Bool t1)
    (_, oldWasCaught) <- lgets caughtRetType
    (ret1, wasCaught1) <- catchRet $ runStmt stmt1
    emit $ QJmp lName2
    emit $ lab1
    (ret2, wasCaught2) <- catchRet $ runStmt stmt2
    emit $ lab2
    when (not oldWasCaught && wasCaught1 && wasCaught2 && (ret1 == ret2))
        (updateCaughtRetType (ret1, True))

runStmt (While expr stmt) = do
    (lab1, lname1) <- newLabel
    (lab2, lname2) <- newLabel
    lName1 <- makeLabelN lname1
    lQuad1 <- makeLabelQ lname1
    lName2 <- makeLabelN lname2
    lQuad2 <- makeLabelQ lname2
    emit lab1
    (r1, t1) <- translateExpression expr
    emit $ QCmpNe r1 (RegInt 0)
    emit $ QJne lName2
    when (t1 /= Bool)
        (throwError $ badTypesSuggestion "while statement" Bool t1)
    (retOld, oldWasCaught) <- lgets caughtRetType
    (newRet, newWasCaught) <- catchRet $ runStmt stmt
    when ((not oldWasCaught) && newWasCaught)
        (updateCaughtRetType (newRet, True))
    emit $ QJmp lName1
    emit lab2

runStmt (SExp expr) = do
    translateExpression expr
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
    updateExpectRetType retType
    mapM_ (uncurry declare) (zip argNames argTypes)
    (caughtRet, _) <- catchRet $ runBlock block
    when (caughtRet /= retType)
        (throwError $ badReturn funName retType)
    cod <- lgets code
    ProgState memory locs bs fl ert crt fr cd flab funs <- get
    put $ ProgState memory locs bs fl ert crt fr [] flab funs

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
    freeLoc <- lgets freeLocation
    updateBlockStart freeLoc
    clearMemory
    mapM (preserveState . defineFun) defList

runText :: String -> IO [QBlock]
runText s = let ts = myLexer s in case pProgram ts of
    Bad s -> do hPutStrLn stderr "\nParse              Failed...\n"
                hPutStrLn stderr s
                exitFailure
    Ok tree -> case runEval (ProgState Map.empty Map.empty 0 0 Void (Void, False) [Reg1, Reg2, Reg3] [] 0 Map.empty) (runProgram tree) of
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
