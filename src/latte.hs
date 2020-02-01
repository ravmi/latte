{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Main where

import Asm64

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



type Location = Int
-- describes where are the variables located
type Locations = Map.Map Ident Location
-- describes what memory looks like (only types for now)
type Memory = Map.Map Location (QArgument, Type)

-- in current state we want to keep locations of variables, state of the memory,
-- current free avaliable memory location, location of beginning of the block
-- and expected return type
data ProgState = ProgState { _memory :: Memory
                 , _locations :: Locations
                 , _blockStart :: Location
                 , _freeLocation :: Location
                 , _expectRetType :: Type
                 , _caughtRetType :: (Type, Bool)
                 , _freeRegisters :: [QArgument]
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

getRegister :: Eval (QArgument)
getRegister = do
    freeRegs <- lgets freeRegisters
    firstReg <- return $ head freeRegs
    update freeRegisters (tail freeRegs)
    return $ firstReg

putRegister :: QArgument -> Eval ()
putRegister reg = do
    freeRegs <- lgets freeRegisters
    case reg of
        Reg 1 -> update freeRegisters (Reg 1:freeRegs)
        Reg 2 -> update freeRegisters (Reg 2:freeRegs)
        Reg 3 -> update freeQArguments (Reg 3:freeRegs)
        _ -> return ()

update :: ProgState :-> a -> a -> Eval ()
update fun val = do
    state <- get
    put $ set fun val state

lgets :: MonadState f m => f :-> a -> m a
lgets f = gets (L.get f)

newLabel :: Eval (Integer)
newLabel = do
    flab <- lgets freeLabel
    update freeLabel (flab + 1)
    return $ flab

makeLabelN :: Integer -> Eval Ident
makeLabelN i = do
    return $ Ident (".L" ++ (show i))

makeLabelQ :: Integer -> Eval Quadruple
makeLabelQ i = do
    return $ QLab (Ident ("L" ++ (show i)))

insertMemory :: Location -> (QArgument, Type) -> Eval ()
insertMemory key val = do
    mem <- lgets memory
    update memory (Map.insert key val mem)

insertLocations :: Ident -> Location -> Eval ()
insertLocations key val = do
    locs <- lgets locations
    update locations (Map.insert key val locs)

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
    update caughtRetType oldRet
    return newRet

allocateVar :: Ident -> Type -> Eval Int
allocateVar varName varType = do
    freeLoc <- lgets freeLocation
    locs <- lgets locations
    insertMemory freeLoc (Mem freeLoc, varType)
    insertLocations varName freeLoc
    update freeLocation (freeLoc + 1)
    return freeLoc

lookupVar :: Ident -> Eval (QArgument, Type)
lookupVar name = do
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

getLoc :: Ident -> Eval (QArgument)
getLoc name = do
    locs <- lgets locations
    mem <- lgets memory
    case Map.lookup name locs of
        Just location -> return $ Mem location
        _ -> throwError $ varUninitialized name

copyToRegister :: QArgument -> Eval QArgument
copyToRegister pos = do
    freeReg <- getRegister
    case pos of
        Mem i -> do emit $ QLoad pos freeReg
                    return freeReg
        RegInt i ->  do emit $ QLoad pos freeReg
                        return freeReg
        RegBool b -> do emit $ QLoad pos freeReg
                        return freeReg
        _ -> do putRegister freeReg
                return pos

pairsToLists :: [(QArgument, Type)] -> ([QArgument], [Type])
pairsToLists pairs = (map fst pairs, map snd pairs)

translateExpression :: Expr -> Eval (QArgument, Type)
translateExpression (EVar name) = lookupVar name
translateExpression (ELitInt i) = return (RegInt i, Int)
translateExpression (ELitTrue) = return (RegBool True, Bool)
translateExpression (ELitFalse) = return (RegBool False, Bool)
translateExpression (EApp funName funArgs) = do
    Fun expectedRet expectedArgTypes  <- lookupFunction funName
    results <- mapM translateExpression funArgs
    (regs, argTypes) <- return $ pairsToLists results
    when (argTypes /= expectedArgTypes)
       (throwError $ badApply funName expectedArgTypes argTypes)
    emit $ QCall funName regs
    mapM_ putRegister regs
    -- @TODO realloc registers
    nReg <- getRegister
    return (nReg, expectedRet)

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

-- static calculation stuff can be added here (both const, one const etc)
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
    when(t1 /= t2 || (t1 /= Str && t1 /= Int))
        (throwError $ badTypes "`+`" [t1, t2])
    r2 <- copyToRegister r2
    case t1 of
        Int -> do emit $ QAdd r1 r2
                  putRegister r1
                  return (r2, t1)
        Str -> do emit $ QConcat r1 r2
                  putRegister r1
                  return (r2, t1)

translateExpression (EAdd exp1 Minus exp2) = do
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when (t1 /= Int || t2 /= Int)
        (throwError $ badTypes "`+`" [t1, t2])
    r1 <- copyToRegister r1
    emit $ QSub r2 r1
    putRegister r2
    return (r1, t1)

translateExpression (ERel exp1 EQU exp2) = do
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when ((t1 /= t2) || (t1 /= Int && t1 /= Str && t1 /= Bool))
        (throwError $ badTypes "`==`" [t1, t2])
    r2 <- copyToRegister r2
    emit $ QCmpIntEq r1 r2
    putRegister r1
    return (r2, Bool)

translateExpression (ERel exp1 op exp2) = do
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when (t1 /= Int || t2 /= Int)
        (throwError $ badTypesSuggestion "comparison operator" [Int, Int] [t1, t2])
    r2 <- copyToRegister r2
    case op of
        LTH -> do emit $ QCmpIntLt r1 r2
        LE -> do  emit $ QCmpIntLe r1 r2
        GTH -> emit $ QCmpIntGt r1 r2
        GE -> do emit $ QCmpIntGe r1 r2
        EQU -> do emit $ QCmpIntEq r1 r2
        NE -> emit $ QCmpIntNe r1 r2
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
    update blockStart freeLoc
    mapM_ runStmt statements

declare :: Ident -> Type -> Eval (Int)
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
    putRegister r1

runStmt :: Stmt -> Eval ()
runStmt Empty = return ()

runStmt (BStmt block) = do
    ret <- preserveState $ catchRet (runBlock block)
    update caughtRetType ret

runStmt (Decl varType varInits) = mapM_ (declareItem varType) varInits
runStmt (Ass varName expr) = do
    (r1, t1) <- translateExpression expr
    (r2, t2) <- translateExpression (EVar varName)
    when (t1 /= t2)
        (throwError $ badTypesSuggestion "assignment" t2 t1)
    r1 <- copyToRegister r1
    emit $ QAss r1 r2
    putRegister r1


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
    update caughtRetType (t1, True)
    emit $ QRet r1

runStmt (VRet) = do
    expectedType <- lgets expectRetType
    when (expectedType /= Void)
        (throwError $ badTypesSuggestion "return statement" expectedType Void)
    update caughtRetType (Void, True)
    emit $ QRetV

runStmt (Cond ELitTrue stmt) = runStmt stmt

runStmt (Cond ELitFalse stmt) = catchRet (runStmt stmt) >> return ()

runStmt (Cond expr stmt) = do
    (r1, t1) <- translateExpression expr
    lname1 <- newLabel
    lName <- makeLabelN lname1
    lQuad <- makeLabelQ lname1
    emit $ QGoToIfNotEqual lName r1 (RegInt (-1))
    when (t1 /= Bool)
        (throwError $ badTypesSuggestion "condition statement" Bool t1)
    catchRet $ runStmt stmt
    emit lQuad
    return ()

runStmt (CondElse ELitTrue stmt1 stmt2) = do
    retOld <- catchRet $ runStmt stmt1
    runStmt stmt2
    update caughtRetType retOld

runStmt (CondElse ELitFalse stmt1 stmt2) = do
    catchRet $ runStmt stmt1
    runStmt stmt2

runStmt (CondElse expr stmt1 stmt2) = do
    lname1 <- newLabel
    lname2 <- newLabel
    (r1, t1) <- translateExpression expr
    lName1 <- makeLabelN lname1
    lQuad1 <- makeLabelQ lname1
    lName2 <- makeLabelN lname2
    lQuad2 <- makeLabelQ lname2
    emit $ QGoToIfNotEqual lName1 r1 (RegInt (-1))
    when (t1 /= Bool)
        (throwError $ badTypesSuggestion "condition statement" Bool t1)
    (_, oldWasCaught) <- lgets caughtRetType
    (ret1, wasCaught1) <- catchRet $ runStmt stmt1
    emit $ QJmp lName2
    emit $ lQuad1
    (ret2, wasCaught2) <- catchRet $ runStmt stmt2
    emit $ lQuad2
    when (not oldWasCaught && wasCaught1 && wasCaught2 && (ret1 == ret2))
        (update caughtRetType (ret1, True))

runStmt (While expr stmt) = do
    lname1 <- newLabel
    lname2 <- newLabel
    lName1 <- makeLabelN lname1
    lQuad1 <- makeLabelQ lname1
    lName2 <- makeLabelN lname2
    lQuad2 <- makeLabelQ lname2
    emit lQuad1
    (r1, t1) <- translateExpression expr
    emit $ QGoToIfNotEqual lName2 r1 (RegInt (-1))
    putRegister r1
    when (t1 /= Bool)
        (throwError $ badTypesSuggestion "while statement" Bool t1)
    (retOld, oldWasCaught) <- lgets caughtRetType
    (newRet, newWasCaught) <- catchRet $ runStmt stmt
    when ((not oldWasCaught) && newWasCaught)
        (update caughtRetType (newRet, True))
    emit $ QJmp lName1
    emit lQuad2

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
    funs <- lgets functions
    argTypes <- getArgTypes arguments
    case Map.lookup funName funs of
        Just t -> throwError $ alreadyDeclared funName
        _ -> update functions (Map.insert funName (Fun retType argTypes) funs)


-- the types are wrong here
insertFunction :: String -> Type -> Eval ()
insertFunction name funType = do
    funs <- lgets functions
    update functions (Map.insert (Ident name) funType funs)


modifyState :: ProgState :-> a -> Eval (b) -> Eval b
modifyState getter ev = do
    prevState <- get
    ret <- ev
    val <- lgets getter
    put prevState
    update getter val
    return ret

flushCode :: Eval ([Quadruple])
flushCode = do
    c <- lgets code
    update code []
    return c

defineFun :: TopDef -> Eval (QBlock)
defineFun (FnDef retType funName arguments block) = do
    argNames <- getArgNames arguments
    argTypes <- getArgTypes arguments
    update expectRetType retType
    update freeLocation (negate (length argNames))
    mapM_ (uncurry declare) (reverse $ zip argNames argTypes)
    update freeLocation 0
    (caughtRet, _) <- catchRet $ runBlock block
    when (caughtRet /= retType)
        (throwError $ badReturn funName retType)
    c <- flushCode

    return $ QBlock funName c (countLocals block) (length argNames)

declareNativeFunctions :: Eval ()
declareNativeFunctions = do
    insertFunction "printInt" (Fun Void [Int])
    insertFunction "printString" (Fun Void [Str])
    insertFunction "readInt" (Fun Int [])
    insertFunction "readString" (Fun Str [])
    return ()

runProgram :: Program -> Eval [QBlock]
runProgram (Program defList) = do
    declareNativeFunctions
    mapM_ declareFun defList
    freeLoc <- lgets freeLocation
    update blockStart freeLoc
    clearMemory
    mapM (preserveState . defineFun) defList

runText :: String -> IO [QBlock]
runText s = let ts = myLexer s in case pProgram ts of
    Bad s -> do hPutStrLn stderr "ERROR"
                hPutStrLn stderr "\nParse              Failed...\n"
                hPutStrLn stderr s
                exitFailure
    Ok tree -> case runEval (ProgState Map.empty Map.empty 0 0 Void (Void, False) [Reg 1, Reg 2, Reg 3, Reg 4, Reg 5] [] 0 Map.empty) (runProgram tree) of
        Right (code, w) -> return $ code
        Left errMessage -> do hPutStrLn stderr errMessage
                              exitFailure

getFilename :: String -> String
getFilename path = head $ splitOn "." fileName where
                   fileName = last $ splitOn "/" path

getDir :: String -> String
getDir path = intercalate "" dirsNoLast where
    dirs = splitOn "/" path
    dirsNoLast = take ((length dirs) - 1) dirs

main :: IO ()
main = do
    args <- getArgs
    case args of
        [s] -> do
            code <- readFile s
            quads <- runText code
            --print quads
            putStrLn $ "OK"
            writeFile ((getDir s) ++ "/" ++ (getFilename s) ++ ".s") (translateProgram quads)
        _ -> hPutStrLn stderr  "Only one argument!"
