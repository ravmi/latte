{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Main where


import Control.Category
import Data.Label hiding (get)
import Prelude hiding ((.), id)

import LatteErrors

import Debug.Trace

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

import CFG (buildCFG, calcInOut, splitIntoBlocks)

import QuadData
import ImdLatte (functionToASM)

import System.FilePath.Posix (dropExtension)


type Loc = Int
-- describes where are the variables located
type Locations = Map.Map Ident Loc
-- describes what memory looks like (only types for now)
type Memory = Map.Map Var (Loc, Type)

type StaticValue = String

-- in current state we want to keep locations of variables, state of the memory,
-- current free avaliable memory location, location of beginning of the block
-- and expected return type
data ProgState = ProgState { _memory :: Memory
                 , _blockStart :: Loc
                 , _freeLocation :: Loc
                 , _expectRetType :: Type
                 , _caughtRetType :: (Type, Bool)
                 , _code :: [Quad]
                 , _freeLabel :: Integer
                 , _functions :: Map.Map Ident Type
                 , _staticMemory :: Map.Map Int String
                 , _staticFirstFree :: Int
                 , _nextVarName :: Int
                 , _varNames :: Map.Map Ident Int
                 }
                 deriving (Eq, Ord, Show, Read)
mkLabels [''ProgState]

type Eval ev = StateT ProgState (WriterT String (ExceptT String Identity)) ev
runEval mem ev = runIdentity $ runExceptT (runWriterT (evalStateT ev mem))

preserveMemory :: Eval a -> Eval a
preserveMemory ev = do
    mem <- lgets memory
    bstart <- lgets blockStart
    floc <- lgets freeLocation
    nextVName <- lgets nextVarName
    vNames <- lgets varNames
    ret <- ev
    update memory mem
    update blockStart bstart
    update freeLocation floc
    update nextVarName nextVName
    update varNames vNames
    return ret

allocStatic :: String -> Eval (Int)
allocStatic str = do
    sFree <- lgets staticFirstFree
    sMem <- lgets staticMemory
    update staticMemory (Map.insert sFree (str ++ "\n") sMem)
    update staticFirstFree (sFree + (length str + 1))
    return sFree


emit :: Quad -> Eval ()
emit q = do
    cd <- lgets code
    update code (cd ++ [q])

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

makeLabelN :: Integer -> Eval String
makeLabelN i = do
    return $ ".L" ++ (show i)

makeLabelQ :: Integer -> Eval Quad
makeLabelQ i = return $ QuadNoAssign (OpLabel ("L" ++ (show i))) QaEmpty QaEmpty

insertMemory :: Var -> (Var, Type) -> Eval ()
insertMemory key val = do
    mem <- lgets memory
    update memory (Map.insert key val mem)

cleanup :: Eval ()
cleanup = do
    update memory Map.empty
    update blockStart 0
    update freeLocation 0
    update expectRetType (Void)
    update caughtRetType (Void, False)
    update nextVarName 0
    update varNames Map.empty


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

getNextVarName :: Eval Int
getNextVarName = do
    nv <- lgets nextVarName
    update nextVarName (nv+1)
    return nv

giveVarName :: Ident -> Eval (Int)
giveVarName iName = do
    numName <- lgets nextVarName
    update nextVarName (numName+1)
    vns <- lgets varNames
    update varNames (Map.insert iName numName vns)
    return numName

allocateVar :: Ident -> Type -> Eval Int
allocateVar varName varType = do
    freeLoc <- lgets freeLocation
    iName <- giveVarName varName
    --locs <- lgets locations
    insertMemory iName (freeLoc, varType)
    --insertLocations varName freeLoc
    update freeLocation (freeLoc + 1)
    return iName


----- OK
lookupVar :: Ident -> Eval (Var, Type)
lookupVar name = do
    vNames <- lgets varNames
    mem <- lgets memory
    --case Map.lookup name vNames of
    --    Just iName -> case Map.lookup iName mem of
    --        Just varInfo -> return varInfo
    --        _ -> throwError $ varUninitialized name
    --    Nothing -> error "variables was not give a name?"
    case Map.lookup name vNames of
        Just iName -> case Map.lookup iName mem of
            Just (pos, typ) -> return (iName, typ)
            _ -> throwError $ varUninitialized name
        _ -> throwError $ varUninitialized name


lookupFunction :: Ident -> Eval (Type)
lookupFunction name = do
    funs <- lgets functions
    case Map.lookup name funs of
        Just fun -> return fun
        _ -> throwError $ varUninitialized name


pairsToLists :: [(QArgument, Type)] -> ([QArgument], [Type])
pairsToLists pairs = (map fst pairs, map snd pairs)


newVarName :: Eval (Var)
newVarName = do
    nfl <- lgets nextVarName
    update nextVarName (nfl+1)
    return nfl


translateExpression :: Expr -> Eval (QArgument, Type)
translateExpression (EVar name) = do
    (v, t) <- lookupVar name
    return (QaVar v, t)
translateExpression (ELitInt i) = return (QaConst (fromIntegral i), Int)
translateExpression (ELitTrue) = return (QaConst 1, Bool)
translateExpression (ELitFalse) = return (QaConst 0, Bool)

---
translateExpression (EApp funName funArgs) = do
    newVar <- newVarName
    Fun expectedRet expectedArgTypes  <- lookupFunction funName
    results <- mapM translateExpression funArgs
    (resultArgs, argTypes) <- return $ pairsToLists results
    when (argTypes /= expectedArgTypes)
       (throwError $ badApply funName expectedArgTypes argTypes)
    case funName of
        Ident strName -> emit $ Quad4 newVar (OpCall strName) (QaList resultArgs) QaEmpty
    return (QaVar newVar, expectedRet)

---TODO allocate stateic
translateExpression (EString str) = do
    newVar <- newVarName
    sMem <- allocStatic str
    return $ (QaConstStr ("." ++ (show sMem)), Str)
    -- emit $ Quad4 newVar (OpAllocString sMem (length str + 1)) QaEmpty QaEmpty
    --return $ (QaVar newVar, Str)

--- OK
translateExpression (Neg expr) = do
    newVar <- newVarName
    (expVar, t) <- translateExpression expr
    when (t /= Bool)
        (throwError $ badTypesSuggestion "`-`" Int t)
    emit $ Quad4 newVar OpNeg expVar QaEmpty
    return (QaVar newVar, Int)

--OK
translateExpression (Not expr) = do
    newVar <- newVarName
    (expVar, t) <- translateExpression expr
    when (t /= Bool)
        (throwError $ badTypesSuggestion "`!`" Bool t)
    emit $ Quad4 newVar OpSub (QaConst 1) expVar
    return (QaVar newVar, Bool)

-- static calculation stuff can be added here (both const, one const etc)
translateExpression (EMul e1 op e2) = do
    newVar <- newVarName
    (r1, t1) <- translateExpression e1
    (r2, t2) <- translateExpression e2
    when (t1 /= Int || t2 /= Int)
        (throwError $ badTypesSuggestion "`*`" [Int, Int] [t1, t2])
    case op of
        Times -> emit $ Quad4 newVar OpMul r2 r1
        Mod -> emit $ Quad4 newVar OpMod r1 r2
        Div -> emit $ Quad4 newVar OpDiv r1 r2
    return (QaVar newVar, t1)

translateExpression (EAdd exp1 Plus exp2) = do
    newVar <- newVarName
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when(t1 /= t2 || (t1 /= Str && t1 /= Int))
        (throwError $ badTypes "`+`" [t1, t2])
    case t1 of
        Int -> do emit $ Quad4 newVar OpAdd r2 r1
        Str -> do emit $ Quad4 newVar (OpCall "concatStrings_") (QaList [r1, r2]) QaEmpty
    return (QaVar newVar, t1)

translateExpression (EAdd exp1 Minus exp2) = do
    newVar <- newVarName
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when (t1 /= Int || t2 /= Int)
        (throwError $ badTypes "`+`" [t1, t2])
    emit $ Quad4 newVar OpSub r1 r2
    return (QaVar newVar, t1)

translateExpression (ERel exp1 EQU exp2) = do
    newVar <- newVarName
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when ((t1 /= t2) || (t1 /= Int && t1 /= Str && t1 /= Bool))
        (throwError $ badTypes "`==`" [t1, t2])
    emit $ Quad4 newVar OpCmpIntEq r1 r2
    return (QaVar newVar, Bool)

translateExpression (ERel exp1 op exp2) = do
    newVar <- newVarName
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when (t1 /= Int || t2 /= Int)
        (throwError $ badTypesSuggestion "comparison operator" [Int, Int] [t1, t2])
    case op of
        LTH -> do emit $ Quad4 newVar OpCmpIntLt r1 r2
        LE -> do  emit $ Quad4 newVar OpCmpIntLe r1 r2
        GTH -> emit $ Quad4 newVar OpCmpIntGt r1 r2
        GE -> do emit $ Quad4 newVar OpCmpIntGe r1 r2
        EQU -> do emit $ Quad4 newVar OpCmpIntEq r1 r2
        NE -> emit $ Quad4 newVar OpCmpIntNe r1 r2
    return (QaVar newVar, Bool)

translateExpression (EAnd exp1 exp2) = do
    newVar <- newVarName
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when (t1 /= Bool || t2 /= Bool)
        (throwError $ badTypesSuggestion "&&" [Bool, Bool] [t1, t2])
    emit $ Quad4 newVar OpAnd r1 r2
    return (QaVar newVar, Bool)

translateExpression (EOr exp1 exp2) = do
    newVar <- newVarName
    (r1, t1) <- translateExpression exp1
    (r2, t2) <- translateExpression exp2
    when (t1 /= Bool || t2 /= Bool)
        (throwError $ badTypesSuggestion "||" [Bool, Bool] [t1, t2])
    emit $ Quad4 newVar OpOr r1 r2
    return (QaVar newVar, Bool)









runBlock :: Block -> Eval ()
runBlock (Block statements) = do
    freeLoc <- lgets freeLocation
    update blockStart freeLoc
    mapM_ runStmt statements

declare :: Ident -> Type -> Eval (Int)
declare varName varType = do
    mem <- lgets memory
    startLoc <- lgets blockStart
    vNames <- lgets varNames
    case Map.lookup varName vNames of
        Just iName -> case Map.lookup iName mem of
            Just (location, _) -> if (location >= startLoc)
                then throwError $ alreadyDeclared varName
                else allocateVar varName varType
            _ -> error "declaring a variable that was declared but not named"
        _ -> allocateVar varName varType


declareItem :: Type -> Item -> Eval ()
declareItem expectedType (NoInit varName) = do
    declare varName expectedType
    return ()

declareItem expectedType (Init varName expr) = do
    (r1, t1) <- translateExpression expr
    when (t1 /= expectedType)
        (throwError $ badTypesSuggestion "assignment" expectedType t1)
    --r1 <- copyToRegister r1
    loc <- declare varName t1
    emit $ Quad4 loc OpAssVar r1 QaEmpty
    --putRegister r1









runStmt :: Stmt -> Eval ()
runStmt Empty = return ()


---TODO
runStmt (BStmt block) = do
    bs <- lgets blockStart
    names <- lgets varNames
    runBlock block
    update blockStart bs
    update varNames names
    --ret <- preserveState $ catchRet (runBlock block)
    --update caughtRetType ret

---TODO
runStmt (Decl varType varInits) = mapM_ (declareItem varType) varInits

runStmt (Ass varName expr) = do
    (r1, t1) <- translateExpression expr
    (r2, t2) <- lookupVar varName
    when (t1 /= t2)
        (throwError $ badTypesSuggestion "assignment" t2 t1)
    emit $ Quad4 r2 OpAssVar r1 QaEmpty

runStmt (Incr varName) = do
    (r1, t1) <- lookupVar varName
    when (t1 /= Int)
        (throwError $ badTypesSuggestion "++" Int t1)
    emit $ Quad4 r1 OpAdd (QaVar r1) (QaConst 1)

runStmt (Decr varName) = do
    (r1, t1) <- lookupVar varName
    when (t1 /= Int)
        (throwError $ badTypesSuggestion "--" Int t1)
    emit $ Quad4 r1 OpSub (QaVar r1) (QaConst 1)

runStmt (Ret expr) = do
    (r1, t1) <- translateExpression expr
    expectedType <- lgets expectRetType
    when (t1 /= expectedType)
        (throwError $ badTypesSuggestion "return statement" expectedType t1)
    update caughtRetType (t1, True)
    emit $ QuadNoAssign OpRet r1 QaEmpty

runStmt (VRet) = do
    expectedType <- lgets expectRetType
    when (expectedType /= Void)
        (throwError $ badTypesSuggestion "return statement" expectedType Void)
    update caughtRetType (Void, True)
    emit $ QuadNoAssign OpRet QaEmpty QaEmpty

runStmt (Cond ELitTrue stmt) = runStmt stmt

runStmt (Cond ELitFalse stmt) = catchRet (runStmt stmt) >> return ()

runStmt (Cond expr stmt) = do
    (r1, t1) <- translateExpression expr
    lname1 <- newLabel
    lName <- makeLabelN lname1
    lQuad <- makeLabelQ lname1
    emit $ QuadNoAssign (OpGoToIfFalse lName) r1 QaEmpty
    when (t1 /= Bool)
        (throwError $ badTypesSuggestion "condition statement" Bool t1)
    catchRet $ runStmt stmt
    emit $ QuadNoAssign (OpLabel lName) QaEmpty QaEmpty
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
    emit $ QuadNoAssign (OpGoToIfFalse lName1) r1 QaEmpty
    when (t1 /= Bool)
        (throwError $ badTypesSuggestion "condition statement" Bool t1)
    (_, oldWasCaught) <- lgets caughtRetType
    (ret1, wasCaught1) <- catchRet $ runStmt stmt1
    emit $ QuadNoAssign (OpJmp lName2) QaEmpty QaEmpty
    emit $ QuadNoAssign (OpLabel lName1) QaEmpty QaEmpty
    (ret2, wasCaught2) <- catchRet $ runStmt stmt2
    emit $ QuadNoAssign (OpLabel lName2) QaEmpty QaEmpty
    when (not oldWasCaught && wasCaught1 && wasCaught2 && (ret1 == ret2))
        (update caughtRetType (ret1, True))

runStmt (While expr stmt) = do
    lname1 <- newLabel
    lname2 <- newLabel
    lName1 <- makeLabelN lname1
    lQuad1 <- makeLabelQ lname1
    lName2 <- makeLabelN lname2
    lQuad2 <- makeLabelQ lname2
    emit $ QuadNoAssign (OpLabel lName1) QaEmpty QaEmpty
    (r1, t1) <- translateExpression expr
    emit $ QuadNoAssign (OpGoToIfFalse lName2) r1 QaEmpty
    when (t1 /= Bool)
        (throwError $ badTypesSuggestion "while statement" Bool t1)
    (retOld, oldWasCaught) <- lgets caughtRetType
    (newRet, newWasCaught) <- catchRet $ runStmt stmt
    when ((not oldWasCaught) && newWasCaught)
        (update caughtRetType (newRet, True))
    emit $ QuadNoAssign (OpJmp lName1) QaEmpty QaEmpty
    emit $ QuadNoAssign (OpLabel lName2) QaEmpty QaEmpty

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

flushCode :: Eval ([Quad])
flushCode = do
    c <- lgets code
    update code []
    return c

defineFun :: TopDef -> Eval (QuadFunction)
defineFun (FnDef retType funName arguments block) = do
    argNames <- getArgNames arguments
    argTypes <- getArgTypes arguments
    update expectRetType retType
    --update freeLocation (negate (length argNames))
    mapM_ (uncurry declare) (zip argNames argTypes)
    xx <- lgets freeLocation
    update nextVarName (xx + countLocals (block))
    --update freeLocation 0
    runBlock block
    (caughtRet, _) <- lgets caughtRetType
    update caughtRetType (Void, False)
    when (caughtRet /= retType)
        (throwError $ badReturn funName retType)
    c <- flushCode
    mem <- lgets memory
    sM <- lgets staticMemory
    fL <- lgets freeLocation
    cleanup
    return $ QuadFunction funName c (fL) (length argNames)  mem

declareNativeFunctions :: Eval ()
declareNativeFunctions = do
    insertFunction "printInt" (Fun Void [Int])
    insertFunction "printString" (Fun Void [Str])
    insertFunction "readInt" (Fun Int [])
    insertFunction "readString" (Fun Str [])

    insertFunction "concatStrings_" (Fun Str [Str, Str])
    return ()


-- gives list of translated functions and state of static memory
programToQuadsInMonad :: Program -> Eval ([QuadFunction], Map.Map Int String)
programToQuadsInMonad (Program defList) = do
    declareNativeFunctions
    mapM_ declareFun defList
    qBlocks <- mapM defineFun defList
    staticM <- lgets staticMemory
    return (qBlocks, staticM)




--functionToASM :: QuadFunction -> ([ASM], Int)


runText :: String -> IO String
runText s = let ts = myLexer s in case pProgram ts of
    Bad s -> do hPutStrLn stderr "ERROR"
                hPutStrLn stderr "\nParse              Failed...\n"
                hPutStrLn stderr s
                exitFailure
    Ok tree -> case runEval (ProgState Map.empty 0 0 Void (Void, False) [] 1 Map.empty Map.empty 0 0 Map.empty) (programToQuadsInMonad tree) of
        Right ((qfunctions, static), w) -> let
            globl n = ".globl " ++ n ++ "\n"
            functionName n = n ++ ":\n"
            showIndent asm = (intercalate "\n" (map (("    " ++) . show) (functionToASM asm)) ++ "\n")
            functionToString q@(QuadFunction (Ident fName) quads _ _ _) = (globl fName) ++ (functionName fName) ++ (showIndent q)
            bCFG ((QuadFunction _ quads _ _ _)) = buildCFG (splitIntoBlocks quads)
            bInOut q@(QuadFunction _ quads _ _ _) = calcInOut (bCFG q) (splitIntoBlocks quads)
            getblocks q@(QuadFunction _ quads _ _ _) = splitIntoBlocks quads
            codeText = concatMap functionToString qfunctions in do
                print qfunctions
                putStrLn $ concatMap printQuadFunction qfunctions
                --print "blocks"
                --print $ getblocks (head qfunctions)
                --print "CFG"
                --print (bCFG $ head qfunctions)
                --print "INOUT"
                --print (bInOut $ head qfunctions)
                putStrLn $ codeText
                return $ codeText

        Left errMessage -> do hPutStrLn stderr errMessage
                              exitFailure

--getFilename :: String -> String
--getFilename path = head $ splitOn "." fileName where
--                   fileName = last $ splitOn "/" path

--getDir :: String -> String
--getDir path = intercalate "" dirsNoLast where
--    dirs = splitOn "/" path
--    dirsNoLast = take ((length dirs) - 1) dirs

main :: IO ()
main = do
    args <- getArgs
    case args of
        [s] -> do
            code <- readFile s
            codeText <- runText code
            --print quads
            putStrLn $ "OK"
            --writeFile ((getDir s) ++ "/" ++ (takeBaseName s) ++ ".s") (codeText)
            writeFile ((dropExtension s) ++ ".s") codeText
        _ -> hPutStrLn stderr  "Only one argument!"


