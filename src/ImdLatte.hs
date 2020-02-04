{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module ImdLatte where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Category
import Data.Label hiding (get)
import Prelude hiding ((.), id)
import qualified Data.Label as L
import qualified Data.List as List
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe

import AbsLatte
import ASM
import CFG (splitIntoBlocks, buildCFG, calcInOut, Graph)
import NextUses (appendNextUses)
import QuadData

import Test.QuickCheck
import Test.QuickCheck.Monadic

compose x [] = x
compose x (y:ys) = compose (y x) ys

------------------------
----------types and  functions used to operate on the types
-----------------------

type NextUse = Int
type MemoryLocation = Int

data QuadProgState = QuadProgState { _addressDesc :: AddressDescriptions -- replace this set with two sets/lists
                 , _registerDesc :: Map.Map Reg (Set.Set Var)
                 , _firstFree :: Int
                 , _nextUseInfo :: Map.Map Var NextUse
                 , _varLocations :: Map.Map Var MemoryLocation
                 }
                 deriving (Eq, Ord, Show, Read)
mkLabels [''QuadProgState]

lgets :: MonadState f m => f :-> a -> m a
lgets f = gets (L.get f)

type Eval ev = StateT QuadProgState (WriterT [ASM] (ExceptT String Identity)) ev
runEval mem ev = runIdentity $ runExceptT (runWriterT (runStateT ev mem))

update :: QuadProgState :-> a -> a -> Eval ()
update fun val = do
    state <- get
    put $ set fun val state

emit :: ASM -> Eval ()
emit asm = tell [asm]














alterAll f m = result where
    keys = Map.keys m
    result = foldl (flip (Map.alter f)) m keys

--- this remove vals from all set values (and also the key if the set turns out empty)
removeFromAllSets val m = result where
    deleteElement (Just s) = case Set.null $ Set.delete val s of
        True -> Nothing
        False -> Just $ Set.delete val s
    deleteElement Nothing = Nothing
    result = alterAll deleteElement m

replaceVarWithReg v1 v2 m = result where
    replaceElement (Just (regs, vars)) = case elem v1 vars of
        True -> Just $ (Set.insert v2 regs, Set.delete v1 vars)
        False -> Just (regs, vars)
    replaceElement Nothing = Nothing
    result = alterAll replaceElement m

removeRegFromDescs reg m = result where
    removeReg Nothing = Nothing
    removeReg (Just (regs, vars)) = Just (Set.delete reg regs, vars)
    result = alterAll removeReg m

addressDescInsertRegister k v = let
    alterFun Nothing = Just $ (Set.singleton v, Set.empty)
    alterFun (Just (regs, vals)) = Just (Set.insert v regs, vals) in do
    adesc <- lgets addressDesc
    update addressDesc (Map.alter alterFun k adesc)

---------------------------------------------------
----- functions calculating next uses of quadruples
-------------------------------------------------------


-------------------------------------
--------------------------------------------------
--------------------------------------------------
----------------------------------------










-- it assumes that register holds some vars
-- for a register finds the next line it's used in
nextRegisterUse :: Reg -> Eval Int
nextRegisterUse reg = do
    varUses <- lgets nextUseInfo
    rdesc <- lgets registerDesc
    case Map.lookup reg rdesc of
        Just heldVars -> return firstUseLine where
            filterHeldVars = (flip elem heldVars) . fst
            varUsesList = Map.toList varUses
            firstUseLine = minimum $ map snd (filter filterHeldVars varUsesList)
        Nothing -> error "trying to spill unused register"

-- it assumes that all registers hold some vars (spilling)
-- finds the register which holds variable that is used as last
-- TODO test it
furthestBusyRegister = do
    rdesc <- lgets registerDesc
    varUses <- lgets nextUseInfo
    regNextUseLines <- mapM nextRegisterUse (Map.keys rdesc)
    let
        furthestLine = maximum regNextUseLines
        regsWithLines = zip (Map.keys rdesc) (regNextUseLines)
        allPairsWithFurthestLine = filter ((furthestLine==) . snd) regsWithLines in
        return $ fst (head allPairsWithFurthestLine)


-----------------------------------
whereInMemory :: Var -> Eval AmdArg
whereInMemory var = do
    varLocs <- lgets varLocations
    case Map.lookup var varLocs of
        Just loc -> return $ AAMem loc
        Nothing -> do
            fl <- lgets firstFree
            update firstFree (fl+1)
            return $ AAMem fl


varDirty :: Var -> Eval Bool
varDirty var = do
    adesc <- lgets addressDesc
    case Map.lookup var adesc of
        Just (regs, vars) -> return $ not (Set.member var vars)
        Nothing -> error "wrong var"


--helper, assumes that var is in reg. It makes sure that var is in it's position in memory. reg will hold everything that
-- was in ver's memory positon
saveVarToMemory :: Reg -> Var -> Eval ()
saveVarToMemory reg var = do
    adesc <- lgets addressDesc
    rdesc <- lgets registerDesc
    whereMem <- whereInMemory var
    emit $ APush whereMem
    emit $ AMov (AAReg reg) whereMem
    emit $ APop (AAReg reg)

    update addressDesc (replaceVarWithReg var reg adesc)

    let
        filterFun k (regs, vars) = (Set.member var vars)
        varsHeldByReg = Set.fromList $ Map.keys (Map.filterWithKey filterFun adesc) in
        update registerDesc (Map.insert reg varsHeldByReg rdesc)


spillReg :: Reg -> Eval ()
spillReg reg = do
    rdesc <- lgets registerDesc
    case Map.lookup reg rdesc of
        Just heldVars -> do
            varsToSave <- filterM varDirty (Set.toList heldVars)
            case varsToSave of
                (h:t) -> do
                    saveVarToMemory reg h
                    spillReg reg
                [] -> return ()
        Nothing -> return ()

saveVarsAfterBlock :: [Var] -> Eval ()
saveVarsAfterBlock vars = do
    adesc <- lgets addressDesc
    rdesc <- lgets registerDesc
    dirtyVars <- filterM varDirty vars

    -- TODO can be improved
    whereNow <- mapM (whereVarPreferReg . QaVar) dirtyVars
    whereShould <- mapM whereInMemory dirtyVars

    mapM_ (emit . APush) whereNow
    mapM_ (emit . APop) (reverse whereShould)

    let
        sets = zip (repeat Set.empty) (map Set.singleton vars)
        newMap = Map.fromList $ zip vars sets in
        update addressDesc newMap


















---------------------------------------------------
---------------------------------------------------------------
------------------find register that holds only vars if there is such-----------------
findExclusiveRegister :: Var -> Eval (Maybe Reg)
findExclusiveRegister var = do
    rdesc <- lgets registerDesc
    let
        filterFun key val = (elem key (Map.keys rdesc)) && (val == (Set.singleton var))
        registersHoldingOnlyVar = Map.keys $ Map.filterWithKey filterFun rdesc in
        case registersHoldingOnlyVar of
            (reg:_) -> return $ Just reg
            [] -> return Nothing






-- If y is in register that holds only one variable and y has no next uses, give the register holding y
--- if failed, find next free register
-- if failed spill
giveReg :: QArgument -> Eval (Reg)
giveReg (QaVar y) = do
    nextUses <- lgets nextUseInfo
    adesc <- lgets addressDesc
    rdesc <- lgets registerDesc

    exclusiveReg <- findExclusiveRegister y
    case (exclusiveReg, Map.lookup y nextUses) of
        (Just reg, Nothing) -> do
            update addressDesc (removeRegFromDescs reg adesc) -- register still holds value, not sure if it's ok
            return reg
        _ -> case workingRegisters List.\\ (Map.keys rdesc) of -- check if there are any free registers
            (reg:_) -> return reg
            [] -> do
                reg <- furthestBusyRegister
                spillReg reg
                return reg

giveReg (QaConst y) = do
    rdesc <- lgets registerDesc
    case workingRegisters List.\\ (Map.keys rdesc) of -- check if there are any free registers
        (reg:_) -> return reg
        [] -> do
            reg <- furthestBusyRegister --TODO perhaps fix it, beacause the implemention is old
            spillReg reg
            return reg

giveReg _ = error "giveReg used in wrong place"


isRegister :: AmdArg -> Bool
isRegister (AAReg _) = True
isRegister (AAMem _) = False
isRegister (AAConst _) = False

whereVarPreferReg :: QArgument -> Eval AmdArg
whereVarPreferReg (QaVar var) = do
    rdesc <- lgets registerDesc
    adesc <- lgets addressDesc
    case Map.lookup var adesc of
        Just (regs, vars) -> case Set.null regs of
            True -> whereInMemory (head $ Set.toList vars)
            False -> return $ AAReg (head (Set.toList regs))
        Nothing -> error "var is nowhere"

whereVarPreferReg (QaConst c) = return $ AAConst c


updateNextUses :: (Map.Map Var NextUse) -> Eval ()
updateNextUses newInfo = do
    oldInfo <- lgets nextUseInfo
    update nextUseInfo (Map.union newInfo oldInfo)


forgetIfUnused :: Var -> Eval ()
forgetIfUnused var = do
    rdesc <- lgets registerDesc
    adesc <- lgets addressDesc
    nextUses <- lgets nextUseInfo

    case Map.lookup var nextUses of
        Just line -> return ()
        Nothing -> do
            update addressDesc (Map.delete var adesc)
            update registerDesc (removeFromAllSets var rdesc)










---
--- Translating single quadruple to assembly
---
quadToAsm (QuadNoAssign OpRet QaEmpty QaEmpty) nu = do
    updateNextUses nu
    emit ARet

quadToAsm (QuadNoAssign OpRet arg QaEmpty) nu = do
    updateNextUses nu
    case arg of
        QaVar v -> do
            wm <- whereVarPreferReg (QaVar v)
            emit $ AMov wm (AAReg Rax)
            emit $ ARet
            --case Map.lookup v vl of
            --    Just mem -> do
            --        emit $ AMov (AAMem mem) (AAReg Rax)
            --        emit ARet
            --    Nothing -> error "variable not declared"
        QaConst c -> do
            emit $ AMov (AAConst c) (AAReg Rax)
            emit ARet

--TODO fix assignment
quadToAsm (QuadNoAssign (OpJmp label) QaEmpty QaEmpty) nu = do
    updateNextUses nu
    emit $ AJmp label
quadToAsm (QuadNoAssign (OpLabel label) QaEmpty QaEmpty) nu = do
    updateNextUses nu
    emit $ ALab label
quadToAsm (QuadNoAssign (OpGoToIfFalse label) arg1 QaEmpty) nu = do
    whereArg <- whereVarPreferReg arg1
    emit $ ACmp (AAConst 1) whereArg
    emit $ AJmpNe label
quadToAsm (Quad4 x (OpAllocString pos len) QaEmpty QaEmpty) nu = error "TODO"
quadToAsm (Quad4 x (OpCall fname) args QaEmpty) nu = error "TODO"
quadToAsm (Quad4 x (OpAssVar) (QaVar y) _) nu = let
    alterRegs (Nothing) = Nothing
    alterRegs (Just s) = case Set.member y s of
        True -> Just $ Set.insert x s
        False -> Just s in do
    updateNextUses nu
    adesc <- lgets addressDesc
    rdesc <- lgets registerDesc
    update registerDesc (alterAll alterRegs rdesc)
    case Map.lookup y adesc of
        Just yVal -> update addressDesc (Map.insert x yVal adesc)
        Nothing -> error "assign error"

quadToAsm (Quad4 x (OpAssVar) (QaConst y) _) nu = do
    updateNextUses nu
    whereX <- whereInMemory x
    emit $ AMov (AAConst y) whereX

quadToAsm q@(Quad4 x op y z) nextUses = do
    rdesc <- lgets registerDesc
    adesc <- lgets addressDesc
    updateNextUses nextUses
    case Map.lookup x nextUses of
        Just xline -> do
            l <- giveReg y
            case y of
                QaVar vy -> case (Just $ Set.singleton vy) /= (Map.lookup l rdesc) of
                    True -> do
                        emit $ AMov (AAMem vy) (AAReg l)
--                        update registerDesc (Map.insert l (Set.singleton vy) rdesc)
--                        addressDescInsertRegister vy l
                    False -> return ()
                QaConst cy -> return ()
            case z of
                _ -> do
                    zp <- whereVarPreferReg z
                    case (zp, op) of
                        (zarg, OpAdd) -> emit $ AAdd zarg (AAReg l)
                        (zarg, OpSub) -> emit $ ASub zarg (AAReg l)
                        (zarg, OpMul) -> emit $ AMul zarg (AAReg l)
                        (zarg, OpDiv) -> do
                            emit $ AMov (AAReg l) (AAReg Rax)
                            emit $ ACdq
                            emit $ ADiv zarg
                            emit $ AMov (AAReg Rax) (AAReg l)
                        (zarg, OpMod) -> do
                            emit $ AMov (AAReg l) (AAReg Rax)
                            emit $ ACdq
                            emit $ ADiv zarg
                            emit $ AMov (AAReg Rdx) (AAReg l)
                        (zarg, OpNeg) -> emit $ ANeg (AAReg l)
                        (zarg, OpAnd) -> emit $ AAnd zarg (AAReg l)
                        (zarg, OpOr) -> emit $ AOr zarg (AAReg l)
                        (zarg, OpCmpIntLt) -> do
                            emit $ ACmp zp (AAReg l)
                            emit $ ASetLt
                            emit $ AMov (AAReg Rax) (AAReg l)
                        (zarg, OpCmpIntLe) -> do
                            emit $ ACmp zp (AAReg l)
                            emit $ ASetLe
                            emit $ AMov (AAReg Rax) (AAReg l)
                        (zarg, OpCmpIntGt) -> do
                            emit $ ACmp zp (AAReg l)
                            emit $ ASetGt
                            emit $ AMov (AAReg Rax) (AAReg l)
                        (zarg, OpCmpIntGe) -> do
                            emit $ ACmp zp (AAReg l)
                            emit $ ASetGe
                            emit $ AMov (AAReg Rax) (AAReg l)
                        (zarg, OpCmpIntEq) -> do
                            emit $ ACmp zp (AAReg l)
                            emit $ ASetEq
                            emit $ AMov (AAReg Rax) (AAReg l)
                        (zarg, OpCmpIntNe) -> do
                            emit $ ACmp zp (AAReg l)
                            emit $ ASetNe
                            emit $ AMov (AAReg Rax) (AAReg l)

            update addressDesc (Map.insert x (Set.singleton l, Set.empty) adesc)
            update registerDesc (compose rdesc [removeFromAllSets x, Map.insert l (Set.singleton x)])
            case y of
                QaVar vy -> forgetIfUnused vy
                QaConst cy-> return ()
            case z of
                QaVar vz -> forgetIfUnused vz
                _ -> return ()
        Nothing -> return ()


quadsToAsm wrappedNextUses aliveBlockEnd = do
    mapM_ (uncurry quadToAsm) wrappedNextUses
    saveVarsAfterBlock aliveBlockEnd

--- translates block of quads to assembly, can modify memory
runQuadsToAsm :: [Quad] -> [Var] -> Map.Map Var MemoryLocation -> Int-> (Map.Map Var MemoryLocation, Int, [ASM])
runQuadsToAsm quads aliveBlockEnd varLocs fFree =
    let
        initSingleAd ad = (ad, (Set.empty, Set.singleton ad))
        initAddressDesc = Map.fromList $ map initSingleAd (Map.keys varLocs)
        initRegisterDesc = Map.empty
        initFirstFree = fFree
        initNextUseInfo = Map.empty
        initVarLocations = varLocs
        (wrapNextUses, aliveStart) = appendNextUses quads aliveBlockEnd
        pState = QuadProgState initAddressDesc initRegisterDesc initFirstFree initNextUseInfo initVarLocations
        evalResult = runEval pState (quadsToAsm wrapNextUses aliveBlockEnd) in
        case evalResult of
            Right ((a, s), w) -> (_varLocations s, _firstFree s, w)





-----------------------------
---- building CFG
--------------------------




--------------------------
------Calculating in-out for all blocks in CFG
-------------------------





------------------------------
------------------------------
------------------------------








-----------
-----------
-------------


loadArgument i = case i of
    0 -> AMov (AAReg Rdi) (AAMem 0)
    1 -> AMov (AAReg Rsi) (AAMem 1)
    2 -> AMov (AAReg Rdx) (AAMem 2)
    3 -> AMov (AAReg Rcx) (AAMem 3)
    4 -> AMov (AAReg R8) (AAMem 4)
    5 -> AMov (AAReg R9) (AAMem 5)
    i -> APop (AAMem i)

loadArguments :: Int -> [ASM]
loadArguments args = map loadArgument [0..(args-1)]

--- MAIN function
--- returns assembly and the number of vars to allocate on stack
--- number of vars isn't known before translation, because asm can
--- in some cases save temporary values to the memory (and need some more than local variables)
functionToASM :: QuadFunction -> [ASM]
functionToASM (QuadFunction (Ident name) quads argFreeMem args memory) = result where
    prolog = [APush (AAReg Rbp), AMov (AAReg Rsp) (AAReg Rbp)]
    loadArgs = loadArguments args
    epilog = [ALeave]

    varsInMemory = Map.keys memory
    aDescVals = zip (repeat Set.empty) (map Set.singleton varsInMemory)
    addressDesc = Map.fromList $ zip varsInMemory aDescVals

    blocks = splitIntoBlocks quads
    numberedBlocks = zip [1..] blocks
    graph = buildCFG blocks
    (inInfo, outInfo) = calcInOut graph blocks

    blocksToAsm ((i, q):t) mem freeLoc = case Map.lookup i outInfo of
        Just alive -> case runQuadsToAsm q alive mem freeLoc of
            (newMem, newFree, asm1) -> case blocksToAsm t newMem newFree of
                (asm2, fm2) -> (asm1 ++ asm2, fm2)
        Nothing -> error "fatal"
    blocksToAsm [] mem freeLoc = ([], freeLoc)

    shortenMem (vname, (position, ty)) = (vname, position)
    shortMem = Map.fromList $ map shortenMem (Map.toList memory)


    (asm, freeMem) = blocksToAsm (zip [1..] blocks) (shortMem) (argFreeMem)
    allocMemory = [ASub (AAConst (8 * freeMem)) (AAReg Rsp)]
    resultAsm = prolog ++ allocMemory ++ loadArgs ++ asm ++ epilog
    result = resultAsm


hehe = QuadFunction (Ident "main") [Quad4 7 OpAssVar (QaConst 12) QaEmpty,Quad4 11 OpAdd (QaVar 6) (QaVar 2),Quad4 10 OpAdd (QaVar 11) (QaVar 3),Quad4 9 OpAdd (QaVar 10) (QaVar 4),Quad4 8 OpMul (QaVar 9) (QaVar 5),QuadNoAssign OpRet (QaVar 8) QaEmpty] 8 5 (Map.fromList [(1,(1,Int)),(2,(2,Int)),(3,(3,Int)),(4,(4,Int)),(5,(5,Int)),(6,(6,Int)),(7,(7,Int))])
hehe1 = QuadFunction (Ident "s") [Quad4 1 OpAssVar (QaConst 1) QaEmpty,Quad4 2 OpAssVar (QaConst 2) QaEmpty,Quad4 3 OpAdd (QaVar 1) (QaVar 2),QuadNoAssign OpRet (QaVar 3) QaEmpty] 3 0 (Map.fromList [(1,(1,Int)),(2,(2,Int))])
hehe2 = QuadFunction (Ident "s") [Quad4 1 OpAssVar (QaConst 1) QaEmpty,Quad4 2 OpAssVar (QaVar 1) QaEmpty,Quad4 3 OpAdd (QaVar 1) (QaVar 2),QuadNoAssign OpRet (QaVar 3) QaEmpty] 3 0 (Map.fromList [(1,(1,Int)),(2,(2,Int))])
hehe3 = QuadFunction (Ident "s") [Quad4 1 OpAssVar (QaConst 1) QaEmpty,Quad4 2 OpAssVar (QaVar 1) QaEmpty,Quad4 3 OpAdd (QaVar 2) (QaVar 1),QuadNoAssign OpRet (QaVar 3) QaEmpty] 3 0 (Map.fromList [(1,(1,Int)),(2,(2,Int))])
hehe4 = QuadFunction (Ident "s") [Quad4 2 OpAssVar (QaVar 1) QaEmpty,Quad4 3 OpAdd (QaVar 2) (QaVar 1),QuadNoAssign OpRet (QaVar 3) QaEmpty] 2 1 (Map.fromList [(1,(0,Int)),(2,(1,Int))])
hehe5 = QuadFunction (Ident "s") [Quad4 1 OpAssVar (QaVar 0) QaEmpty,Quad4 2 OpAdd (QaVar 0) (QaVar 1),QuadNoAssign OpRet (QaVar 2) QaEmpty] 2 1 (Map.fromList [(0,(0,Int)),(1,(1,Int))])
r = functionToASM hehe
r1 = functionToASM hehe1
r2 = functionToASM hehe2
r3 = functionToASM hehe3
r4 = functionToASM hehe4
r5 = functionToASM hehe5
