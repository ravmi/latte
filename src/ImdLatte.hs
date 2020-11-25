{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module ImdLatte where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Category
import Prelude hiding ((.), id)
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
import Debug.Trace
import GarbageCollector (collectGarbageBlock)

import CFG

import Test.QuickCheck
import Test.QuickCheck.Monadic

compose x [] = x
compose x (y:ys) = compose (y x) ys

------------------------
----------types and  functions used to operate on the types
-----------------------

type NextUse = Int
type MemoryLocation = Int

data QuadProgState = QuadProgState { addressDesc :: AddressDescriptions
                 , registerDesc :: Map.Map Reg (Set.Set Var)
                 , firstFree :: Int
                 , nextUseInfo :: Map.Map Var NextUse
                 , varLocations :: Map.Map Var MemoryLocation
                 }
                 deriving (Eq, Ord, Show, Read)

updateAddressDesc :: AddressDescriptions -> Eval ()
updateAddressDesc x = do
    QuadProgState a b c d e <- get
    put $ QuadProgState x b c d e

updateRegisterDesc :: (Map.Map Reg (Set.Set Var)) -> Eval ()
updateRegisterDesc x = do
    QuadProgState a b c d e <- get
    put $ QuadProgState a x c d e

updateFirstFree :: Int -> Eval ()
updateFirstFree x = do
    QuadProgState a b c d e <- get
    put $ QuadProgState a b x d e

updateNextUseInfo :: Map.Map Var NextUse -> Eval ()
updateNextUseInfo x = do
    QuadProgState a b c d e <- get
    put $ QuadProgState a b c x e

updateVarLocations :: Map.Map Var MemoryLocation -> Eval ()
updateVarLocations x = do
    QuadProgState a b c d e <- get
    put $ QuadProgState a b c d x

lgets f = do
    s <- get
    return $ f s

type Eval ev = StateT QuadProgState (WriterT [ASM] (ExceptT String Identity)) ev
runEval mem ev = runIdentity $ runExceptT (runWriterT (runStateT ev mem))


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
    updateAddressDesc (Map.alter alterFun k adesc)

forgetVarFromRegDesc :: Var -> Eval ()
forgetVarFromRegDesc v = let
    alterFun Nothing = Nothing
    alterFun (Just s) = Just $ Set.delete v s in do
    rdesc <- lgets registerDesc
    updateRegisterDesc (alterAll alterFun rdesc)

forgetVarFromAddDesc :: Var -> Eval ()
forgetVarFromAddDesc v = let
    alterFun Nothing = Nothing
    alterFun (Just (regs, vars)) = Just $ (regs, Set.delete v vars) in do
    adesc <- lgets addressDesc
    updateAddressDesc (alterAll alterFun adesc)





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
furthestBusyRegister = do
    rdesc <- lgets registerDesc
    varUses <- lgets nextUseInfo
    regNextUseLines <- mapM nextRegisterUse (Map.keys rdesc)
    let
        furthestLine = maximum regNextUseLines
        regsWithLines = zip (Map.keys rdesc) (regNextUseLines)
        allPairsWithFurthestLine = filter ((furthestLine==) . snd) regsWithLines in
        case allPairsWithFurthestLine of
            (h:_) -> return $ fst h
            _ -> error "furthestBusy"


-----------------------------------
whereOrAlloc :: Var -> Eval AmdArg
whereOrAlloc var = do
    varLocs <- lgets varLocations
    case Map.lookup var varLocs of
        Just loc -> return $ AAMem loc
        Nothing -> do
            fl <- lgets firstFree
            updateFirstFree (fl+1)
            updateVarLocations (Map.insert var fl varLocs)
            return $ AAMem fl


varDirty :: Var -> Eval Bool
varDirty var = do
    adesc <- lgets addressDesc
    case Map.lookup var adesc of
        Just (regs, vars) -> return $ not (Set.member var vars)
        Nothing -> return True --error "wrong var"


--helper, assumes that var is in reg. It makes sure that var is in it's position in memory. reg will hold everything that
-- was in ver's memory positon
saveVarToMemory :: Reg -> Var -> Eval ()
saveVarToMemory reg var = do
    adesc <- lgets addressDesc
    rdesc <- lgets registerDesc
    whereMem <- whereOrAlloc var
    emit $ APush whereMem
    emit $ AMov (AAReg reg) whereMem
    emit $ APop (AAReg reg)

    updateAddressDesc (replaceVarWithReg var reg adesc)

    let
        filterFun k (regs, vars) = (Set.member var vars)
        varsHeldByReg = Set.fromList $ Map.keys (Map.filterWithKey filterFun adesc) in
        updateRegisterDesc (Map.insert reg varsHeldByReg rdesc)


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
    dirtyVars <- filterM varDirty vars

    whereNow <- mapM (whereVarPreferReg . QaVar) dirtyVars
    whereShould <- mapM whereOrAlloc dirtyVars

    mapM_ (emit . APush) whereNow
    mapM_ (emit . APop) (reverse whereShould)

    let
        sets = zip (repeat Set.empty) (map Set.singleton vars)
        newMap = Map.fromList $ zip vars sets in
        updateAddressDesc newMap



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
            updateAddressDesc (removeRegFromDescs reg adesc) -- register still holds value, not sure if it's ok
            return reg
        _ -> case workingRegisters List.\\ (Map.keys rdesc) of -- check if there are any free registers
            (reg:_) -> return reg
            [] -> do
                reg <- furthestBusyRegister
                spillReg reg
                return reg

giveReg arg = let
    giveRegConst = do
        rdesc <- lgets registerDesc
        case workingRegisters List.\\ (Map.keys rdesc) of -- check if there are any free registers
            (reg:_) -> return reg
            [] -> do
                reg <- furthestBusyRegister
                spillReg reg
                return reg in
    case arg of
        (QaConst _) -> giveRegConst
        (QaConstStr _) -> giveRegConst
        _ -> error "giveReg used in wrong place"


isRegister :: AmdArg -> Bool
isRegister (AAReg _) = True
isRegister (AAMem _) = False
isRegister (AAConst _) = False

whereVarPreferReg :: QArgument -> Eval AmdArg
whereVarPreferReg (QaVar var) = do
    rdesc <- lgets registerDesc
    adesc <- lgets addressDesc
    mem <- lgets varLocations
    case Map.lookup var adesc of
        Just (regs, vars) -> case Set.null regs of
            True -> case (List.intersect (Set.toList vars) (Map.keys mem)) of
                (h:_) -> whereOrAlloc h
                _ -> error "whereVarPreferReg1"
            False -> case (Set.toList regs) of
                (h:_) -> return $ AAReg h
                _ -> error "whereVarPreferRegs2"
        Nothing ->  (error "var is nowhere")

whereVarPreferReg (QaConst c) = return $ AAConst c
whereVarPreferReg (QaConstStr str) = return $ AAConstStr str


updateNextUses :: (Map.Map Var NextUse) -> Eval ()
updateNextUses newInfo = do
    oldInfo <- lgets nextUseInfo
    updateNextUseInfo (newInfo)
    --updateNextUseInfo (Map.union newInfo oldInfo)


forgetIfUnused :: QArgument -> Eval ()
forgetIfUnused y = do
    rdesc <- lgets registerDesc
    adesc <- lgets addressDesc
    nextUses <- lgets nextUseInfo

    case y of
        (QaVar var) -> case Map.lookup var nextUses of
            Just line -> return ()
            Nothing -> do
                updateAddressDesc (Map.delete var adesc)
                updateRegisterDesc (removeFromAllSets var rdesc)
        (QaConst c) -> return ()
        (QaConstStr _) -> return ()
        (QaEmpty) -> return ()


quadToAsm (QuadNoAssign OpSaveToHeap (QaList [ar, wher, what]) _) nu = do
    updateNextUses nu
    varWhere <- whereVarPreferReg wher
    varWhat <- whereVarPreferReg what
    varAr <- whereVarPreferReg ar
    emit $ AMov varWhere (AAReg Rax)
    emit $ AAdd varAr (AAReg Rax)
    emit $ AMov varWhat (AAReg Rdx)
    emit $ ASave (AAReg Rdx) (AAReg Rax)

quadToAsm (QuadNoAssign OpRet QaEmpty QaEmpty) nu = do
    updateNextUses nu
    emit ALeave
    emit ARet

quadToAsm (QuadNoAssign OpRet arg QaEmpty) nu = do
    updateNextUses nu
    case arg of
        QaEmpty -> emit ARet
        _ -> do
            wm <- whereVarPreferReg arg
            emit $ AMov wm (AAReg Rax)
            emit ALeave
            emit ARet

quadToAsm (QuadNoAssign (OpJmp label) QaEmpty QaEmpty) nu = do
    updateNextUses nu
    emit $ AJmp label

quadToAsm (QuadNoAssign (OpLabel label) QaEmpty QaEmpty) nu = do
    updateNextUses nu
    emit $ ALab label

quadToAsm (QuadNoAssign (OpGoToIfFalse label) arg1 QaEmpty) nu = do
    updateNextUses nu
    whereArg <- whereVarPreferReg arg1
    emit $ ACmp (AAConst 1) whereArg
    emit $ AJmpNe label

quadToAsm (QuadNoAssign (OpGoToIfTrue label) arg1 QaEmpty) nu = do
    updateNextUses nu
    whereArg <- whereVarPreferReg arg1
    emit $ ACmp (AAConst 1) whereArg
    emit $ AJmpE label

quadToAsm (Quad4 x (OpCall fname) (QaList args) QaEmpty) nu = let
    putArguments [] = []
    putArguments l@((i, arg):t) = case i of
        0 -> ((AMov arg (AAReg Rdi)):putArguments t)
        1 -> ((AMov arg (AAReg Rsi)):putArguments t)
        2 -> ((AMov arg (AAReg Rdx)):putArguments t)
        3 -> ((AMov arg (AAReg Rcx)):putArguments t)
        4 -> ((AMov arg (AAReg R8)):putArguments t)
        5 -> ((AMov arg (AAReg R9)):putArguments t)
        i -> map (APush . snd) (reverse l) in do
    case (length args) > 6 of
        False -> return ()
        True -> emit $ ASub (AAConst (((length args) - 6) * 8)) (AAReg Rsp)
    amdArgs <- mapM whereVarPreferReg args
    updateNextUses nu
    mapM_ emit (putArguments $ zip [0..] amdArgs)
    emit $ ACall fname
    case (length args) > 6 of
        False -> return ()
        True -> emit $ AAdd (AAConst (((length args) - 6) * 8)) (AAReg Rsp)
    mapM_ forgetIfUnused args
    case x == -1 of
        True -> return ()
        False -> do
            whereX <- whereOrAlloc x
            emit $ AMov (AAReg Rax) whereX
            forgetVarFromRegDesc x
            forgetVarFromAddDesc x
            adesc <- lgets addressDesc
            updateAddressDesc (Map.insert x (Set.empty, Set.singleton x) adesc)

quadToAsm (Quad4 x (OpAssVar) (QaVar y) _) nu = let
    alterRegs (Nothing) = Nothing
    -- if y was rememered in some register, then x should be as well
    alterRegs (Just s) = case Set.member y s of
        True -> Just $ Set.insert x s
        False -> Just s in do
    updateNextUses nu
    adesc <- lgets addressDesc
    rdesc <- lgets registerDesc
    updateRegisterDesc (alterAll alterRegs rdesc)
    case Map.lookup y adesc of
        -- positions of x are now the same as y
        Just yVal -> updateAddressDesc (Map.insert x yVal adesc)
        Nothing -> error "assign error"

quadToAsm (Quad4 x (OpAssVar) (QaConst y) _) nu = do
    updateNextUses nu
    whereX <- whereOrAlloc x
    emit $ AMov (AAConst y) whereX
    forgetVarFromRegDesc x
    forgetVarFromAddDesc x
    adesc <- lgets addressDesc
    updateAddressDesc (Map.insert x (Set.empty, Set.singleton x) adesc)

quadToAsm (Quad4 x (OpAssVar) (QaConstStr y) _) nu = do
    updateNextUses nu
    whereX <- whereOrAlloc x
    emit $ AMov (AAConstStr y) (AAReg Rax)
    emit $ AMov (AAReg Rax) whereX
    forgetVarFromRegDesc x
    forgetVarFromAddDesc x
    adesc <- lgets addressDesc
    updateAddressDesc (Map.insert x (Set.empty, Set.singleton x) adesc)

quadToAsm q@(Quad4 x op y z) nextUses = do
    rdesc <- lgets registerDesc
    updateNextUses nextUses
    nuses <- lgets nextUseInfo

    vlocs <- lgets varLocations


    case Map.lookup x nextUses of
        Just xline -> do
            l <- giveReg y
            case y of
                QaVar vy -> case (Just $ Set.singleton vy) /= (Map.lookup l rdesc) of
                    True -> do
                        whereY <- whereVarPreferReg y
                        emit $ AMov whereY (AAReg l)
                    False -> return ()
                QaConst cy -> do
                    emit $ AMov (AAConst cy) (AAReg l)
                QaConstStr cy -> do
                    emit $ AMov (AAConstStr cy) (AAReg l)
            -- there we should be sure that y is in new safe register, is is not marked as busy though
            case (z, op) of
                (zarg, OpLoadFromHeap) -> do
                    zarg2 <- whereVarPreferReg zarg
                    emit $ AAdd zarg2 (AAReg l)
                    emit $ ALoad (AAReg l) (AAReg l)
                (QaEmpty, OpNeg) -> emit $ ANeg (AAReg l)
                _ -> do
                    zp <- whereVarPreferReg z
                    case (zp, op) of
                        (zarg, OpAdd) -> emit $ AAdd zarg (AAReg l)
                        (zarg, OpSub) -> emit $ ASub zarg (AAReg l)
                        (zarg, OpMul) -> emit $ AMul zarg (AAReg l)
                        (zarg, OpDiv) -> do
                            emit $ AMov (AAReg l) (AAReg Rax)
                            emit $ ACdq
                            case zarg of
                                AAConst czarg -> do
                                    regzarg <- giveReg z
                                    emit $ AMov zarg (AAReg regzarg)
                                    emit $ ADiv (AAReg regzarg)
                                _ -> do
                                    emit $ ADiv zarg
                            emit $ AMov (AAReg Rax) (AAReg l)
                        (zarg, OpMod) -> do
                            emit $ AMov (AAReg l) (AAReg Rax)
                            emit $ ACdq
                            case zarg of
                                AAConst czarg -> do
                                    regzarg <- giveReg z
                                    emit $ AMov zarg (AAReg regzarg)
                                    emit $ ADiv (AAReg regzarg)
                                _ -> do
                                    emit $ ADiv zarg
                            emit $ AMov (AAReg Rdx) (AAReg l)
                        (zarg, OpAnd) -> emit $ AAnd zarg (AAReg l)
                        (zarg, OpOr) -> emit $ AOr zarg (AAReg l)
                        (zarg, OpCmpIntLt) -> do
                            emit $ AXor (AAReg Rax) (AAReg Rax)
                            emit $ ACmp zp (AAReg l)
                            emit $ ASetLt
                            emit $ AMov (AAReg Rax) (AAReg l)
                        (zarg, OpCmpIntLe) -> do
                            emit $ AXor (AAReg Rax) (AAReg Rax)
                            emit $ ACmp zp (AAReg l)
                            emit $ ASetLe
                            emit $ AMov (AAReg Rax) (AAReg l)
                        (zarg, OpCmpIntGt) -> do
                            emit $ AXor (AAReg Rax) (AAReg Rax)
                            emit $ ACmp zp (AAReg l)
                            emit $ ASetGt
                            emit $ AMov (AAReg Rax) (AAReg l)
                        (zarg, OpCmpIntGe) -> do
                            emit $ AXor (AAReg Rax) (AAReg Rax)
                            emit $ ACmp zp (AAReg l)
                            emit $ ASetGe
                            emit $ AMov (AAReg Rax) (AAReg l)
                        (zarg, OpCmpIntEq) -> do
                            emit $ AXor (AAReg Rax) (AAReg Rax)
                            emit $ ACmp zp (AAReg l)
                            emit $ ASetEq
                            emit $ AMov (AAReg Rax) (AAReg l)
                        (zarg, OpCmpIntNe) -> do
                            emit $ AXor (AAReg Rax) (AAReg Rax)
                            emit $ ACmp zp (AAReg l)
                            emit $ ASetNe
                            emit $ AMov (AAReg Rax) (AAReg l)

            adesc <- lgets addressDesc
            rdesc2 <- lgets registerDesc
            updateAddressDesc (Map.insert x (Set.singleton l, Set.empty) adesc)
            updateRegisterDesc (compose rdesc2 [removeFromAllSets x, Map.insert l (Set.singleton x)])
            mapM_ forgetIfUnused [y, z]
            return ()

        Nothing -> return ()

quadsToAsm wrappedNextUses aliveBlockEnd = do
    mapM_ (uncurry quadToAsm) wrappedNextUses
    saveVarsAfterBlock aliveBlockEnd
    memory <- lgets varLocations
    return ()

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
        (wrapNextUses, aliveStart) =  appendNextUses quads aliveBlockEnd
        pState = QuadProgState initAddressDesc initRegisterDesc initFirstFree initNextUseInfo initVarLocations
        evalResult = runEval pState (quadsToAsm wrapNextUses aliveBlockEnd) in
        case evalResult of
            Right ((a, s), w) -> (varLocations s, firstFree s, w)


loadArgument i = case i of
    0 -> [AMov (AAReg Rdi) (AAMem 0)]
    1 -> [AMov (AAReg Rsi) (AAMem 1)]
    2 -> [AMov (AAReg Rdx) (AAMem 2)]
    3 -> [AMov (AAReg Rcx) (AAMem 3)]
    4 -> [AMov (AAReg R8) (AAMem 4)]
    5 -> [AMov (AAReg R9) (AAMem 5)]
    i -> [AMov (AAMem (5-i)) (AAReg Rax), AMov (AAReg Rax) (AAMem i)]

loadArguments :: Int -> [ASM]
loadArguments args = concatMap loadArgument [0..(args-1)]

--- MAIN function
--- returns assembly and the number of vars to allocate on stack
--- number of vars isn't known before translation, because asm can
--- in some cases save temporary values to the memory (and need some more than local variables)
functionToASM :: QuadFunction -> [ASM]
functionToASM (QuadFunction (Ident name) quads argFreeMem args memory) = result where
    prolog = [APush (AAReg Rbp), AMov (AAReg Rsp) (AAReg Rbp)]
    loadArgs = loadArguments args
    epilog = [ALeave, ARet]

    varsInMemory = Map.keys memory
    aDescVals = zip (repeat Set.empty) (map Set.singleton varsInMemory)
    addressDesc = Map.fromList $ zip varsInMemory aDescVals

    blocks = splitIntoBlocks quads
    numberedBlocks = zip [1..] blocks
    graph = buildCFG blocks
    (inInfo, outInfo) = calcInOut graph blocks


    --addGarbageCollection (i, block) = case Map.lookup i outInfo of
    --    Just alive -> collectGarbageBlock block alive
    --    Nothing -> error "fatal"

    --newQuads = concatMap addGarbageCollection (zip [1..] blocks)
    --newBlocks = splitIntoBlocks newQuads
    --newGraph = buildCFG newBlocks
    --(newInInfo, newOutInfo) = calcInOut newGraph newBlocks


    blocksToAsm ((i, q):t) mem freeLoc = case Map.lookup i outInfo of
        Just alive -> case runQuadsToAsm q alive mem freeLoc of
            (newMem, newFree, asm1) -> case blocksToAsm t newMem newFree of
                (asm2, fm2) -> (asm1 ++ asm2, fm2)
        Nothing -> error "fatal"
    blocksToAsm [] mem freeLoc = ([], freeLoc)

    shortenMem (vname, (position, ty)) = (vname, position)
    shortMem = Map.fromList $ map shortenMem (Map.toList memory)


    (asm, freeMem) = blocksToAsm (zip [1..] blocks) (shortMem) (argFreeMem)
    allocMemory = case freeMem of
        0 -> []
        _ -> [ASub (AAConst (8 * freeMem)) (AAReg Rsp)]
    resultAsm = prolog ++ allocMemory ++ loadArgs ++ asm ++ epilog
    result = resultAsm
