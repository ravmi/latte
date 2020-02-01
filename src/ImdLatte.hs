{-# LANGUAGE TemplateHaskell, TypeOperators #-}




module ImdLatte where

import AbsLatte
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Category
import Data.Label hiding (get)
import Prelude hiding ((.), id)
import qualified Data.Label as L

import System.Exit ( exitFailure, exitSuccess )
import System.IO



import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe
import qualified Data.List as List

import Test.QuickCheck
import Test.QuickCheck.Monadic

-- Haskell module generated by the BNF converter

data QArgument = Mem Int | RegInt Integer | RegBool Bool | Reg Int
--Reg1 | Reg2 | Reg3 | Reg4 | Reg5
  deriving (Eq, Ord, Show, Read)


data QBlock = QBlock Ident [Quadruple] Int Int
  deriving (Eq, Ord, Show, Read)


type QCode = [Quadruple]

-- niech drugi argument to bedzie zawsze register (nie pamiec)

data Quadruple
    = QAdd QArgument QArgument
    | QSub QArgument QArgument
    | QMul QArgument QArgument
    | QDiv QArgument QArgument
    | QMod QArgument QArgument
    | QAss QArgument QArgument
    | QInc QArgument
    | QDec QArgument
    | QRet QArgument
    | QRetV
    | QLab Ident
    | QJmp Ident

    | QGoToIfEqual Ident QArgument QArgument
    | QGoToIfNotEqual Ident QArgument QArgument
    | QGoToIfGreater Ident QArgument QArgument
    | QGoToIfGreaterEqual Ident QArgument QArgument
    | QGoToIfLesser Ident QArgument QArgument
    | QGoToIfLesserEqual Ident QArgument QArgument


    | QNeg QArgument
    | QAnd QArgument QArgument
    | QOr QArgument QArgument
    | QXor QArgument QArgument
    | QNot QArgument
    | QSwap QArgument QArgument

    | QCmpIntLt QArgument QArgument
    | QCmpIntLe QArgument QArgument
    | QCmpIntGt QArgument QArgument
    | QCmpIntGe QArgument QArgument
    | QCmpIntEq QArgument QArgument
    | QCmpIntNe QArgument QArgument


    | QAlloc QArgument Int
    | QConcat QArgument QArgument

    | QCall Ident [QArgument]
    | QLoad QArgument QArgument
    | QStore QArgument QArgument
    deriving (Eq, Ord, Show, Read)


-- najpierw zaloz, ze nie uzywasz rejestrow ponownie, wszystko zapisuj w zmiennych lokalnych (tu bedzie problem)
-- rozbij na bloki, lista w postaci blok, zmienne lokalne

type BasicBlock = [Quadruple]

data Quad = Quad Op Int Int Int
    deriving (Eq, Ord, Show, Read)
data Op = OpAdd | Mul | OpStore | OpMov
    deriving (Eq, Ord, Show, Read)

data Reg = Rax | Rbx | Rcx | Rdx
    deriving (Eq, Ord, Show, Read)


data Location = LocReg Reg | LocVar Int
    deriving (Eq, Ord, Show, Read)


allRegisters = [Rax, Rbx, Rcx, Rdx]

-- preprocessing function, calculates next uses of each quadruple argument
nextUses :: [Quad] -> [Int] -> [(Quad, (Map.Map Int Int))]
nextUses quads aliveBlockEnd = reverse $ nextUsesRev nextUsesMap revQuads where
    revQuads = reverse $ zip [1..] quads
    quadsLen = length quads
    nextUsesMap = Map.fromList (zip aliveBlockEnd (repeat $ quadsLen+1))

nextUsesRev nextUsesMap [] = []
nextUsesRev nextUsesMap ((i, (Quad op x y z)):t1) = (((Quad op x y z), nextUses):t2) where
    filterFun k v = elem k [x, y, z]
    nextUses = Map.filterWithKey filterFun nextUsesMap
    m1 = (Map.delete x nextUsesMap)
    m2 = Map.insert y i m1
    newMap = Map.insert z i m2
    t2 = nextUsesRev newMap t1

type LineNumber = Int
type Var = Int
type NextUsesMap = Map.Map Var LineNumber


compose x [] = x
compose x (y:ys) = compose (y x) ys


---------------------------------------------------
----- functions calculating next uses of quadruples
-------------------------------------------------------

-- Updates map of next uses to its state from before quadruple
nextUsesBeforeQuad :: NextUsesMap -> LineNumber -> Quad -> NextUsesMap
nextUsesBeforeQuad nextUses lineNum (Quad _ x y z) = result where
    operations = [Map.delete x, Map.insert y lineNum, Map.insert z lineNum]
    result = compose nextUses operations

--- helper function, takes reveresd list with line numbers and returns for each quadruple information about next uses of
--- its variables
revNextUsesList :: [(LineNumber, Quad)] -> NextUsesMap -> [NextUsesMap]
revNextUsesList [] nextUses = [nextUses]
revNextUsesList ((lineNum, q@(Quad _ x y z)):t) nextUses = result where
    nextUsesBefore = nextUsesBeforeQuad nextUses lineNum q
    filterXYZ k val = elem k [x, y, z]
    result = (Map.filterWithKey filterXYZ nextUses):(revNextUsesList t nextUsesBefore)

-- appends next uses to each line and returns variable alive at the beginning of the block
appendNextUses :: [Quad] -> [Var] -> ([(Quad, NextUsesMap)], [Var])
appendNextUses quads aliveBlockEnd = result where
    numberedReversed = reverse $ zip [1..] quads
    countQuads = length quads
    nextUsesAtTheEnd = Map.fromList $ zip aliveBlockEnd (repeat (countQuads + 1))
    revNextResult = reverse $ revNextUsesList numberedReversed nextUsesAtTheEnd
    aliveStart = Map.keys $ head revNextResult
    quadsWithUses = zip quads (tail revNextResult)
    result = (quadsWithUses, aliveStart)
-----------------------------
--------------------------------------------------
--------------------------------------------------



type NextUse = Int
type MemoryLocation = Int

data ProgState = ProgState { _addressDesc :: Map.Map Int (Set.Set Location)
                 , _registerDesc :: Map.Map Reg (Set.Set Var)
                 , _firstFree :: Int
                 , _nextUseInfo :: Map.Map Var NextUse
                 , _varLocations :: Map.Map Var MemoryLocation
                 }
                 deriving (Eq, Ord, Show, Read)
mkLabels [''ProgState]

lgets :: MonadState f m => f :-> a -> m a
lgets f = gets (L.get f)

type Eval ev = StateT ProgState (WriterT [ASM] (ExceptT String Identity)) ev
runEval mem ev = runIdentity $ runExceptT (runWriterT (runStateT ev mem))




update :: ProgState :-> a -> a -> Eval ()
update fun val = do
    state <- get
    put $ set fun val state


isRegister :: Location -> Bool
isRegister (LocReg _) = True
isRegister (LocVar _) = False






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
        return $ fst (head allPairsWithFurthestLine)




-----------------------------------
whereInMemory :: Var -> Eval MemoryLocation
whereInMemory var = do
    varLocs <- lgets varLocations
    case Map.lookup var varLocs of
        Just loc -> return loc
        Nothing -> do
            fl <- lgets firstFree
            update firstFree (fl+1)
            return fl


varDirty :: Var -> Eval Bool
varDirty var = do
    adesc <- lgets addressDesc
    case Map.lookup var adesc of
        Just s -> return $ not (Set.member (LocVar var) s)
        Nothing -> error "wrong var"


--helper, assumes that var is in reg. It makes sure that var is in it's position in memory. reg will hold everything that
-- was in ver's memory positon
saveVarToMemory :: Reg -> Var -> Eval ()
saveVarToMemory reg var = do
    adesc <- lgets addressDesc
    rdesc <- lgets registerDesc
    whereMem <- whereInMemory var
    emit $ PushMem (whereMem)
    emit $ Store reg (whereMem)
    emit $ PopReg reg
    update addressDesc (replaceInAllSets (LocVar var) (LocReg reg) adesc)
    let
        filterFun k v = (Set.member (LocVar var) v)
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













---------------------------------------------------
---------------------------------------------------------------
------------------find register that holdsy only var if there is such-----------------
findExclusiveRegister :: Var -> Eval (Maybe Reg)
findExclusiveRegister var = do
    rdesc <- lgets registerDesc
    adesc <- lgets addressDesc
    let
        filterFun key val = (elem key (Map.keys rdesc)) && (val == (Set.singleton var))
        registersHoldingOnlyVar = Map.keys $ Map.filterWithKey filterFun rdesc in
        case registersHoldingOnlyVar of
            (reg:_) -> return $ Just reg
            [] -> return Nothing

--- this remove vals from all set values (and also the key if the set turns out empty)
removeFromAllSets val m = result where
    deleteElement (Just s) = case Set.null $ Set.delete val s of
        True -> Nothing
        False -> Just $ Set.delete val s
    deleteElement Nothing = Nothing
    result = alterAll deleteElement m

replaceInAllSets v1 v2 m = result where
    replaceElement (Just s) = case elem v1 s of
        True -> Just $ compose s [Set.delete v1, Set.insert v2]
        False -> Just s
    replaceElement Nothing = Nothing
    result = alterAll replaceElement m

alterAll f m = result where
    keys = Map.keys m
    result = foldl (flip (Map.alter f)) m keys



-- If y is in register that holds only one variable and y has no next uses, give the register holding y
--- if failed, find next free register
-- if failed spill
giveReg y = do
    nextUses <- lgets nextUseInfo
    adesc <- lgets addressDesc
    rdesc <- lgets registerDesc

    exclusiveReg <- findExclusiveRegister y
    case (exclusiveReg, Map.lookup y nextUses) of
        (Just reg, Nothing) -> do
            update addressDesc (removeFromAllSets (LocReg reg) adesc) -- register still holds value, not sure if it's ok
            return reg
        _ -> case allRegisters List.\\ (Map.keys rdesc) of -- check if there are any free registers
            (reg:_) -> return reg
            [] -> do
                reg <- furthestBusyRegister --TODO perhaps fix it, beacause the implemention is old
                spillReg reg
                return reg


emit :: ASM -> Eval ()
emit asm = tell [asm]

whereVarPreferReg :: Var -> Eval Location
whereVarPreferReg var = do
    rdesc <- lgets registerDesc
    adesc <- lgets addressDesc
    case Map.lookup var adesc of
        Just s -> let
            regs = Set.toList $ Set.filter isRegister s
            noregs = Set.toList $ Set.filter (not . isRegister) s
            result = head $ regs ++ noregs in
            return result
        Nothing -> error "var is nowhere"


updateNextUses :: (Map.Map Var NextUse) -> Eval ()
updateNextUses newInfo = do
    oldInfo <- lgets nextUseInfo
    update nextUseInfo (Map.union newInfo oldInfo)


updateVarAfterQuad :: Var -> Eval ()
updateVarAfterQuad var = do
    rdesc <- lgets registerDesc
    adesc <- lgets addressDesc
    nextUses <- lgets nextUseInfo

    case Map.lookup var nextUses of
        Just line -> return ()
        Nothing -> do
            update addressDesc (Map.delete var adesc)
            update registerDesc (removeFromAllSets var rdesc)

quadToAsm q@(Quad op x y z) nextUses = do
    rdesc <- lgets registerDesc
    adesc <- lgets addressDesc
    updateNextUses nextUses
    case Map.lookup x nextUses of
        Just xline -> do
            l <- giveReg y
            case (Just $ Set.singleton y) /= (Map.lookup l rdesc) of
                True -> emit $ Load y l
                False -> return ()
            zp <- whereVarPreferReg z
            case (zp, op) of
                (LocReg zreg, OpAdd) -> emit $ AddRR zreg l
                (LocVar zvar, OpAdd) -> emit $ AddVR zvar l
            update addressDesc (Map.insert x (Set.singleton (LocReg l)) adesc)
            update registerDesc (compose rdesc [removeFromAllSets x, Map.insert l (Set.singleton x)])
            updateVarAfterQuad y
            updateVarAfterQuad z
        Nothing -> return ()


-- We assume that all t
-- TODO 1 musi zwrocic liczbe spilled, zeby zaalokowac i zwolnic dodatkowa pamiec
-- Nie bedziemy zwalniac, te dodatkowo zaalokowane wartosci moga rosnac jak jest duzo blokow
-- (no ale nie bedzie duzo blokow ofc)
quadsToAsm wrappedNextUses = do
    mapM_ (uncurry quadToAsm) wrappedNextUses


-- all living variables mu be in the memory at the end

--Map.filterWithKey () where
--    adesc <- lgets addressDesc
--    filterDirty k v = (elem k aliveVars) && (not $ k elem v)
--    varsToSave = Map.filterWithKey filterDirty adesc



-- trzebaj eszcze zapisac do pamieci ofc
--required: aliveBlockEnd, args, locals, tmps, quads
--- firstFree is in memory
runQuadsToAsm quads firstFree aliveBlockEnd =
    let
        initAddressDesc = Map.fromList $ zip [1..firstFree-1] (map (Set.singleton . LocVar) [1..firstFree-1])
        initRegisterDesc = Map.empty
        initFirstFree = firstFree
        initNextUseInfo = Map.empty
        initVarLocations = Map.fromList $ zip [1..firstFree-1] [1..firstFree-1]
        wrapNextUses = nextUses quads aliveBlockEnd
        pState = ProgState initAddressDesc initRegisterDesc initFirstFree initNextUseInfo initVarLocations
        evalResult = runEval pState (quadsToAsm wrapNextUses) in
        case evalResult of
            Right ((a, s), w) -> return $ w
            Left errMessage -> do hPutStrLn stderr errMessage
                                  exitFailure

--test2 = nextUses [Quad Add 5 6 7, Quad Mul 1 2 3, Quad Mul 11 1 13] [5, 1, 2]

quad1 = Quad OpAdd 1 2 3
quad2 = Quad OpAdd 4 5 6
quad3 = Quad OpAdd 7 8 9
quad4 = Quad OpAdd 10 11 12
quad5 = Quad OpAdd 13 14 15
quads :: [Quad]
quads = [quad1, quad2, quad3, quad4, quad5]
--prepareQuads quads = reverse (zip [1..] quads)
--alQuad :: Map.Map Var LineNumber
--alQuad = Map.fromList [(1, 100), (5, 100)]

data ASM = PushMem MemoryLocation | PushReg Reg | Store Reg MemoryLocation | PopReg Reg | Load MemoryLocation Reg | AddRR Reg Reg | AddVR Var Reg
                 deriving (Eq, Ord, Show, Read)

ret = runQuadsToAsm quads 200 [1, 4, 7, 10, 13]


--- TODO --------------pozapisuj zmienne, potem duzo testow
----------------------
------testing functions
------------jhhhhhhh--




-----------------------
-----Test giveReg----
------------------
---------TESTING HERE
testGiveReg1 = let
        --- here starting conditions
        ad = Map.fromList $ zip [1..5] (map (Set.singleton . LocVar) [1..5])
        rd = Map.empty
        ff = 20
        nui = Map.empty
        vl = Map.fromList $ zip [1..5] [1..5]
        pState = ProgState ad rd ff nui vl
        evalResult = runEval pState (giveReg 4) in
        case evalResult of
            Right ((a, s), w) -> do
                runTest (ff == 21) "testGiveReg1"
                runTest (Map.null nui) "sdf"
            Left errMessage -> do hPutStrLn stderr "Testing failed badly"
                                  hPutStrLn stderr errMessage
                                  exitFailure


runTest :: Bool -> String -> IO ()
runTest ok msg = do
    case ok of
        True -> putStrLn "OK"
        False -> do hPutStrLn stderr "tests failed"
                    hPutStrLn stderr msg
                    exitFailure
------------------------------------
-----------------------------------
-------------------------------





















