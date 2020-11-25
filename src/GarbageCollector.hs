module GarbageCollector where
import QuadData
import Control.Category
import Prelude hiding ((.), id)
import qualified Data.List as List
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Debug.Trace

import NextUses

import qualified Data.List as List

import qualified Data.Map as Map
import qualified Data.Set as Set

data GProgState = GProgState { freeInt :: Int
                 , refCount :: Map.Map Int (Set.Set Int)
                 }
                 deriving (Eq, Ord, Show, Read)

lgets f = do
    s <- get
    return $ f s

type Eval ev = StateT GProgState (WriterT [Quad] (ExceptT String Identity)) ev
runEval mem ev = runIdentity $ runExceptT (runWriterT (runStateT ev mem))

updateFreeInt :: Int -> Eval ()
updateFreeInt x = do
    GProgState a b <- get
    put $ GProgState x b

updateRefCount :: (Map.Map Int (Set.Set Int)) -> Eval ()
updateRefCount x = do
    GProgState a b <- get
    put $ GProgState a x

emit :: Quad -> Eval ()
emit q = tell [q]



-----------------------------
-----------------------------
-----------------------------

varDelete key val m = case Map.lookup key m of
    Just s -> let
        newSet = Set.delete val s in
        case null newSet of
            True -> Map.delete key m
            False -> Map.insert key (Set.delete val s) m
    Nothing -> m

varDeleteM key val = do
    rc <- lgets refCount
    updateRefCount $ varDelete key val rc

varInsert key val m = case Map.lookup key m of
    Just s -> let
        newSet = Set.insert val s in
        Map.insert key newSet m
    Nothing -> Map.insert key (Set.singleton val) m

varInsertM key val = do
    rc <- lgets refCount
    updateRefCount $ varInsert key val rc

varHolds :: Int -> Eval (Maybe Int)
varHolds var = do
    rc <- lgets refCount
    case filter ((elem var) . snd) (Map.toList rc) of
        [] -> return $ Nothing
        (h:_) -> return $ Just $ fst h

collectQuad quad alive = do
    rco <- lgets refCount
    case quad of
        (Quad4 x _ (QaConst _) _) -> do
            vH <- varHolds x
            case vH of
                Just i -> varDeleteM i x
                Nothing -> return ()
        _ -> return ()
    case quad of
        (Quad4 x (OpCall "_latte_default_concat") (QaList args) QaEmpty) -> do
            vH <- varHolds x
            case vH of
                Just i -> varDeleteM i x
                Nothing -> return ()
            fInt <- lgets freeInt
            updateFreeInt (fInt+1)
            rc <- lgets refCount
            updateRefCount (Map.insert fInt (Set.singleton x) rc)
        (Quad4 x (OpCall "_latte_default_alloc") (QaList args) QaEmpty) -> do
            vH <- varHolds x
            case vH of
                Just i -> varDeleteM i x
                Nothing -> return ()
            fInt <- lgets freeInt
            updateFreeInt (fInt+1)
            rc <- lgets refCount
            updateRefCount (Map.insert fInt (Set.singleton x) rc)
        _ -> return ()


    case quad of
        (Quad4 x OpAssVar (QaVar vy) _) -> do
             hx <- varHolds x
             hy <- varHolds vy
             case hx of
                 Just i -> varDeleteM  i vy
                 Nothing -> return ()
             case hy of
                 Just i -> varInsertM  i x
                 Nothing -> return ()
        _ -> return ()

    rcount <- lgets refCount

    case (Map.keys rco) List.\\ (Map.keys rcount) of
        [] -> return ()
        (h:_) -> case Map.lookup h rco  of
            Just vName ->  emit $ Quad4 (-1) (OpCall "_latte_default_free") (QaList [QaVar h]) QaEmpty
            Nothing -> error "Garbage Collector crashed"

    rcount2 <- lgets refCount
    emit quad

    let
        alterF (Just s) = let
            intsc = Set.intersection s alive in
            case Set.null intsc of
                True -> Nothing
                False -> Just intsc
        alterF Nothing = Nothing
        alterAll f m = foldl (flip (Map.alter f)) m (Map.keys m) in do
        rc <- lgets refCount
        updateRefCount (alterAll alterF rc)


        rc3 <- lgets refCount
        let
            forgottenMemory = (Map.keys rcount2) List.\\ (Map.keys rc3)
            varsFromMemory mem = case Map.lookup mem rcount2 of
                Just vars -> vars
                Nothing -> error "Garbage Collector crashed"
            toFree = foldl Set.union Set.empty (map varsFromMemory forgottenMemory)
            freeThisVar var = emit $ Quad4 (-1) (OpCall "_latte_default_free") (QaList [QaVar var]) QaEmpty in do
            mapM_ freeThisVar toFree


collectGarbageBlock :: [Quad] -> [Int] -> [Quad]
collectGarbageBlock quads aliveBlockEnd = result where
    (quadsWithNU, _) = appendNextUses quads aliveBlockEnd
    getkeys (a, b) = (a, Set.fromList $ Map.keys b)
    calculation =  mapM_ (uncurry collectQuad) (map getkeys quadsWithNU)
    result = case runEval (GProgState 0 Map.empty) calculation of
        Right (_, w) -> w
