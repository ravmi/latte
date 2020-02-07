{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module GarbageCollector where
import QuadData
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

import Debug.Trace

import NextUses

import qualified Data.List as List

import qualified Data.Map as Map
import qualified Data.Set as Set

data GProgState = GProgState { _freeInt :: Int
                 , _refCount :: Map.Map Int (Set.Set Int)
                 }
                 deriving (Eq, Ord, Show, Read)
mkLabels [''GProgState]

lgets :: MonadState f m => f :-> a -> m a
lgets f = gets (L.get f)

type Eval ev = StateT GProgState (WriterT [Quad] (ExceptT String Identity)) ev
runEval mem ev = runIdentity $ runExceptT (runWriterT (runStateT ev mem))

update :: GProgState :-> a -> a -> Eval ()
update fun val = do
    state <- get
    put $ set fun val state


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

varInsert key val m = case Map.lookup key m of
    Just s -> let
        newSet = Set.insert val s in
        Map.insert key newSet m
    Nothing -> Map.insert key (Set.singleton val) m

collectQuad quad alive = do
    case quad of
        (Quad4 x (OpCall "_latte_default_concat") (QaList args) QaEmpty) -> do
            fInt <- lgets freeInt
            update freeInt (fInt+1)
            rc <- lgets refCount
            update refCount (Map.insert fInt (Set.singleton x) rc)
        (Quad4 x (OpCall "_latte_default_alloc") (QaList args) QaEmpty) -> do
            fInt <- lgets freeInt
            update freeInt (fInt+1)
            rc <- lgets refCount
            update refCount (Map.insert fInt (Set.singleton x) rc)
        _ -> return ()
    rcount <- lgets refCount
    case quad of
        (Quad4 x OpAssVar (QaVar y) QaEmpty) -> let
            varHolds var = case filter ((elem var) . snd) (Map.toList rcount) of
                [] -> Nothing
                (h:_) -> Just $ fst h
            vY = varHolds y
            vX = varHolds x in do
            case vX of
                Just i ->  update refCount (varDelete i x rcount)
                Nothing -> return ()
            rcount2 <- lgets refCount
            case vY of
                Just i -> update refCount (varInsert i x rcount2)
                Nothing -> return ()
        _ -> return ()

    let
        alterF (Just s) = let
            intsc = Just (Set.intersection s alive) in
            case null intsc of
                True -> Nothing
                False -> intsc
        alterF Nothing = Nothing
        alterAll f m = foldl (flip (Map.alter f)) m (Map.keys m) in do
        rc <- lgets refCount
        update refCount (alterAll alterF rc)


        rc2 <- lgets refCount
        let
            forgottenMemory = (Map.keys rcount) List.\\ (Map.keys rcount)
            varsFromMemory mem = case Map.lookup mem rc2 of
                Just vars -> vars
                Nothing -> error "Garbage Collector crashed"
            toFree = foldl Set.union Set.empty (map varsFromMemory forgottenMemory)
            freeThisVar var = emit $ Quad4 var (OpCall "__latte_default_free") (QaList [QaVar var]) QaEmpty in do
            debug <- trace (show toFree ++ "////////////////////////////////") lgets freeInt
            mapM_ freeThisVar toFree
        emit quad


collectGarbageBlock :: [Quad] -> [Int] -> [Quad]
collectGarbageBlock quads aliveBlockEnd = result where
    (quadsWithNU, _) = appendNextUses quads aliveBlockEnd
    getkeys (a, b) = (a, Set.fromList $ Map.keys b)
    calculation =  (mapM_ (uncurry collectQuad) (map getkeys quadsWithNU))
    result = case runEval (GProgState 0 Map.empty) calculation of
        Right (_, w) -> w
