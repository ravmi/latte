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
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import System.IO
import LexLatte
import ParLatte
import SkelLatte
import PrintLatte
import AbsLatte
import ErrM

moduleName = "Code"



type Location = Integer
-- describes where are the variables located
type Locations = Map.Map Ident Location
-- describes what memory looks like (only types for now)
type Memory = Map.Map Integer Type


-- in current state we want to keep locations of variables, state of the memory,
-- current free avaliable memory location, location of beginning of the block
-- and expected return type
data ProgState = { memory :: Memory
                 , locations :: Locations
                 , blockStart :: Location
                 , freeLocation :: Location
                 , expectRetType :: Type
                 }

type Eval ev = StateT ProgState (WriterT String (ExceptT String Identity)) ev
runEval mem ev = runIdentity $ runExceptT (runWriterT (evalStateT ev mem))

-- this allows to run block without affecting environument outisde of the block.
runLocal :: Eval a -> Eval a
runLocal p = do
    s <- get
    x <- p
    put s
    return x

-- @TODO
varUnitialized :: Ident -> String
varUnitialized x = "@TODO" ++ Ident
alreadyDeclared :: Ident -> String
alreadyDeclared x = "@TODO" ++ Ident

allocateVar Ident :: Type -> Eval ()
allocateVar varName varType = do
    freeLoc <- gets freeLocation
    locs <- gets locations
    mem <- gets memory




lookupLocation :: Ident -> Eval Location
lookupLocation name = do
    locs <- gets locations
    case Map.lookup name locs of
        Just location -> return location
        _ -> throwError $ varUnitialized name



allocate :: Ident -> Type -> Eval ()
allocate varName varType = do
    locs <- get locations
    varLoc <- lookupLocation varName
    startLoc <- gets blockStart
    case Map.lookup name locs of
        Just location -> case location < startLoc of
            True -> allocateVar varName varType
            False -> throwError $ alreadyDeclared varType



