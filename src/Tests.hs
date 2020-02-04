module Tests where

import ImdLatte
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

import QuadData







-----------------------
-----Test giveReg----
------------------
---------TESTING HERE
testGiveReg1 = let
        --- here starting conditions
        prepAdesc x = (Set.empty, Set.singleton x)
        ad = Map.fromList $ zip [1..5] (map prepAdesc [1..5])
        rd = Map.empty
        ff = 20
        nui = Map.empty
        vl = Map.fromList $ zip [1..5] [1..5]
        pState = QuadProgState ad rd ff nui vl
        evalResult = runEval pState (giveReg (QaVar 4)) in
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






v1 = QaVar 1
v2 = QaVar 2
v3 = QaVar 3
v4 = QaVar 4
v5 = QaVar 5
v6 = QaVar 6

ve = QaEmpty
v11 = QaList []
v12 = QaConst 5

qa1 = Quad4 5 OpAdd v2 v4
qa2 = QuadNoAssign (OpLabel ("L1")) ve ve
qa3 = Quad4 1 OpAdd v2 v3
qa4 = Quad4 2 OpAdd v3 v4
qa5 = QuadNoAssign (OpGoToIfFalse "L1") ve ve
qa6 = Quad4 4 OpAdd v5 v6
qa7 = Quad4 12 (OpCall "hehe") (QaList []) ve
qa8 = QuadNoAssign (OpJmp "L1") ve ve
qa9 = Quad4 6 OpAdd v5 v4

quadsTest = [qa1, qa2, qa3, qa4, qa5, qa6, qa7, qa8, qa9]
quadsBlocks = splitIntoBlocks quadsTest
cfgGraph = buildCFG quadsBlocks
ci = calcInOut cfgGraph quadsBlocks
--[(1,[2,4,3,6]),(2,[2,3,4,5,6]),(3,[5,6,2,3]),(4,[2,3,4,5,6]),(5,[5,4])],fromList [(1,[2,3,4,5,6]),(2,[2,3,4,5,6]),(3,[2,3,4,5,6]),(4,[2,3,4,5,6]),(5,[])]

testMap1 = Map.fromList [(1, "afh"), (2, "eee"), (5, "rrr")]
--testLookupMany = (lookupMany [1, 5] testMap1) == "afhrrr"



quad1 = Quad4 1 OpAdd (QaVar 2) (QaVar 3)
quad2 = Quad4 4 OpAdd (QaVar 5) (QaVar 6)
quad3 = Quad4 7 OpAdd (QaVar 8) (QaVar 9)
quad4 = Quad4 10 OpAdd (QaVar 11) (QaVar 12)
quad5 = Quad4 13 OpAdd (QaVar 14) (QaVar 15)
ff = 50
quads = [quad1, quad2, quad3, quad4, quad5]
prepareQuads quads = reverse (zip [1..] quads)
--alQuad :: Map.Map Var LineNumber
--alQuad = Map.fromList [(1, 100), (5, 100)]


--ret = runQuadsToAsm quads 200 [1, 4, 7, 10, 13]
--------------