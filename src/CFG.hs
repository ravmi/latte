module CFG where
-- Control Flow Graph

import QuadData
import NextUses (usedInBlock, definedInBlock)
import qualified Data.Map as Map
import Data.List as List
import Debug.Trace
type Graph = Map.Map Int [Int]

startsBlock :: Quad -> Bool
startsBlock q = case q of
    QuadNoAssign (OpLabel _) _ _ -> True
    Quad4 _ (OpCall _) _ _ -> True
    Quad4 _ (OpAllocString _ _) _ _ -> True
    QuadNoAssign (OpJmp _) _ _ -> True
    QuadNoAssign (OpGoToIfFalse _) _ _ -> True
    _ -> False

endsBlock :: Quad -> Bool
endsBlock q = case q of
    QuadNoAssign (OpJmp _) _ _ -> True
    QuadNoAssign (OpGoToIfFalse _) _ _ -> True
    _ -> False


splitIntoBlocksHelper [] [] = []
splitIntoBlocksHelper cB [] = [cB]
splitIntoBlocksHelper currentBlock (quad:rest) = case startsBlock quad of
    True -> (currentBlock:(splitIntoBlocksHelper [quad] rest))
    False -> splitIntoBlocksHelper (quad:currentBlock) rest
    --False -> case endsBlock quad of
    --    True -> (quad:currentBlock):(splitIntoBlocksHelper [] rest)

splitIntoBlocks quads = filter (not. null) $ map reverse (splitIntoBlocksHelper [] quads)

lookupMany l m = result where
    lookupOne m x = case Map.lookup x m of
        Just x -> x
        Nothing -> []
    result = concatMap (lookupOne m) l

canGoTo :: (Int, [Quad]) -> (Int, [Quad]) -> Bool
canGoTo (i1, b1) (i2, b2) = case (last b1, b2) of
    (QuadNoAssign (OpGoToIfFalse lab1) _ _, (QuadNoAssign (OpLabel lab2) _ _:_)) -> lab1 == lab2
    (QuadNoAssign (OpJmp lab1) _ _, ((QuadNoAssign (OpLabel lab2) _ _):_)) -> lab1 == lab2
    _ -> (i1+1) == i2

buildCFG :: [[Quad]] -> Map.Map Int [Int]
buildCFG blocks = graph where
    numberedBlocksList = zip [1..] blocks
    numBlock = Map.fromList numberedBlocksList
    getSuccessors b1 = map fst $ filter (canGoTo b1) numberedBlocksList
    graph = Map.fromList $ zip [1..] (map getSuccessors numberedBlocksList)

cfgHelper1 :: Graph -> Graph -> Graph -> Graph -> Graph -> Int -> (Graph, Graph)
cfgHelper1 graph useM defM inM outM size = result where
    calcIn n = case (Map.lookup n useM, Map.lookup n outM, Map.lookup n defM) of
        --(Just nuse, Just nout, Just ndef) -> trace (((show useM) ++ (show defM) ++ (show outM)) ++ (show (n, List.nub (nuse ++ (nout List.\\ ndef))))) (n, List.nub (nuse ++ (nout List.\\ ndef)))
        (Just nuse, Just nout, Just ndef) -> (n, List.nub (nuse ++ (nout List.\\ ndef)))
        _ -> error "buildInout"
    calcOut n = case Map.lookup n graph of
       Just l -> (n, List.nub (lookupMany l inM))
       Nothing -> error "buildInout"
    r1 = map calcIn [1..size]
    r2 = map calcOut [1..size]
    result = (Map.fromList r1, Map.fromList r2)

cfgHelper2 graph useM defM inM outM size = case cfgHelper1 graph useM defM inM outM size of
        (a, b) -> case (a == inM) && (b == outM) of
            True -> (a, b)
            False -> cfgHelper2 graph useM defM a b size

calcInOut graph blocks = result where
    size = length blocks
    useM = Map.fromList $ zip [1..] (map usedInBlock blocks)
    defM = Map.fromList $ zip [1..] (map definedInBlock blocks)
    inM = Map.fromList $ zip [1..size] (repeat [])
    outM = Map.fromList $ zip [1..size] (repeat [])
    result = cfgHelper2 graph useM defM inM outM size

inOutFromBlocks blocks = calcInOut graph blocks where
    graph = buildCFG blocks