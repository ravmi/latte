module NextUses where

import QuadData
import qualified Data.Map as Map
import qualified Data.List as List
import Debug.Trace

type LineNumber = Int
type NextUsesMap = Map.Map Var LineNumber


-- Updates map of next uses to its state from before quadruple
nextUsesBeforeQuad :: NextUsesMap -> LineNumber -> Quad -> NextUsesMap
nextUsesBeforeQuad nextUses lineNum q = result where
    insertList (h:t) m = insertList t (Map.insert h lineNum m)
    insertList [] m = m
    deleteList (h:t) m = deleteList t (Map.delete h m)
    deleteList [] m = m
    used = usedInQuad q
    defined = definedInQuad q

    result = insertList used (deleteList defined nextUses)


definedInQuad :: Quad -> [Var]
definedInQuad (Quad4 x _ y z) = [x]
definedInQuad (QuadNoAssign _ y z) = []

definedInBlock :: [Quad] -> [Var]
definedInBlock quads = List.nub $ concatMap definedInQuad quads

yAndZ :: Quad -> (QArgument, QArgument)
yAndZ (Quad4 _ _ y z) = (y, z)
yAndZ (QuadNoAssign _ y z) = (y, z)

usedInQuad q = result where
    (y, z) = yAndZ q
    filterVar (QaVar x) = [x]
    filterVar _ = []
    usedInName name = case name of
        QaVar var -> [var]
        QaList l -> concat (map filterVar l)
        _ -> []
    result = List.nub $ (usedInName y) ++ (usedInName z)

usedInBlock b = usedInBlock2 (reverse b) []

usedInBlock2 [] tillNow = tillNow
usedInBlock2 (q:t) tillNow = usedInBlock2 t newvars where
    used = usedInQuad q
    defined = definedInQuad q
    newvars = List.nub $ (tillNow List.\\ defined) ++ used


--- helper function, takes reveresd list with line numbers and returns for each quadruple information about next uses of
--- its variables
revNextUsesList :: [(LineNumber, Quad)] -> NextUsesMap -> [NextUsesMap]
revNextUsesList [] nextUses = [nextUses]
revNextUsesList ((lineNum, q):t) nextUses = result where
    nextUsesBefore = nextUsesBeforeQuad nextUses lineNum q
    --allNames = List.nub $ (definedInQuad q) ++ (usedInQuad q)
    --filterFun k val = elem k allNames
    --result = (Map.filterWithKey filterFun nextUses):(revNextUsesList t nextUsesBefore)
    result = (nextUses:revNextUsesList t nextUsesBefore)


-- appends next uses to each line and returns variable alive at the beginning of the block
-- IMPORTANT function
appendNextUses :: [Quad] -> [Var] -> ([(Quad, NextUsesMap)], [Var])
appendNextUses quads aliveBlockEnd = result where
    numberedReversed = reverse $ zip [1..] quads
    countQuads = length quads
    nextUsesAtTheEnd = Map.fromList $ zip aliveBlockEnd (repeat (countQuads + 1))
    revNextResult = reverse $ revNextUsesList numberedReversed nextUsesAtTheEnd
    aliveStart = case revNextResult of
        (h:_) -> Map.keys h
        _ -> error "appendNextUses" Map.keys $ head revNextResult
    quadsWithUses = zip quads (tail revNextResult)
    result =  (quadsWithUses, aliveStart)
