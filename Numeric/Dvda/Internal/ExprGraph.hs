-- ExprGraph.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Internal.ExprGraph( exprsToGNodes
                                      , exprsToGNodes_
                                      ) where

import Data.Graph.Inductive hiding (nodes, edges)
import Data.Maybe
import Data.List(foldl')

import Numeric.Dvda.Internal.Expr
import Numeric.Dvda.Internal.ExprUtils
import Numeric.Dvda.Internal.GNode

exprsToGNodes_ :: Eq a => [Expr a] -> [GNode (Expr a)]
exprsToGNodes_ exprs = exprsToGNodes exprs $ map (\k -> "out"++show k) [(0::Integer)..]

exprsToGNodes :: Eq a => [Expr a] -> [String] -> [GNode (Expr a)]
exprsToGNodes exprs names = gnodesOut
  where
    (_, gnodesOut, _) = foldl' f (0,[],0) (zip exprs names)
    f (nextFreeNode0, gnodes0, outputIdx) (expr,name) =
      (nextFreeNode + 1, gnodes0 ++ gnodes' ++ [outGNode], outputIdx + 1)
      where
        (topNode, nextFreeNode, gnodes') = exprGobbler gnodes0 nextFreeNode0 expr
        outGNode = GOutput nextFreeNode expr topNode outputIdx name

-- | take all the GNodes already in the graph
-- | take an assignment node and an expression
-- | return the assignment or existing node, the next free node, and any added Gnodes
exprGobbler :: Eq a => [GNode (Expr a)] -> Node -> Expr a -> (Node, Node, [GNode (Expr a)])
exprGobbler oldGNodes thisIdx expr
  -- node already exists
  | isJust existingGNode = (existingNode, thisIdx, [])
  -- insert new node
  | otherwise = case getChildren expr
                of CSource -> (thisIdx, thisIdx + 1, [GSource thisIdx expr])
                   CUnary child -> (thisIdx, nextFreeIdx, newGNode:childGNodes)
                     where
                       newGNode = GUnary thisIdx expr childIdx
                       oldGNodes' = oldGNodes ++ [GSource thisIdx expr]
                       (childIdx, nextFreeIdx, childGNodes) = exprGobbler oldGNodes' (thisIdx + 1) child
                   CBroadcast child -> (thisIdx, nextFreeIdx, newGNode:childGNodes)
                     where
                       newGNode = GBroadcast thisIdx expr childIdx
                       oldGNodes' = oldGNodes ++ [GSource thisIdx expr]
                       (childIdx, nextFreeIdx, childGNodes) = exprGobbler oldGNodes' (thisIdx + 1) child
                   CBinary childX childY -> (thisIdx, nextFreeIdx', newGNode:(childXGNodes++childYGNodes))
                     where
                       newGNode = GBinary thisIdx expr (childXIdx, childYIdx)
                       oldGNodes' = oldGNodes ++ [GSource thisIdx expr]
                       (childXIdx, nextFreeIdx, childXGNodes) = exprGobbler oldGNodes' (thisIdx + 1) childX
                       oldGNodes'' = oldGNodes' ++ childXGNodes
                       (childYIdx, nextFreeIdx', childYGNodes) = exprGobbler oldGNodes'' nextFreeIdx childY
  where
    existingGNode = gmatch expr oldGNodes
    -- existingNode = fmap getIdx $ gmatch expr oldGNodes
    existingNode = getIdx $ fromJust existingGNode
