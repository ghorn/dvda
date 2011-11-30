-- ExprGraph.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.ExprGraph( exprsToGNodes
                             , exprOfGNode
                             , GNode(..)
                             ) where

import Data.Graph.Inductive hiding (nodes, edges)
import Data.Maybe
import Data.List(foldl')

import Numeric.Dvda.Expr.Expr
--import Numeric.Dvda.Config(outputNames)

data GNode a = GSource Node (Expr a)
             | GUnary  Node (Expr a) Node
             | GBinary Node (Expr a) (Node, Node) deriving Eq

exprOfGNode :: GNode a -> Expr a
exprOfGNode (GSource _ x) = x
exprOfGNode (GUnary _ x _) = x
exprOfGNode (GBinary _ x _) = x

getIdx :: GNode a -> Node
getIdx (GSource n _) = n
getIdx (GUnary n _ _) = n
getIdx (GBinary n _ _) = n

gmatch :: Eq a => Expr a -> [GNode a] -> Maybe (GNode a)
gmatch expr gnodes = case dropWhile (\x -> exprOfGNode x /= expr) gnodes
                      of [] -> Nothing
                         x:_ -> Just x


exprsToGNodes :: Eq a => [Expr a] -> ([GNode a], [Node])
exprsToGNodes exprs = (gnodesOut, topNodesOut)
  where
    (topNodesOut, _, gnodesOut) = foldl' f ([],0,[]) exprs
    f (topNodes, nextFreeNode, gnodes) expr = (topNodes ++ [topNode], nextFreeNode', gnodes ++ gnodes')
      where
        (topNode, nextFreeNode', gnodes') = exprGobbler gnodes nextFreeNode expr

-- | take all the GNodes already in the graph
-- | take an assignment node and an expression
-- | return the assignment or existing node, the next free node, and any added Gnodes
exprGobbler :: Eq a => [GNode a] -> Node -> Expr a -> (Node, Node, [GNode a])
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


--addOutput :: Eq a => (String, Expr a) -> Gr (GExpr a) (Expr a) -> Gr (GExpr a) (Expr a)
--addOutput (name, expr) graph = graphGobbler newIdx expr newGraph
--  where
--    newIdx = head $ newNodes 1 graph
--    newGraph = insNode (newIdx, GOutput name (dim expr)) graph
--
--
--getGraphOp :: Expr a -> GraphOp a
--getGraphOp src@(Source {}) = GSource (sourceType src) (dim src)
--getGraphOp ew@(Unary {}) = GUnary (unaryType ew) (dim ew)
--getGraphOp binary@(Binary {}) = GBinary (binaryType binary) (dim binary)
--
--
--
--
