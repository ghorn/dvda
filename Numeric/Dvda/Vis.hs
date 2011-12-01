-- Vis.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Vis( previewExprs
                       , previewExprs_
--                       , lexprsToGraph
                       ) where

import Data.Graph.Inductive hiding (nodes, edges)
import Data.GraphViz
import Data.Text.Lazy(pack)
import Control.Concurrent(threadDelay)
import Data.List(foldl')

import Numeric.Dvda.Expr
import Numeric.Dvda.Internal.GNode
import Numeric.Dvda.Internal.ExprGraph

previewExprs :: (Eq a, Show a) => [Expr a] -> IO ()
previewExprs exprs = do
  preview $ nmap ShowOp $ emap ShowExpr (exprsToGraph exprs)
  threadDelay 10000

previewExprs_ :: (Eq a, Show a) => [Expr a] -> IO ()
previewExprs_ exprs = do
  preview $ nmap ShowOp $ emap HideExpr (exprsToGraph exprs)
  threadDelay 10000

data NodeShow a = ShowOp (Node, Expr a)
                | ShowOpAndDim (Node, Expr a)
                | ShowOpAndType (Node, Expr a)
                | ShowOpAndNode (Node, Expr a) deriving Eq
data EdgeShow a = ShowExpr (Node, Node, Expr a)
                | HideExpr (Node, Node, Expr a) deriving Eq

nodeShowToNode :: NodeShow a -> Node
nodeShowToNode (ShowOp (n, _)) = n
nodeShowToNode (ShowOpAndDim (n, _)) = n
nodeShowToNode (ShowOpAndType (n, _)) = n
nodeShowToNode (ShowOpAndNode (n, _)) = n

edgeShowToNode :: EdgeShow a -> Node
edgeShowToNode (ShowExpr (n, _, _)) = n
edgeShowToNode (HideExpr (n, _, _)) = n

instance Show a => Labellable (NodeShow a) where
  toLabelValue (ShowOp (_, expr)) = toLabelValue $ pack $ showNode expr
  toLabelValue (ShowOpAndDim (_, expr)) = toLabelValue $ pack $ showNode expr ++ " <-" ++ showDim expr
  toLabelValue (ShowOpAndType (_, expr)) = toLabelValue $ pack $ "{" ++ showType expr ++ "}: " ++ showNode expr
  toLabelValue (ShowOpAndNode (n, expr)) = toLabelValue $ pack $ "(node " ++show n ++ ") " ++ showNode expr

instance Show a => Labellable (EdgeShow a) where
  toLabelValue (ShowExpr (_, _, expr)) = toLabelValue $ pack $ show expr
  toLabelValue (HideExpr (_, _, _)) = toLabelValue $ pack ""

instance (Eq a, Show a) => Ord (NodeShow a) where
  compare n1 n2 = compare (nodeShowToNode n1) (nodeShowToNode n2)
instance (Eq a, Show a) => Ord (EdgeShow a) where
  compare e1 e2 = compare (edgeShowToNode e1) (edgeShowToNode e2)

gNodesToLEdges :: [GNode a] -> [(Node, Node, (Node, Node, a))]
gNodesToLEdges gnodes = foldl' f [] gnodes
  where
    f lEdges (GSource _ _) = lEdges
    f lEdges (GUnary n _ c) = lEdges++[(c, n, (c, n, exprOfGNode (gnodes !! c)))]
    f lEdges (GBinary n _ (cx,cy)) = lEdges++[ (cx, n, (cx, n, exprOfGNode (gnodes !! cx)))
                                             , (cy, n, (cy, n, exprOfGNode (gnodes !! cy)))]

gNodesToLNodes :: [GNode a] -> [(Node, (Node, a))]
gNodesToLNodes = map f
  where
    f (GSource n expr  ) = (n, (n, expr))
    f (GUnary  n expr _) = (n, (n, expr))
    f (GBinary n expr _) = (n, (n, expr))

exprsToGraph :: Eq a => [Expr a] -> Gr (Node, Expr a) (Node, Node, Expr a)
exprsToGraph exprs = mkGraph (gNodesToLNodes gNodes) (gNodesToLEdges gNodes)
  where
    gNodes = fst $ exprsToGNodes exprs
