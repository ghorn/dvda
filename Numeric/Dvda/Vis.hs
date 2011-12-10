{- |
   Module      : Numeric.Dvda.Vis
   Description : Make pretty GraphViz plots of expressions

   Turn one or more expressions into a pretty GraphViz plot. The graph displayed is exactly representitive of the C code that is generated in `Numeric.Dvda.Function.toFunction` and called in `Numeric.Dvda.Function.callC`.
 -}

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Vis( previewExprs
                       , previewExprs_
                       ) where

import Data.Graph.Inductive hiding (nodes, edges)
import Data.GraphViz
import Data.Text.Lazy(pack)
import Control.Concurrent(threadDelay)
import Data.List(foldl')

import Numeric.Dvda.Symbolic(sym)
import Numeric.Dvda.Internal.Expr
import Numeric.Dvda.Internal.ExprUtils
import Numeric.Dvda.Internal.GNode
import Numeric.Dvda.Internal.ExprGraph

-- | Plot a graph visualizing a list of `Numeric.Dvda.Expr.Expr`s. The edges of the graph will be labeled with the local expression's value.
previewExprs :: (Eq a, Show a) => [Expr a] -> [String] -> IO ()
previewExprs exprs names = do
  preview $ nmap ShowOp $ emap ShowExpr (lexprsToGraph exprs names)
  threadDelay 10000

-- | Plot a graph visualizing a list of `Numeric.Dvda.Expr.Expr`s. The edges of the graph will be unlabeled.
previewExprs_ :: (Eq a, Show a) => [Expr a] -> [String] -> IO ()
previewExprs_ exprs names = do
  preview $ nmap ShowOp $ emap HideExpr (lexprsToGraph exprs names)
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

gNodesToLEdges :: [GNode (Expr a)] -> [(Node, Node, (Node, Node, (Expr a)))]
gNodesToLEdges gnodes = foldl' f [] gnodes
  where
    f lEdges (GSource _ _) = lEdges
    f lEdges (GOutput n _ c _ _) = lEdges++[(c, n, (c, n, exprOfGNode (gnodes !! c)))]
    f lEdges (GBroadcast n _ c) = lEdges++[(c, n, (c, n, exprOfGNode (gnodes !! c)))]
    f lEdges (GUnary n _ c) = lEdges++[(c, n, (c, n, exprOfGNode (gnodes !! c)))]
    f lEdges (GBinary n _ (cx,cy)) = lEdges++[ (cx, n, (cx, n, exprOfGNode (gnodes !! cx)))
                                             , (cy, n, (cy, n, exprOfGNode (gnodes !! cy)))]

gNodesToLNodes :: [GNode (Expr a)] -> [(Node, (Node, Expr a))]
gNodesToLNodes = map f
  where
    f (GSource n expr  ) = (n, (n, expr))
    f (GBroadcast n expr _) = (n, (n, expr))
    f (GOutput n _ _ _ name) = (n, (n, sym name))
    f (GUnary  n expr _) = (n, (n, expr))
    f (GBinary n expr _) = (n, (n, expr))

lexprsToGraph :: Eq a => [Expr a] -> [String] -> Gr (Node, Expr a) (Node, Node, Expr a)
lexprsToGraph exprs names = mkGraph (gNodesToLNodes gNodes) (gNodesToLEdges gNodes)
  where
    gNodes = exprsToGNodes exprs names
