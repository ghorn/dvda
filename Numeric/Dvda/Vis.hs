-- Vis.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Vis( previewExpr
--                       , exprsToGraph
--                       , lexprToGraph
--                       , lexprsToGraph
--                       , GraphOp(..)
                             ) where

import Data.Graph.Inductive hiding (nodes, edges)
import Data.GraphViz
import Data.Text.Lazy(pack)
import Control.Concurrent(threadDelay)
import Data.List(foldl')

import Numeric.Dvda.ExprGraph
import Numeric.Dvda.Expr

previewExpr :: (Eq a, Show a) => Expr a -> IO ()
previewExpr expr = do
  preview $ nmap ShowOp $ emap ShowExpr (exprToGraph expr)
  threadDelay 10000

data NodeShow a = ShowOp (Node, Expr a)
                | ShowOpAndDim (Node, Expr a)
                | ShowOpAndType (Node, Expr a)
                | ShowOpAndNode (Node, Expr a) deriving Eq
data EdgeShow a = ShowExpr (Node, Node, Expr a) deriving Eq

instance Show a => Labellable (NodeShow a) where
  toLabelValue (ShowOp (_, expr)) = toLabelValue $ pack $ showNode expr
  toLabelValue (ShowOpAndDim (_, expr)) = toLabelValue $ pack $ showNode expr ++ " <-" ++ showDim expr
  toLabelValue (ShowOpAndType (_, expr)) = toLabelValue $ pack $ "{" ++ showType expr ++ "}: " ++ showNode expr
  toLabelValue (ShowOpAndNode (n, expr)) = toLabelValue $ pack $ "(node " ++show n ++ ") " ++ showNode expr

instance Show a => Labellable (EdgeShow a) where
  toLabelValue (ShowExpr (_, _, expr)) = toLabelValue $ pack $ show expr

instance (Eq a, Show a) => Ord (NodeShow a) where
  compare = error "why was compare supposed to be defined, anyways?"
instance (Eq a, Show a) => Ord (EdgeShow a) where
  compare = error "why was compare supposed to be defined, anyways?"


gNodesToLEdges :: [GNode a] -> [(Node, Node, (Node, Node, Expr a))]
gNodesToLEdges gnodes = foldl' f [] gnodes
  where
    f lEdges (GSource _ _) = lEdges
    f lEdges (GUnary n _ c) = lEdges++[(c, n, (c, n, exprOfGNode (gnodes !! c)))]
    f lEdges (GBinary n _ (cx,cy)) = lEdges++[ (cx, n, (cx, n, exprOfGNode (gnodes !! cx)))
                                             , (cy, n, (cy, n, exprOfGNode (gnodes !! cy)))]
    

gNodesToLNodes :: [GNode a] -> [(Node, (Node, Expr a))]
gNodesToLNodes = map f
  where
    f (GSource n expr  ) = (n, (n, expr))
    f (GUnary  n expr _) = (n, (n, expr))
    f (GBinary n expr _) = (n, (n, expr))

exprToGraph :: Eq a => Expr a -> Gr (Node, (Expr a)) (Node, Node, (Expr a))
exprToGraph expr = mkGraph (gNodesToLNodes gNodes) (gNodesToLEdges gNodes)
  where
    gNodes = exprToGNodes expr

--getGraphOp :: Expr a -> GraphOp a
--getGraphOp src@(Source {}) = GSource (sourceType src) (dim src)
--getGraphOp ew@(Unary {}) = GUnary (unaryType ew) (dim ew)
--getGraphOp binary@(Binary {}) = GBinary (binaryType binary) (dim binary)
