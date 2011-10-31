-- ExprToGraph.hs

{-# OPTIONS_GHC -Wall #-}

module Had.Expr.ExprToGraph( exprToGr
                           ) where

import Had.Expr.Expr
import Had.Expr.ElemwiseType

import Data.Graph.Inductive hiding (pre)

edgeName :: String
edgeName = ""


newNode :: Int -> String -> LNode String
newNode idx name = (idx, name)


newEdge :: Int -> Int -> String -> LEdge String
newEdge idx0 idx1 name = (idx1, idx0, name)


exprToGr :: (Show a, Num a) => Expr a -> Gr String String
exprToGr expr = mkGraph nodes' edges'
  where
    (nodes', edges') = exprToNe expr


exprToNe :: (Show a, Num a) => Expr a -> ([(Int, String)], [(Int, Int, String)])
exprToNe expr = (topNode:nodes', edges')
  where
    topNode = newNode 0 "output"
    (nodes', edges') = f 0 1 expr

    f :: (Num a, Show a) => Int -> Int -> Expr a -> ([LNode String], [LEdge String])
    f parentIdx idx (Op2 op2type x y) = ((nn:nnX)++nnY, (ne:neX)++neY)
      where
        nn = newNode idx (show op2type)
        ne = newEdge parentIdx idx edgeName
        (nnX, neX) = f idx (idx + 1) x
        (nnY, neY) = f idx (idx + (length nnX) + 1) y
    f parentIdx idx (Elemwise elemwiseType x) = ((nn:nnX), (ne:neX))
      where
        nn = newNode idx (pre ++ "  " ++ post)
             where (pre, post) = elemwisePrePost elemwiseType
        ne = newEdge parentIdx idx edgeName
        (nnX, neX) = f idx (idx + 1) x
    f parentIdx idx (Source a) = ([nn], [ne])
      where
        nn = newNode idx (show a)
        ne = newEdge parentIdx idx edgeName
