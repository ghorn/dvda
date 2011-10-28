-- Expr.hs

{-# OPTIONS_GHC -Wall #-}

--module Expr( Expr(..)
--           , exprToGr
--           ) where


import Data.GraphViz
import Data.Graph.Inductive

data (Show a, Num a) => Expr a = Number a
                               | I Integer
                               | Sym String
                               | Mul (Expr a) (Expr a)
                               | Div (Expr a) (Expr a)
                               | Add (Expr a) (Expr a)
                               | Sub (Expr a) (Expr a) deriving (Eq)
instance (Num a, Show a) => Show (Expr a) where
  show (Number a) = show a
  show (I a) = show a
  show (Sym a) = a
  show (Mul a b) = "("++show a++" * "++show b++")"
  show (Div a b) = "("++show a++" / "++show b++")"
  show (Add a b) = "("++show a++" + "++show b++")"
  show (Sub a b) = "("++show a++" - "++show b++")"


instance (Show a, Eq a, Num a) => Num (Expr a) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = error "suck it"
  signum = error "suck it"
  fromInteger = I

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
    f parentIdx idx (Mul x y) = ((nn:nnX)++nnY, (ne:neX)++neY)
      where
        nn = newNode idx "*"
        ne = newEdge parentIdx idx edgeName
        (nnX, neX) = f idx (idx + 1) x
        (nnY, neY) = f idx (idx + (length nnX) + 1) y
    f parentIdx idx (Div x y) = ((nn:nnX)++nnY, (ne:neX)++neY)
      where
        nn = newNode idx "/"
        ne = newEdge parentIdx idx edgeName
        (nnX, neX) = f idx (idx + 1) x
        (nnY, neY) = f idx (idx + (length nnX) + 1) y
    f parentIdx idx (Add x y) = ((nn:nnX)++nnY, (ne:neX)++neY)
      where
        nn = newNode idx "+"
        ne = newEdge parentIdx idx edgeName
        (nnX, neX) = f idx (idx + 1) x
        (nnY, neY) = f idx (idx + (length nnX) + 1) y
    f parentIdx idx (Sub x y) = ((nn:nnX)++nnY, (ne:neX)++neY)
      where
        nn = newNode idx "-"
        ne = newEdge parentIdx idx edgeName
        (nnX, neX) = f idx (idx + 1) x
        (nnY, neY) = f idx (idx + (length nnX) + 1) y
    f parentIdx idx a = ([nn], [ne])
      where
        nn = newNode idx (show a)
        ne = newEdge parentIdx idx edgeName


exampleExpr :: Expr Integer
exampleExpr = x*34 + 5
  where
    x = Sym "x"

main :: IO ()
main = do
--  preview aGr
  print exampleExpr
  preview $ exprToGr exampleExpr
