-- Expr.hs

{-# OPTIONS_GHC -Wall #-}

module Expr( Expr(..)
           , exprToGr
           , sym
           ) where


import Data.GraphViz
import Data.Graph.Inductive hiding (pre)


data Op2Type = Mul
             | Div
             | Add
             | Sub deriving Eq
instance Show Op2Type where
  show Mul = "*"
  show Div = "/"
  show Add = "+"
  show Sub = "-"

data ElemwiseType = Abs
                  | Sqr deriving (Show, Eq)
elemwisePrePost :: ElemwiseType -> (String, String)
elemwisePrePost Abs = ("|", "|")
elemwisePrePost Sqr = ("( ", "^2 )")

data (Show a, Eq a) => SourceType a = Number a
                                    | I Integer
                                    | Sym String deriving Eq
instance (Show a, Eq a) => Show (SourceType a) where
  show (Number a) = show a
  show (I i) = show i
  show (Sym s) = s

data (Show a, Eq a) => Expr a = Source (SourceType a)
                              | Elemwise ElemwiseType (Expr a)
                              | Op2 Op2Type (Expr a) (Expr a) deriving Eq
instance (Show a, Eq a) => Show (Expr a) where
  show (Op2 op2Type x y) = "( " ++ show x ++" "++ show op2Type ++" "++ show y ++ " )"
  show (Source sourcetype) = show sourcetype
  show (Elemwise elemwiseType x) = pre ++ show x ++ post
    where
      (pre, post) = elemwisePrePost elemwiseType

sym :: (Show a, Eq a) => String -> Expr a
sym name = Source (Sym name)

instance (Show a, Eq a, Num a) => Num (Expr a) where
  (+) = Op2 Add
  (-) = Op2 Sub
  (*) = Op2 Mul
  abs = Elemwise Abs
--  signum = error "suck it"
  fromInteger x = Source (I x)


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


exampleExpr :: Expr Integer
exampleExpr = abs(x*34) + 5
  where
    x = sym "x"

main :: IO ()
main = do
  print exampleExpr
  preview $ exprToGr exampleExpr
