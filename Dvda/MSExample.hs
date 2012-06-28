{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}

module Dvda.MSExample( problem
                     , run
                     ) where

import qualified Data.IntMap as IM

import Dvda
import Dvda.SparseLA
import Dvda.MS

n :: Int
n = 2

nx :: Int
nx = 2

nu :: Int
nu = 1 

stateMat :: SparseMat (Expr DIM0 Double)
stateMat = smat "x" (nx, n)

actionMat :: SparseMat (Expr DIM0 Double)
actionMat = smat "u" (nu,n)

time :: Expr DIM0 Double
time = sym "time"

ts :: Expr DIM0 Double
ts = time / (fromIntegral n - 1)

stateVecs :: [SparseVec (Expr DIM0 Double)]
stateVecs = map (flip getCol stateMat) [0..n-1]

actionVecs :: [SparseVec (Expr DIM0 Double)]
actionVecs = map (flip getCol actionMat) [0..n-1]

stateLists :: [[Expr DIM0 Double]]
stateLists = map (\(SparseVec _ xs) -> IM.elems xs) stateVecs

actionLists :: [[Expr DIM0 Double]]
actionLists = map (\(SparseVec _ xs) -> IM.elems xs) actionVecs


--------------------------------------------------------
myOde :: [Expr DIM0 Double] -> [Expr DIM0 Double] -> [Expr DIM0 Double]
myOde xs us = [v, -k*x - b*v + u]
  where
    [x,v] = xs
    [u] = us
    k = 5
    b = 0.2

errVecs :: [SparseVec (Expr DIM0 Double)]
errVecs = dynamicsErrorsEuler stateVecs actionVecs myOde ts

errLists :: [[Expr DIM0 Double]]
errLists = map (\(SparseVec _ xs) -> IM.elems xs) errVecs

costFun :: [[Expr DIM0 Double]] -> [[Expr DIM0 Double]] -> Expr DIM0 Double
costFun stateLists' actionLists' = sum (map stateCost stateLists') + sum (map actionCost actionLists')
  where
    stateCost [x,v] = 2*x*x + 3*v*v
    stateCost _ = error "goddammit"

    actionCost [u] = 7*u*u
    actionCost _ = error "goddammit again"

dvList :: [Expr DIM0 Double]
dvList = [time] ++ concat stateLists ++ concat actionLists

ceqs :: [Expr Z Double]
ceqs = concat errLists

cineqs :: [Expr Z Double]
cineqs = []

cost :: Expr Z Double
cost = costFun stateLists actionLists

problem :: FunGraph Double [Expr Z Double]
           (Expr Z Double :* [Expr Z Double] :* [Expr Z Double] :* [[Expr Z Double]] :* [Expr Z Double] :* [[Expr Z Double]])
problem = msProblem ceqs cineqs cost dvList

run :: IO ()
run = do
  _ <- buildHSFunctionFromGraph problem
  print "yay"
