{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}

module Dvda.MSExample( run
                     ) where

import qualified Data.IntMap as IM

import Dvda
import Dvda.SparseLA
import Dvda.MS
import Dvda.Graph

n :: Int
n = 2

nx :: Int
nx = 2

nu :: Int
nu = 1 

stateMat :: SparseMat (Expr Z Double)
stateMat = smat "x" (nx, n)

actionMat :: SparseMat (Expr Z Double)
actionMat = smat "u" (nu,n)

time :: Expr Z Double
time = sym "time"

bSpring :: Expr Z Double
bSpring = sym "b"

kSpring :: Expr Z Double
kSpring = sym "k"

ts :: Expr Z Double
ts = time / (fromIntegral n - 1)

stateVecs :: [SparseVec (Expr Z Double)]
stateVecs = map (flip getCol stateMat) [0..n-1]

actionVecs :: [SparseVec (Expr Z Double)]
actionVecs = map (flip getCol actionMat) [0..n-1]

stateLists :: [[Expr Z Double]]
stateLists = map (\(SparseVec _ xs) -> IM.elems xs) stateVecs

actionLists :: [[Expr Z Double]]
actionLists = map (\(SparseVec _ xs) -> IM.elems xs) actionVecs

paramList :: [Expr Z Double]
paramList = [bSpring, kSpring]

--------------------------------------------------------

myOde :: [Expr Z Double] -> [Expr Z Double] -> [Expr Z Double]
myOde xs us = [v, -k*x - b*v + u]
  where
    [x,v] = xs
    [u] = us
    k = 5
    b = 0.2

errVecs :: [SparseVec (Expr Z Double)]
errVecs = dynamicsErrorsEuler stateVecs actionVecs myOde ts

errLists :: [[Expr Z Double]]
errLists = map (\(SparseVec _ xs) -> IM.elems xs) errVecs

costFun :: [[Expr Z Double]] -> [[Expr Z Double]] -> Expr Z Double
costFun stateLists' actionLists' = sum (map stateCost stateLists') + sum (map actionCost actionLists')
  where
    stateCost [x,v] = 2*x*x + 3*v*v
    stateCost _ = error "goddammit"

    actionCost [u] = 7*u*u
    actionCost _ = error "goddammit again"

dvList :: [Expr Z Double]
dvList = [time] ++ concat stateLists ++ concat actionLists

ceqs :: [Expr Z Double]
ceqs = concat errLists

cineqs :: [Expr Z Double]
cineqs = []

cost :: Expr Z Double
cost = costFun stateLists actionLists

run :: IO ()
run = do
  let costFg = msCost cost dvList paramList
      cnstFg = msConstraints ceqs cineqs dvList paramList

      fundir = "testProblem"

  let FunGraph _ _ _ (cost :* costGrad) = costFg
      FunGraph _ _ _ (cineqs :* ceqs :* cineqsJacobs :* ceqsJacobs) = cnstFg

  putStrLn "--------------------------------------------------------------"
  putStrLn $ fullShow costFg cost
  putStrLn "--------------------------------------------------------------"
  mapM_ putStrLn $ map (fullShow costFg) costGrad
  
  _ <- writeMS fundir "cost" costFg
  _ <- writeMS fundir "constraints" cnstFg

  return ()