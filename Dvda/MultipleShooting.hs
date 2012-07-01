{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Dvda.MultipleShooting ( multipleShooting
                             , simpleSystem
                             , boundEqs
                             , boundEq
                             , boundInterval
                             , boundIntervals
                             , ltZero
                             , replaceFinalCost
                             , devectorize
                             ) where

import Text.Printf
import Data.List ( elemIndex, mapAccumL, zipWith6 )
import Data.Maybe

import Dvda ( Expr, Z, svec )
import Dvda.SparseLA

data Ode a = Ode (SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a)) (Int,Int)
data Cost a = Cost (SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> Expr Z a) (Int,Int)

data Constraint a = Constraint { expression :: SparseVec (Expr Z a)
                               , lcon :: SparseVec Double
                               , ucon :: SparseVec Double
                               } deriving Show

data Bound a = Bound { dvIdx :: Int
                     , lbound :: Double
                     , ubound :: Double
                     , var :: Expr Z a
                     } deriving Show

data System a = System { odes :: [Ode a]
                       , costs :: [Cost a]
                       , dts :: [Expr Z a]
                       }

data MultipleShooting a = MultipleShooting { system :: System a
                                           , states :: [SparseVec (Expr Z a)]
                                           , actions :: [SparseVec (Expr Z a)]
                                           , params :: SparseVec (Expr Z a)
                                           , dodeConstraints :: [Constraint a]
                                           , objFun :: Expr Z a
                                           , designVars :: SparseVec (Expr Z a)
                                           }

devectorize :: SparseVec Double -> MultipleShooting a -> ([SparseVec Double], [SparseVec Double], SparseVec Double)
devectorize sol ms = (xTraj, uTraj, svFromList params')
  where
    (xDims, uDims) = unzip $ map (\(Cost _ d) -> d) (costs (system ms))
    (sol', xTraj) = mapAccumL f (denseListFromSv sol) xDims
    (params', uTraj) = mapAccumL f sol' uDims

    f acc n = (xs, svFromList x)
      where
        (x,xs) = splitAt n acc

idxOfVec :: (Eq (Expr Z a), Num (Expr Z a)) => Expr Z a -> SparseVec (Expr Z a) -> Int
idxOfVec val vec
  | isJust idx = fromJust idx
  | otherwise  = error $ "Error - idxOfMat fail"
  where
    idx = elemIndex val (denseListFromSv vec)


boundEq :: (Eq (Expr Z a), Num (Expr Z a)) => MultipleShooting a -> Expr Z a -> Double -> Bound a
boundEq ms x val = Bound { dvIdx = idxOfVec x (designVars ms)
                         , lbound = val
                         , ubound = val
                         , var = x
                         }

boundEqs :: (Eq (Expr Z a), Num (Expr Z a))
            => MultipleShooting a -> SparseVec (Expr Z a) -> SparseVec Double -> [Bound a]
boundEqs ms xs vals = zipWith (boundEq ms) (denseListFromSv xs) (denseListFromSv vals)

boundInterval :: (Eq (Expr Z a), Num (Expr Z a)) => MultipleShooting a -> Expr Z a -> (Double, Double) -> Bound a
boundInterval ms x (lb, ub) = Bound { dvIdx = idxOfVec x (designVars ms)
                                    , lbound = lb
                                    , ubound = ub
                                    , var = x
                                    }

boundIntervals :: (Eq (Expr Z a), Num (Expr Z a))
                  => MultipleShooting a -> SparseVec (Expr Z a) -> [(Double,Double)] -> [Bound a]
boundIntervals ms xs bnds = zipWith (boundInterval ms) (denseListFromSv xs) bnds


multipleShooting :: Fractional (Expr Z a) => System a -> SparseVec (Expr Z a) -> MultipleShooting a
multipleShooting sys params'
  | dimensionsMatch = MultipleShooting { system = sys
                                       , states = states'
                                       , actions = actions'
                                       , params = params'
                                       , dodeConstraints = dcs
                                       , objFun = objFun'
                                       , designVars = dvs
                                       }
  | otherwise = error $ printf "Error in multipleShooting: lengths of odes (%d), costs (%d), dts (%d) are not consistent" nOdes nCosts nDts
  where
    dimensionsMatch = (nOdes == nDts) && (nCosts == nOdes + 1) && (and $ zipWith (==) odeDims costDims)

    odeDims = map (\(Ode _ d) -> d) (odes sys)
    costDims = map (\(Cost _ d) -> d) (costs sys)

    nOdes = length (odes sys)
    nCosts = length (costs sys)
    nDts = length (dts sys)

    states'  = zipWith (\(nx,_) k -> svec ("x_"++show k) nx) costDims [0..nCosts-1]
    actions' = zipWith (\(_,nu) k -> svec ("u_"++show k) nu) costDims [0..nCosts-1]

    dcs = map eqZero $ zipWith6 simpsonsRuleError (init states') (init actions') (tail states') (tail actions') (odes sys) (dts sys)

    objFun' = sum $ zipWith3 (\(Cost cost _) x u -> cost x u) (costs sys) states' actions'

    dvs = svCats [svCats states', svCats actions', params']

--multipleShootingSolver :: MultipleShooting a -> [Constraint a] -> IO ([Bound a] -> SparseVec Double -> IO (SparseVec Double, Double))
--multipleShootingSolver ms moreConstraints = do
--  let allConstraints = concat [dodeConstraints ms, moreConstraints]
--
----  solver <- createSolver IpoptExactHessian (designVars ms) (objFun ms) (svCats $ map expression allConstraints)
--  solver <- createSolver Snopt (designVars ms) (objFun ms) (svCats $ map expression allConstraints)
--  
--  let solve :: [Bound] -> SparseVec Double -> IO (SparseVec Double, Double)
--      solve bounds xGuess = solveNlp solver xGuess (svFromList boxLbs, svFromList boxUbs) (nlLbs, nlUbs)
--        where
--          nlLbs = svCats $ map lcon allConstraints
--          nlUbs = svCats $ map ucon allConstraints
--          (boxLbs, boxUbs) = unzip $ map f [0..rows (designVars ms)-1]
--          f idx
--            | isNothing bnd = (-1e50, 1e50)
--            | otherwise     = (lbound (fromJust bnd), ubound (fromJust bnd))
--              where
--                bnd = find (\x -> idx == dvIdx x) bounds
--
--  return solve

simpleSystem :: Ode a -> Cost a -> Expr Z a -> Int -> System a
simpleSystem ode cost dt n = System { odes = replicate (n-1) ode
                                    , costs = replicate n cost
                                    , dts = replicate (n-1) dt
                                    }

replaceFinalCost :: Cost a -> System a -> System a
replaceFinalCost cost sysIn = System { odes = odes sysIn
                                     , costs = init (costs sysIn) ++ [cost]
                                     , dts = dts sysIn
                                     }


simpsonsRuleError :: Fractional (Expr Z a) => SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> Ode a -> Expr Z a -> SparseVec (Expr Z a)
simpsonsRuleError xk uk xkp1 ukp1 (Ode ode _) dt = xkp1 - xk - (svScale (dt/6.0) (f0 + fourFm + f1))
  where
    f0 = ode xk uk
    f1 = ode xkp1 ukp1

    um = svScale 0.5 (uk + ukp1)
    xm = xm' - xm''
      where
        xm' = svScale 0.5 (xk + xkp1)
        xm'' = svScale (0.125*dt) (f1 - f0)

    fm = ode xm um
    fourFm = svScale 4 fm

eqZero :: SparseVec (Expr Z a) -> Constraint a
eqZero g = Constraint {expression = g, lcon = zeros', ucon = zeros'}
  where
    zeros' = svZeros (svSize g)

ltZero :: SparseVec (Expr Z a) -> Constraint a
ltZero g = Constraint {expression = g, lcon = veryNegative, ucon = zeros'}
  where
    veryNegative = svFromList $ replicate (svSize g) (-1e30)
    zeros' = svZeros (svSize g)
