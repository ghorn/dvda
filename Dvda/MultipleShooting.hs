{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Dvda.MultipleShooting ( Ode(..)
                             , Cost(..)
                             , MultipleShooting(..)
                             , Constraint(..)
                             , ConstraintType(..)
                             , DesignVars(..)
                             , Bound(..)
                             , multipleShooting
                             , simpleSystem
                             , boundEqs
                             , boundEq
                             , boundInterval
                             , boundIntervals
                             , ltZero
                             , replaceFinalCost
                             , vectorizeDvs
                             , dvIdx
                             , numDvs
                             , interpolateInitialGuess
                             , simpsonsRuleError
                             , eulerError
                             ) where

import Text.Printf
import Data.List ( elemIndex, zipWith6 )
import Data.Maybe

import Dvda ( Expr, Z, svec )
import Dvda.SparseLA ( SparseVec, svCats, svScale, svZeros, svSize, sparseListFromSv, svFromList )

data Ode a = Ode (SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a)) (Int,Int)
data Cost a = Cost (SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> Expr Z a) (Int,Int)

data ConstraintType = ConstraintLt | ConstraintGt | ConstraintEq deriving Show
data Constraint a = Constraint ConstraintType (SparseVec (Expr Z a)) (SparseVec (Expr Z a)) deriving Show

data Bound a = Bound { var :: Expr Z a
                     , lbound :: a
                     , ubound :: a
                     } deriving Show

data System a = System { odes :: [Ode a]
                       , costs :: [Cost a]
                       , dts :: [Expr Z a]
                       }

data DesignVars a = DesignVars { dvStates :: [SparseVec a]
                               , dvActions :: [SparseVec a]
                               , dvParams :: [a]
                               }

data MultipleShooting a = MultipleShooting { msSystem :: System a
                                           , msDesignVars :: DesignVars (Expr Z a)
                                           , msDodeConstraints :: [Constraint a]
                                           , msRuntimeConstants :: [Expr Z a]
                                           , msObjFun :: Expr Z a
                                           }

numDvs :: MultipleShooting a -> Int
numDvs = length . vectorizeDvs . msDesignVars

dvIdx :: Eq (Expr Z a) => MultipleShooting a -> Expr Z a -> Int
dvIdx ms val
  | isJust idx = fromJust idx
  | otherwise  = error $ "Error - idxOfDvs fail"
  where
    idx = elemIndex val (vectorizeDvs $ msDesignVars ms)

vectorizeDvs :: DesignVars a -> [a]
vectorizeDvs (DesignVars {dvStates = states, dvActions = actions, dvParams = params}) =
  sparseListFromSv (svCats [svCats states, svCats actions]) ++ params
  

boundEq :: Eq (Expr Z a) => Expr Z a -> a -> Bound a
boundEq x val = Bound { lbound = val
                      , ubound = val
                      , var = x
                      }

boundEqs :: Eq (Expr Z a) => SparseVec (Expr Z a) -> SparseVec a -> [Bound a]
boundEqs xs vals = zipWith boundEq (sparseListFromSv xs) (sparseListFromSv vals)

boundInterval :: Eq (Expr Z a) => Expr Z a -> (a, a) -> Bound a
boundInterval x (lb, ub) = Bound { lbound = lb
                                 , ubound = ub
                                 , var = x
                                 }

boundIntervals :: Eq (Expr Z a) => SparseVec (Expr Z a) -> [(a,a)] -> [Bound a]
boundIntervals xs bnds = zipWith boundInterval (sparseListFromSv xs) bnds


multipleShooting :: Fractional (Expr Z a) => System a -> [Expr Z a] -> [Expr Z a]
                    -> (SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> Ode a -> Expr Z a -> SparseVec (Expr Z a))
                    -> MultipleShooting a
multipleShooting sys params runtimeConstants odeError
  | dimensionsMatch = MultipleShooting { msSystem = sys
                                       , msDesignVars = DesignVars { dvStates = states
                                                                   , dvActions = actions
                                                                   , dvParams = params
                                                                   }
                                       , msDodeConstraints = dcs
                                       , msRuntimeConstants = runtimeConstants
                                       , msObjFun = objFun
                                       }
  | otherwise = error $ printf "Error in multipleShooting: lengths of odes (%d), costs (%d), dts (%d) are not consistent" nOdes nCosts nDts
  where
    dimensionsMatch = (nOdes == nDts) && (nCosts == nOdes + 1) && (and $ zipWith (==) odeDims costDims)

    odeDims = map (\(Ode _ d) -> d) (odes sys)
    costDims = map (\(Cost _ d) -> d) (costs sys)

    nOdes = length (odes sys)
    nCosts = length (costs sys)
    nDts = length (dts sys)

    states  = zipWith (\(nx,_) k -> svec ("x_"++show k) nx) costDims [0..nCosts-1]
    actions = zipWith (\(_,nu) k -> svec ("u_"++show k) nu) costDims [0..nCosts-1]

    dcs = map eqZero $ zipWith6 odeError (init states) (init actions) (tail states) (tail actions) (odes sys) (dts sys)

    objFun = sum $ zipWith3 (\(Cost cost _) x u -> cost x u) (costs sys) states actions

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

eulerError :: Fractional (Expr Z a) => SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> Ode a -> Expr Z a -> SparseVec (Expr Z a)
eulerError xk uk xkp1 _ (Ode ode _) dt = xkp1 - (xk + svScale dt f0)
  where
    f0 = ode xk uk

simpsonsRuleError :: Fractional (Expr Z a) => SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> Ode a -> Expr Z a -> SparseVec (Expr Z a)
simpsonsRuleError xk uk xkp1 ukp1 (Ode ode _) dt = xkp1 - xk - (svScale (dt/6.0) (f0 + fourFm + f1))
  where
    f0 = ode xk uk
    f1 = ode xkp1 ukp1

    um = svScale 0.5 (uk + ukp1)
    xm = xm' - xm''
      where
        xm' = svScale 0.5 (xk + xkp1)
        xm'' = svScale (0.125 * dt) (f1 - f0)

    fm = ode xm um
    fourFm = svScale 4 fm

eqZero :: SparseVec (Expr Z a) -> Constraint a
eqZero g = Constraint ConstraintEq g (svZeros $ svSize g)

ltZero :: Fractional a => SparseVec (Expr Z a) -> Constraint a
ltZero g = Constraint ConstraintLt g (svZeros $ svSize g)

interpolateInitialGuess :: Fractional a => SparseVec a -> SparseVec a -> Int -> [SparseVec a]
interpolateInitialGuess x0 xf n' = map (combine x0 xf) alphas
  where
    n = fromIntegral n'
    alphas = map (/ (n-1)) $ map fromIntegral [0..n'-1]
    combine v0 vf alpha =
      svFromList $ zipWith (\x y -> alpha*x + (1-alpha)*y) (sparseListFromSv v0) (sparseListFromSv vf)

