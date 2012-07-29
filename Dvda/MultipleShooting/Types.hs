{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Dvda.MultipleShooting.Types ( Step(..)
                                   , Constraint(..)
                                   , Ode(..)
                                   , BCTime(..)
                                   , eulerError
                                   , simpsonsRuleError
                                   , eulerError'
                                   , simpsonsRuleError'
                                   ) where

import Data.HashSet ( HashSet )

import Dvda.Expr ( Expr(..) )
import Dvda.HashMap ( HashMap )
import Dvda.SparseLA

data BCTime = ALWAYS | TIMESTEP Int deriving (Show, Eq)

data Constraint a = Constraint (Expr a) Ordering (Expr a) deriving Show

data Step a = Step { stepStates :: Either (Maybe [(Expr a, String)]) [Expr a]
                   , stepActions :: Either (Maybe [(Expr a, String)]) [Expr a]
                   , stepParams :: HashSet (Expr a)
                   , stepConstants :: HashSet (Expr a)
                   , stepDxdt :: Maybe [Expr a]
                   , stepCost :: Maybe (Expr a)
                   , stepDt :: Maybe (Expr a)
                   , stepBounds :: HashMap (Expr a) (a,a, BCTime)
                   , stepConstraints :: [Constraint a]
                   , stepIdx :: Int
                   , stepOutputs :: HashMap String [Expr a]
                   , stepPeriodic :: HashSet (Expr a)
                   }

data Ode a = Ode (SparseVec (Expr a) -> SparseVec (Expr a) -> SparseVec (Expr a)) (Int,Int)

wrapOdeError :: Fractional (Expr a)
                => (SparseVec (Expr a) -> SparseVec (Expr a) -> SparseVec (Expr a) -> SparseVec (Expr a) -> Ode a -> Expr a -> SparseVec (Expr a))
                -> [Expr a] -> [Expr a] -> [Expr a] -> [Expr a]
                -> ([Expr a] -> [Expr a] -> [Expr a])
                -> Expr a
                -> [Expr a]
wrapOdeError odeError xk uk xkp1 ukp1 dxdt dt =
  denseListFromSv $ odeError xk' uk' xkp1' ukp1' (Ode dxdt' (error "FUUUUCK")) dt
  where
    xk'   = svFromList xk
    xkp1' = svFromList xkp1
    uk'   = svFromList uk
    ukp1' = svFromList ukp1
    dxdt' x u = svFromList $ dxdt (denseListFromSv x) (denseListFromSv u)

eulerError' :: Fractional (Expr a)
               => [Expr a] -> [Expr a] -> [Expr a] -> [Expr a]
               -> ([Expr a] -> [Expr a] -> [Expr a])
               -> Expr a
               -> [Expr a]
eulerError' = wrapOdeError eulerError

simpsonsRuleError' :: Fractional (Expr a)
                      => [Expr a] -> [Expr a] -> [Expr a] -> [Expr a]
                      -> ([Expr a] -> [Expr a] -> [Expr a])
                      -> Expr a
                      -> [Expr a]
simpsonsRuleError' = wrapOdeError simpsonsRuleError

eulerError :: Fractional (Expr a) => SparseVec (Expr a) -> SparseVec (Expr a) -> SparseVec (Expr a) -> SparseVec (Expr a) -> Ode a -> Expr a -> SparseVec (Expr a)
eulerError xk uk xkp1 _ (Ode ode _) dt = xkp1 - (xk + svScale dt f0)
  where
    f0 = ode xk uk

simpsonsRuleError :: Fractional (Expr a) => SparseVec (Expr a) -> SparseVec (Expr a) -> SparseVec (Expr a) -> SparseVec (Expr a) -> Ode a -> Expr a -> SparseVec (Expr a)
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
