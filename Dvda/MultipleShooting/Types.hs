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

import Data.HashMap.Lazy ( HashMap )
import Data.HashSet ( HashSet )

import Dvda ( Z )
import Dvda.Expr ( Expr(..) )
import Dvda.SparseLA

data BCTime = ALWAYS | TIMESTEP Int deriving (Show, Eq)

data Constraint a = Constraint (Expr Z a) Ordering (Expr Z a) deriving Show

data Step a = Step { stepStates :: Either (Maybe [(Expr Z a, String)]) [Expr Z a]
                   , stepActions :: Either (Maybe [(Expr Z a, String)]) [Expr Z a]
                   , stepParams :: Maybe (HashSet (Expr Z a))
                   , stepConstants :: Maybe (HashSet (Expr Z a))
                   , stepDxdt :: Maybe [Expr Z a]
                   , stepCost :: Maybe (Expr Z a)
                   , stepDt :: Maybe (Expr Z a)
                   , stepBounds :: HashMap (Expr Z a) (a,a, BCTime)
                   , stepConstraints :: [Constraint a]
                   , stepIdx :: Int
                   , stepOutputs :: HashMap String [Expr Z a]
                   }

data Ode a = Ode (SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a)) (Int,Int)

wrapOdeError :: Fractional (Expr Z a)
                => (SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> SparseVec (Expr Z a) -> Ode a -> Expr Z a -> SparseVec (Expr Z a))
                -> [Expr Z a] -> [Expr Z a] -> [Expr Z a] -> [Expr Z a]
                -> ([Expr Z a] -> [Expr Z a] -> [Expr Z a])
                -> Expr Z a
                -> [Expr Z a]
wrapOdeError odeError xk uk xkp1 ukp1 dxdt dt =
  denseListFromSv $ odeError xk' uk' xkp1' ukp1' (Ode dxdt' (error "FUUUUCK")) dt
  where
    xk'   = svFromList xk
    xkp1' = svFromList xkp1
    uk'   = svFromList uk
    ukp1' = svFromList ukp1
    dxdt' x u = svFromList $ dxdt (denseListFromSv x) (denseListFromSv u)

eulerError' :: Fractional (Expr Z a)
               => [Expr Z a] -> [Expr Z a] -> [Expr Z a] -> [Expr Z a]
               -> ([Expr Z a] -> [Expr Z a] -> [Expr Z a])
               -> Expr Z a
               -> [Expr Z a]
eulerError' = wrapOdeError eulerError

simpsonsRuleError' :: Fractional (Expr Z a)
                      => [Expr Z a] -> [Expr Z a] -> [Expr Z a] -> [Expr Z a]
                      -> ([Expr Z a] -> [Expr Z a] -> [Expr Z a])
                      -> Expr Z a
                      -> [Expr Z a]
simpsonsRuleError' = wrapOdeError simpsonsRuleError

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
