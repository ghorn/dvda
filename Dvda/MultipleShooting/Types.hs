{-# OPTIONS_GHC -Wall #-}

module Dvda.MultipleShooting.Types ( Step(..)
                                   , Constraint(..)
                                   , Ode(..)
                                   ) where

import Data.HashMap.Lazy ( HashMap )
import Data.HashSet ( HashSet )

import Dvda ( Z )
import Dvda.Expr ( Expr(..) )

data Constraint a = Constraint (Expr Z a) Ordering (Expr Z a) deriving Show

data Step a = Step { stepStates :: Maybe [(Expr Z a, String)]
                   , stepActions :: Maybe [(Expr Z a, String)]
                   , stepParams :: Maybe (HashSet (Expr Z a))
                   , stepConstants :: Maybe (HashSet (Expr Z a))
                   , stepDxdt :: Maybe [Expr Z a]
                   , stepCost :: Maybe (Expr Z a)
                   , stepBounds :: HashMap (Expr Z a) (a,a)
                   , stepConstraints :: [Constraint a]
                   , stepIdx :: Int
                   }

data Ode a = Ode ([Expr Z a] -> [Expr Z a] -> [Expr Z a])
