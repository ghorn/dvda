{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Dvda.MSMonad ( -- * monadic dsl api
                      setStates
                    , setActions
                    , setParams
                    , setConstants
                    , setDxdt
                    , setCost
                    , addConstraint
                    , setBound
                      -- * monadic dsl internal
--                    , runSteps
                    ) where

import Data.Array.Repa ( Z(..) )
import Data.List ( nub, union, sort )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( isJust )
import Control.Monad ( when )
import Control.Monad.State ( State )
import qualified Control.Monad.State as State
import Data.Set ( Set )
import Debug.Trace ( trace )
import Text.Printf ( printf )

import Dvda ( sym )
import Dvda.Expr ( Expr(..), Const(..) )
import Dvda.MultipleShooting ( Bound(..) )

data Constraint' a = Constraint' (Expr Z a) Ordering (Expr Z a) deriving Show

data Step a = Step { stepStates :: Maybe [Expr Z a]
                   , stepActions :: Maybe [Expr Z a]
                   , stepDxdt :: Maybe [Expr Z a]
                   , stepCost :: Maybe (Expr Z a)
                   , stepBounds :: Map (Expr Z a) (a,a)
                   , stepConstraints :: [Constraint' a]
                   , stepParams :: Maybe [Expr Z a]
                   , stepConstants :: Maybe [Expr Z a]
                   }

failDuplicates :: [String] -> [String]
failDuplicates names
  | length names == length (nub names) = names
  | otherwise = error $ "ERROR: saw duplicate names in: " ++ show (sort names)

setStates :: [String] -> State (Step a, Int) [Expr Z a]
setStates names = do
  (step,k) <- State.get
  when (isJust (stepStates step)) $ error "states already set, don't call setStates twice"
  let syms = map (sym . (++ "_" ++ show k)) (failDuplicates names)
  State.put (step {stepStates = Just syms}, k)
  return syms

setActions :: [String] -> State (Step a, Int) [Expr Z a]
setActions names = do
  (step,k) <- State.get
  when (isJust (stepActions step)) $ error "actions already set, don't call setActions twice"
  let syms = map (sym . (++ "_" ++ show k)) (failDuplicates names)
  State.put $ (step {stepActions = Just syms}, k)
  return syms

setParams :: Eq (Expr Z a) => [String] -> State (Step a, Int) [Expr Z a]
setParams names = do
  (step, k)  <- State.get
  when (isJust (stepParams step)) $ error "params already set, don't call setParams twice"
  let syms = map sym (failDuplicates names)
  State.put $ (step {stepParams = Just syms}, k)
  return syms

setConstants :: Eq (Expr Z a) => [String] -> State (Step a, Int) [Expr Z a]
setConstants names = do
  (step, k)  <- State.get
  when (isJust (stepConstants step)) $ error "constants already set, don't call setConstants twice"
  let syms = map sym (failDuplicates names)
  State.put $ (step {stepConstants = Just syms}, k)
  return syms
  
-------------------------------------------

setDxdt :: [Expr Z a] -> State (Step a, Int) ()
setDxdt vars = do
  (step, k)  <- State.get
  when (isJust (stepDxdt step)) $ error "dxdt already set, don't call setDxdt twice"
  State.put $ (step {stepDxdt = Just vars}, k)

setCost :: Expr Z a -> State (Step a, Int) ()
setCost var = do
  (step, k)  <- State.get
  when (isJust (stepCost step)) $ error "cost already set, don't call setCost twice"
  State.put $ (step {stepCost = Just var}, k)

setBound :: (Show a, Show (Expr Z a), Ord (Expr Z a)) => Expr Z a -> (a, a) -> State (Step a) ()
setBound var@(ESym _ _) (lb, ub) = do
  step <- State.get
  let bound = Bound {boundVar = var, boundL = lb, boundU = ub}
      oldBounds = stepBounds step
      newBounds = Map.insertWith merge var (lb,ub) oldBounds
        where
          merge new old = trace msg new
            where
              msg = printf "WARNING: setBound called twice on %s, using new bound\nold bound: %s, new bound: %s" (show var) (show old) (show new)
  State.put $ step {stepBounds = newBounds}
setBound var (lb, ub) = trace "WARNING - setBound called on non-design variable, using addConstraint instead" $ do
  addConstraint var GT (EConst (CSingleton Z lb))
  addConstraint var LT (EConst (CSingleton Z ub))


addConstraint :: Expr Z a -> Ordering -> Expr Z a -> State (Step a) ()
addConstraint x ordering y =
  State.state (\step -> ((), step {stepConstraints = (stepConstraints step) ++ [Constraint' x ordering y]}))


------------------------------------------------------------------------
--runSteps :: Eq (Expr Z a) => State (Step a, Int) b -> Int -> Sys a
--runSteps userStepFun n = Sys { sysSteps = steps
--                             , sysParams = params
--                             , sysConstants = rcs
--                             }
--  where
--    emptyStep = Step { stepStates = []
--                     , stepActions = []
--                     , stepDxdt = []
--                     , stepBounds = []
--                     , stepConstraints = []
--                     , stepCost = Nothing
--                     }
--    runStep userStepFun' k = State.execState userStepFun' (emptyStep, k, ([], []))
--    outs = map (runStep userStepFun) [0..n-1]
--    
--    steps = map (\(step,_,_) -> step) outs
--    params = nub $ concat $ map (\(_,_,(params',   _)) -> params') outs
--    rcs =    nub $ concat $ map (\(_,_,(      _,rcs')) -> rcs'   ) outs

cartpole :: (Eq (Expr Z a), Num (Expr Z a)) => State (Step a, Int) ()
cartpole = do
  [x, v] <- setStates ["x","v"]
  [u] <- setActions ["u"]
  [k, b] <- setConstants ["k", "b"]
  setDxdt [v, -k*x - b*v + u]
