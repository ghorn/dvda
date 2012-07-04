{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Dvda.MultipleShooting.MSMonad ( setStates
                                     , setActions
                                     , setParams
                                     , setConstants
                                     , setDxdt
                                     , setCost
                                     , addConstraint
                                     , setBound
                                     , runOneStep
                                     ) where

import Data.Array.Repa ( Z(..) )
import Data.Hashable ( Hashable )
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List ( nub, sort ) --, union )
import Data.Maybe ( isJust, isNothing )
import Control.Monad ( when )
import Control.Monad.State ( State )
import qualified Control.Monad.State as State
import Debug.Trace ( trace )
import Text.Printf ( printf )

import Dvda ( sym )
import Dvda.Expr ( Expr(..), Const(..) )
import Dvda.MultipleShooting.Types

failDuplicates :: [String] -> [String]
failDuplicates names
  | length names == length (nub names) = names
  | otherwise = error $ "ERROR: saw duplicate names in: " ++ show (sort names)

setStates :: [String] -> State (Step a) [Expr Z a]
setStates names' = do
  step <- State.get
  when (isJust (stepStates step)) $ error "states already set, don't call setStates twice"
  let names = failDuplicates names'
      syms = map (sym . (++ "_" ++ show (stepIdx step))) (failDuplicates names)
  State.put $ step {stepStates = Just (zip syms names)}
  return syms

setActions :: [String] -> State (Step a) [Expr Z a]
setActions names' = do
  step <- State.get
  when (isJust (stepActions step)) $ error "actions already set, don't call setActions twice"
  let names = failDuplicates names'
      syms = map (sym . (++ "_" ++ show (stepIdx step))) (failDuplicates names)
  State.put $ step {stepActions = Just (zip syms names)}
  return syms

setParams :: (Eq (Expr Z a), Hashable (Expr Z a)) => [String] -> State (Step a) [Expr Z a]
setParams names = do
  step  <- State.get
  when (isJust (stepParams step)) $ error "params already set, don't call setParams twice"
  let syms = map sym (failDuplicates names)
  State.put $ step {stepParams = Just (HS.fromList syms)}
  return syms

setConstants :: (Eq (Expr Z a), Hashable (Expr Z a)) => [String] -> State (Step a) [Expr Z a]
setConstants names = do
  step  <- State.get
  when (isJust (stepConstants step)) $ error "constants already set, don't call setConstants twice"
  let syms = map sym (failDuplicates names)
  State.put $ step {stepConstants = Just (HS.fromList syms)}
  return syms
  
-------------------------------------------

setDxdt :: [Expr Z a] -> State (Step a) ()
setDxdt vars = do
  step  <- State.get
  when (isJust (stepDxdt step)) $ error "dxdt already set, don't call setDxdt twice"
  State.put $ step {stepDxdt = Just vars}

setCost :: Expr Z a -> State (Step a) ()
setCost var = do
  step  <- State.get
  when (isJust (stepCost step)) $ error "cost already set, don't call setCost twice"
  State.put $ step {stepCost = Just var}

setBound :: (Show a, Show (Expr Z a), Eq (Expr Z a), Hashable (Expr Z a)) => Expr Z a -> (a, a) -> State (Step a) ()
setBound var@(ESym _ _) (lb, ub) = do
  step <- State.get
  let oldBounds = stepBounds step
      newBounds = HM.insertWith merge var (lb,ub) oldBounds
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
  State.state (\step -> ((), step {stepConstraints = (stepConstraints step) ++ [Constraint x ordering y]}))


runOneStep :: State (Step a) b -> Int -> Step a
runOneStep userStep k
  | isNothing (stepStates  ret) = error "ERROR: need to set states"
  | isNothing (stepActions ret) = error "ERROR: need to set actions"
  | isNothing (stepDxdt    ret) = error "ERROR: need to set dxdt"
  | otherwise = ret
  where
    ret = State.execState userStep $ Step { stepStates = Nothing
                                          , stepActions = Nothing
                                          , stepDxdt = Nothing
                                          , stepCost = Nothing
                                          , stepBounds = HM.empty
                                          , stepConstraints = []
                                          , stepParams = Nothing
                                          , stepConstants = Nothing
                                          , stepIdx = k
                                          }


--simpleSystem userStep n = undefined
--  where
--    steps = map (runOneStep userStep) [0..n-1]
--    -- ensure that state/action (names) are the same in all steps
--    f Nothing hs = hs
--    f (Just next) hs = HS.union hs next
--    params    = foldr f HS.empty (map stepParams    steps)
--    constants = foldr f HS.empty (map stepConstants steps)
--    states'  = map (fst . unzip . fromJust . stepStates ) steps -- fromJust checked in runOneStep
--    actions' = map (fst . unzip . fromJust . stepActions) steps -- fromJust checked in runOneStep
--
--    stateNames  = map (snd . unzip . fromJust . stepStates ) steps  -- fromJust checked in runOneStep
--    actionNames = map (snd . unzip . fromJust . stepActions) steps  -- fromJust checked in runOneStep    
--
--    states = if all (head stateNames  ==) stateNames
--             then states'
--             else error "ERROR: different states in different timesteps"
--    actions = if all (head stateNames  ==) stateNames
--              then actions'
--              else error "ERROR: different actions in different timesteps"
--
--    constraints = concatMap stepConstraints steps
--
    

--cartpole :: State (Step Double) ()
--cartpole = do
--  [x, v] <- setStates ["x","v"]
--  [u] <- setActions ["u"]
--  [k, b] <- setConstants ["k", "b"]
--  setDxdt [v, -k*x - b*v + u]
--  setBound x (-5, 5)
--  setBound v (-10, 10)
--  setBound u (-20, 10)
