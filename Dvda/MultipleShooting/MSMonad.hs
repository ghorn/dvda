{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Dvda.MultipleShooting.MSMonad ( setStates
                                     , setActions
                                     , setParams
                                     , setConstants
                                     , setDxdt
                                     , setCost
                                     , setDt
                                     , setOutput
                                     , getTimeStep
                                     , addConstraint
                                     , setBound
                                     , runOneStep
                                     , execDxdt
                                     ) where

import Data.Array.Repa ( Z(..) )
import Data.Hashable ( Hashable )
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List ( nub, sort ) --, union )
import Data.Maybe ( isJust, isNothing )
import Control.Monad ( when, zipWithM_ )
import Control.Monad.State ( State )
import qualified Control.Monad.State as State
--import Debug.Trace ( trace )
--import Numeric.LinearAlgebra ( Element )
import Text.Printf ( printf )

import Dvda ( sym )
import Dvda.Expr ( Expr(..) )
import Dvda.MultipleShooting.Types

failDuplicates :: [String] -> [String]
failDuplicates names
  | length names == length (nub names) = names
  | otherwise = error $ "ERROR: saw duplicate names in: " ++ show (sort names)

checkOctaveName :: String -> String
checkOctaveName name
  | any (`elem` "~!@#$%^&*()+`-=[]{}\\|;:,.<>/?") name =
    error $ "ERROR: setOutput saw illegal octave variable character in string: \"" ++ name ++ "\""
  | otherwise = name

setStates :: [String] -> State (Step a) [Expr Z a]
setStates names' = do
  step <- State.get
  case stepStates step of (Right states) -> return states
                          (Left (Just _)) -> error "states already set, don't call setStates twice"
                          (Left Nothing) -> do
                            let names = failDuplicates (map checkOctaveName names')
                                syms = map (sym . (++ "_" ++ show (stepIdx step))) (failDuplicates names)
                            State.put $ step {stepStates = Left (Just (zip syms names))}
                            zipWithM_ setOutput syms names
                            return syms

setActions :: [String] -> State (Step a) [Expr Z a]
setActions names' = do
  step <- State.get
  case stepActions step of (Right actions) -> return actions
                           (Left (Just _)) -> error "actions already set, don't call setActions twice"
                           (Left Nothing) -> do
                             let names = failDuplicates (map checkOctaveName names')
                                 syms = map (sym . (++ "_" ++ show (stepIdx step))) (failDuplicates names)
                             State.put $ step {stepActions = Left (Just (zip syms names))}
                             zipWithM_ setOutput syms names
                             return syms

setParams :: (Eq (Expr Z a), Hashable (Expr Z a)) => [String] -> State (Step a) [Expr Z a]
setParams names = do
  step  <- State.get
  when (isJust (stepParams step)) $ error "params already set, don't call setParams twice"
  let syms = map sym (failDuplicates (map checkOctaveName names))
  State.put $ step {stepParams = Just (HS.fromList syms)}
  return syms

setConstants :: (Eq (Expr Z a), Hashable (Expr Z a)) => [String] -> State (Step a) [Expr Z a]
setConstants names = do
  step  <- State.get
  when (isJust (stepConstants step)) $ error "constants already set, don't call setConstants twice"
  let syms = map sym (failDuplicates (map checkOctaveName names))
  State.put $ step {stepConstants = Just (HS.fromList syms)}
  return syms

setOutput :: Expr Z a -> String -> State (Step a) ()
setOutput var name = do
  step <- State.get
  let hm = stepOutputs step
      err = error $ "ERROR: already have an output with name: \"" ++ name ++ "\""
  State.put $ step {stepOutputs = HM.insertWith err (checkOctaveName name) [var] hm}

setDt :: Expr Z a -> State (Step a) ()
setDt expr = do
  step  <- State.get
  when (isJust (stepDt step)) $ error "dt already set, don't call setDt twice"
  State.put $ step {stepDt = Just expr}

getTimeStep :: State (Step a) Int
getTimeStep = do
  step <- State.get
  return (stepIdx step)
  
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

setBound :: (Show a, Eq a, Show (Expr Z a), Eq (Expr Z a), Hashable (Expr Z a))
            => Expr Z a -> (a, a) -> BCTime -> State (Step a) ()
setBound var@(ESym _ _) (lb, ub) bnd = do
  step <- State.get
  let k = stepIdx step
      newbnd = (lb,ub,bnd)
      oldBounds = stepBounds step

      err old = error $ printf "ERROR: setBound called twice on %s (old bound: %s, new bound: %s)" (show var) (show old) (show newbnd)

  let putNewBnd = case bnd of
        (TIMESTEP j) -> if j /= k
                        then Nothing
                        else case (HM.lookup var (stepBounds step)) of
                          Just oldbnd@(_, _, TIMESTEP _) -> err oldbnd
                          _ -> Just newbnd
        ALWAYS -> case (HM.lookup var (stepBounds step)) of
          Just oldbnd@(_,_,ALWAYS) -> err oldbnd
          Just (_,_,TIMESTEP _) -> Nothing
          Nothing -> Just newbnd

  when (isJust putNewBnd) $
    State.put $ step {stepBounds = HM.insert var newbnd oldBounds}
setBound _ _ _ = error "WARNING - setBound called on non-design variable, use addConstraint instead"


addConstraint :: Expr Z a -> Ordering -> Expr Z a -> State (Step a) ()
addConstraint x ordering y =
  State.state (\step -> ((), step {stepConstraints = (stepConstraints step) ++ [Constraint x ordering y]}))


runOneStep :: State (Step a) b -> Int -> Step a
runOneStep userStep k
  | isNothing (stepDxdt ret) = error "ERROR: need to set dxdt"
  | isNothing (stepDt ret) = error "ERROR: need to set timestep dt"
  | otherwise = stateErr `seq` actionErr `seq` ret
  where
    stateErr = case stepStates ret of Left Nothing -> error "ERROR: need to set states"
                                      _ -> ()
    actionErr = case stepActions ret of Left Nothing -> error "ERROR: need to set actions"
                                        _ -> ()
    ret = State.execState userStep $ Step { stepStates = Left Nothing
                                          , stepActions = Left Nothing
                                          , stepDxdt = Nothing
                                          , stepCost = Nothing
                                          , stepDt = Nothing
                                          , stepBounds = HM.empty
                                          , stepConstraints = []
                                          , stepParams = Nothing
                                          , stepConstants = Nothing
                                          , stepIdx = k
                                          , stepOutputs = HM.empty
                                          }

execDxdt :: Num (Expr Z a)
            => State (Step a) b -> Int -> [Expr Z a] -> [Expr Z a] -> [Expr Z a]
execDxdt userStep k x u = case stepDxdt $ State.execState userStep step0 of
  Nothing -> error "ERROR: need to set dxdt"
  Just dxdt -> dxdt
  where
    step0 = Step { stepStates  = Right x
                 , stepActions = Right u
                 , stepDxdt = Nothing
                 , stepDt = Nothing
                 , stepCost = Nothing
                 , stepBounds = HM.empty
                 , stepConstraints = []
                 , stepParams = Nothing
                 , stepConstants = Nothing
                 , stepIdx = k
                 , stepOutputs = HM.empty
                 }
