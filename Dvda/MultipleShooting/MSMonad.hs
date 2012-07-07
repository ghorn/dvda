{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Dvda.MultipleShooting.MSMonad ( State
                                     , setStates
                                     , setActions
                                     , addParam
                                     , addParams
                                     , addConstant
                                     , addConstants
                                     , setDxdt
                                     , setCost
                                     , setDt
                                     , addOutput
                                     , getTimeStep
                                     , setPeriodic
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
  | any (`elem` "\"'~!@#$%^&*()+`-=[]{}\\|;:,.<>/?") name =
    error $ "ERROR: addOutput saw illegal octave variable character in string: \"" ++ name ++ "\""
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
                            zipWithM_ addOutput syms names
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
                             zipWithM_ addOutput syms names
                             return syms

addParam :: (Eq (Expr Z a), Hashable (Expr Z a)) => String -> State (Step a) (Expr Z a)
addParam name = do
  [blah] <- addParams [name]
  return blah

addConstant :: (Eq (Expr Z a), Hashable (Expr Z a)) => String -> State (Step a) (Expr Z a)
addConstant name = do
  [blah] <- addParams [name]
  return blah

addParams :: (Eq (Expr Z a), Hashable (Expr Z a)) => [String] -> State (Step a) [Expr Z a]
addParams names = do
  step  <- State.get
  let syms = map (sym . checkOctaveName) names
      params0 = stepParams step
  State.put $ step {stepParams = HS.union params0 (HS.fromList syms)}
  return syms

addConstants :: (Eq (Expr Z a), Hashable (Expr Z a)) => [String] -> State (Step a) [Expr Z a]
addConstants names = do
  step  <- State.get
  let syms = map (sym . checkOctaveName) names
      constants0 = stepConstants step
  State.put $ step {stepConstants = HS.union constants0 (HS.fromList syms)}
  return syms

addOutput :: Expr Z a -> String -> State (Step a) ()
addOutput var name = do
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

setPeriodic :: (Eq (Expr Z a), Hashable (Expr Z a)) => Expr Z a -> State (Step a) ()
setPeriodic var = do
  step <- State.get
  State.put $ step {stepPeriodic = HS.insert var (stepPeriodic step)}
  
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
setBound _ _ _ = do
  -- if execDxdt has put the x/u, they won't be symbolic - ignore them
  step <- State.get
  case stepStates step of
    Left _ -> error "WARNING - setBound called on non-design variable, use addConstraint instead"
    _ -> return ()


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
                                          , stepParams = HS.empty
                                          , stepConstants = HS.empty
                                          , stepIdx = k
                                          , stepOutputs = HM.empty
                                          , stepPeriodic = HS.empty
                                          }

execDxdt :: Num (Expr Z a) => State (Step a) b -> Int -> [Expr Z a] -> [Expr Z a] -> [Expr Z a]
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
                 , stepParams = HS.empty
                 , stepConstants = HS.empty
                 , stepIdx = k
                 , stepOutputs = HM.empty
                 , stepPeriodic = HS.empty
                 }
