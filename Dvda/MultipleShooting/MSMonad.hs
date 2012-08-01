{-# OPTIONS_GHC -Wall #-}

module Dvda.MultipleShooting.MSMonad ( State
                                     , setStates
                                     , setActions
                                     , addParam
                                     , addParams
                                     , addConstant
                                     , addConstants
                                     , setDxdt
                                     , setLagrangeTerm
                                     , setMayerTerm
                                     , setDt
                                     , addOutput
                                     , setPeriodic
                                     , addConstraint
                                     , setBound
                                     , lagrangeStateName
                                     , lagrangeTermName
                                     ) where

import Data.Hashable ( Hashable )
import qualified Data.HashSet as HS
import Data.List ( nub, sort )
import Data.Maybe ( isJust, fromMaybe )
import Data.Monoid ( mappend )
import Control.Monad ( when, zipWithM_ )
import Control.Monad.State ( State )
import qualified Control.Monad.State as State

import qualified Dvda.HashMap as HM

import Dvda.Expr ( Expr(..), sym )
import Dvda.MultipleShooting.Types

lagrangeStateName,lagrangeTermName :: String
lagrangeStateName = "lagrangeState"
lagrangeTermName = "lagrangeTerm"

failDuplicates :: [String] -> [String]
failDuplicates names
  | length names == length (nub names) = names
  | otherwise = error $ "ERROR: saw duplicate names in: " ++ show (sort names)

checkOctaveName :: String -> String
checkOctaveName name
  | any (`elem` badChars) name =
    error $ "ERROR: saw illegal octave variable character in string: \"" ++ name ++
    "\", illegal characters: " ++ badChars
  | name == lagrangeStateName = error "don't call your variable \"" ++ lagrangeStateName ++ "\", it's reserved"
  | name == lagrangeTermName = error "don't call your variable \"" ++ lagrangeTermName ++ "\", it's reserved"
  | otherwise = name
  where
    badChars = "\"'~!@#$%^&*()+`-=[]{}\\|;:,.<>/?"

setStates :: [String] -> State (Step a) [Expr a]
setStates names' = do
  step <- State.get
  case stepStates step of Just _ -> error "states already set, don't call setStates twice"
                          Nothing -> do
                            let names = failDuplicates (map checkOctaveName names')
                                syms = map sym (failDuplicates names)
                            State.put $ step {stepStates = Just syms}
                            zipWithM_ addOutput syms names
                            return syms

setActions :: [String] -> State (Step a) [Expr a]
setActions names' = do
  step <- State.get
  case stepActions step of Just _ -> error "actions already set, don't call setActions twice"
                           Nothing -> do
                             let names = failDuplicates (map checkOctaveName names')
                                 syms = map sym (failDuplicates names)
                             State.put $ step {stepActions = Just syms}
                             zipWithM_ addOutput syms names
                             return syms

addParam :: (Eq a, Hashable a) => String -> State (Step a) (Expr a)
addParam name = do
  [blah] <- addParams [name]
  return blah

addConstant :: (Eq a, Hashable a) => String -> State (Step a) (Expr a)
addConstant name = do
  [blah] <- addConstants [name]
  return blah

addParams :: (Eq a, Hashable a) => [String] -> State (Step a) [Expr a]
addParams names = do
  step  <- State.get
  let syms = map (sym . checkOctaveName) names
      params0 = stepParams step
  State.put $ step {stepParams = HS.union params0 (HS.fromList syms)}
  return syms

addConstants :: (Eq a, Hashable a) => [String] -> State (Step a) [Expr a]
addConstants names = do
  step  <- State.get
  let syms = map (sym . checkOctaveName) names
      constants0 = stepConstants step
  State.put $ step {stepConstants = HS.union constants0 (HS.fromList syms)}
  return syms

addOutput :: Expr a -> String -> State (Step a) ()
addOutput var name = do
  step <- State.get
  let hm = stepOutputs step
      err = error $ "ERROR: already have an output with name: \"" ++ name ++ "\""
  State.put $ step {stepOutputs = HM.insertWith err (checkOctaveName name) var hm}

setDt :: Expr a -> State (Step a) ()
setDt expr = do
  step  <- State.get
  when (isJust (stepDt step)) $ error "dt already set, don't call setDt twice"
  State.put $ step {stepDt = Just expr}

setPeriodic :: (Eq a, Hashable a, Show a) => Expr a -> State (Step a) ()
setPeriodic var = do
  step <- State.get
  let newPeriodic
        | var `HS.member` (stepPeriodic step) = error $ "you called setPeriodic twice on \"" ++ show var ++ "\""
        | not (var `elem` (fromMaybe [] (mappend (stepStates step) (stepActions step)))) =
          error $ "you can only make states or actions periodic, you can't make \"" ++ show var ++ "\" periodic"
        | otherwise = HS.insert var (stepPeriodic step)
  State.put $ step {stepPeriodic = newPeriodic}
-------------------------------------------

setDxdt :: [Expr a] -> State (Step a) ()
setDxdt vars = do
  step  <- State.get
  when (isJust (stepDxdt step)) $ error "dxdt already set, don't call setDxdt twice"
  State.put $ step {stepDxdt = Just vars}

setLagrangeTerm :: Expr a -> (a,a) -> State (Step a) ()
setLagrangeTerm var (lb,ub) = do
  step  <- State.get
  when (isJust (stepLagrangeTerm step)) $ error "Lagrange term already set, don't call setLagrangeTerm twice"
  State.put $ step {stepLagrangeTerm = Just (var,(lb,ub))}

setMayerTerm :: Expr a -> State (Step a) ()
setMayerTerm var = do
  step  <- State.get
  when (isJust (stepMayerTerm step)) $ error "Mayer term already set, don't call setMayerTerm twice"
  State.put $ step {stepMayerTerm = Just var}

setBound :: (Show a, Eq a, Hashable a)
            => Expr a -> (a, a) -> BCTime -> State (Step a) ()
setBound var@(ESym _) (lb, ub) bctime = do
  step <- State.get
  State.put $ step {stepBounds = (var, (lb,ub,bctime)):(stepBounds step)}
setBound _ _ _ = error "WARNING - setBound called on non-design variable, try addConstraint instead"

addConstraint :: Expr a -> Ordering -> Expr a -> State (Step a) ()
addConstraint x ordering y =
  State.state (\step -> ((), step {stepConstraints = (stepConstraints step) ++ [Constraint x ordering y]}))
