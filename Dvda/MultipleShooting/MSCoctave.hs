{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}

module Dvda.MultipleShooting.MSCoctave ( msCoctave
                                       , run
                                       ) where

import Data.Array.Repa ( Z(..) )
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List ( transpose )
import Data.List ( zipWith6 )
import Data.Maybe ( fromJust, catMaybes )
import Control.Monad.State ( State )

import Dvda
import Dvda.Expr ( Expr(..), Const(..) )
import Dvda.SymMonad ( rad )
--import Dvda.SparseLA ( SparseVec )
import Dvda.MultipleShooting.MSMonad
import Dvda.MultipleShooting.Types
import Dvda.OctaveSyntax ( toOctaveSource )
import Dvda.Codegen ( writeSourceFile )

{-
    min f(x) st:
    
    c(x) <= 0
    ceq(x) == 0
    A*x <= b
    Aeq*x == beq
    lb <= x <= ub
-}
--type Integrator a = ([Expr Z a] -> [Expr Z a] -> [Expr Z a] -> [Expr Z a]
--                     -> ([Expr Z a] -> [Expr Z a] -> [Expr Z a])
--                     -> Expr Z a -> [Expr Z a])

type Integrator a = [Expr Z Double]
                   -> [Expr Z Double]
                   -> [Expr Z Double]
                   -> [Expr Z Double]
                   -> ([Expr Z Double]
                       -> [Expr Z Double] -> [Expr Z Double])
                   -> Expr Z Double
                   -> [Expr Z Double]

msCoctave
  :: (Double ~ a)
     => State (Step a) b
     -> Integrator a
     -> Int
     -> String
     -> FilePath
     -> IO ()
msCoctave userStep odeError n funDir name = do
  _ <- writeSourceFile        costSource funDir (name ++ "_cost.m")
  _ <- writeSourceFile constraintsSource funDir (name ++ "_constraints.m")
  _ <- writeSourceFile       setupSource funDir (name ++ "_setup.m")
  _ <- writeSourceFile      structSource funDir (name ++ "_struct.m")
  return ()
  where
    steps = map (runOneStep userStep) [0..n-1]
    dts = map (fromJust . stepDt) steps
    
    fromLeft (Left x) = x
    fromLeft (Right _) = error "ERROR: fromLeft got Right"
    states'  = map (fst . unzip . fromJust . fromLeft . stepStates ) steps -- fromJust checked in runOneStep
    actions' = map (fst . unzip . fromJust . fromLeft . stepActions) steps -- fromJust checked in runOneStep

    stateNames  = map (snd . unzip . fromJust . fromLeft . stepStates ) steps  -- fromJust checked in runOneStep
    actionNames = map (snd . unzip . fromJust . fromLeft . stepActions) steps  -- fromJust checked in runOneStep    
    
    -- ensure that state/action (names) are the same in all steps
    states = if all (head stateNames  ==) stateNames
             then states'
             else error "ERROR: different states in different timesteps"
    actions = if all (head actionNames  ==) actionNames
              then actions'
              else error "ERROR: different actions in different timesteps"

    g Nothing hs = hs
    g (Just next) hs = HS.union hs next
    params    = HS.toList $ foldr g HS.empty (map stepParams    steps)
    constants = HS.toList $ foldr g HS.empty (map stepConstants steps)

    boundMap = foldr HM.union HM.empty (map stepBounds steps)
    ------------------------------------------------------------------------------------
    cost = case catMaybes $ map stepCost steps of [] -> error "need to set cost function"
                                                  cs -> sum cs

    (ceq, cineq) = foldl f ([],[]) allConstraints
      where
        f (eqs,ineqs) (Constraint x EQ y) = (eqs ++ [x - y], ineqs)
        f (eqs,ineqs) (Constraint x LT y) = (eqs, ineqs ++ [x - y])
        f (eqs,ineqs) (Constraint x GT y) = (eqs, ineqs ++ [y - x])

        dodeConstraints = map (Constraint (EConst (CSingleton Z 0)) EQ) $ concat $
                          zipWith6 odeError (init states) (init actions) (tail states) (tail actions)
                          (map (execDxdt userStep) [0..]) dts
    
        allConstraints = dodeConstraints ++ (concatMap stepConstraints steps)


    -------------------------------------------------------------------------------------
    dvs = concat states ++ concat actions ++ params

    costFg = runFunGraph $ do
      cost' <- node cost
      costGrad <- rad cost' dvs
      inputs_ (dvs :* constants)
      outputs_ (cost' :* costGrad)

    constraintsFg = runFunGraph $ do
       cineqJacob <- mapM (flip rad dvs) cineq
       ceqJacob   <- mapM (flip rad dvs) ceq
       inputs_ (dvs :* constants)
       outputs_ (cineq :* ceq :* cineqJacob :* ceqJacob)

    costSource        = toOctaveSource costFg        (name ++ "_cost")
    constraintsSource = toOctaveSource constraintsFg (name ++ "_constraints")

    (lbs, ubs, _) = unzip3 $ map getBnd dvs
      where
        getBnd dv = case HM.lookup dv boundMap of
          Nothing -> error $ "please set bounds for " ++ show dv
          Just bnd -> bnd

    setupSource =
      unlines $
      [ "function [x0, Aineq, bineq, Aeq, beq, lb, ub] = "++ name ++"_setup()"
      , ""
--      , "x0 = " ++ show (vectorizeDvs dvsGuess) ++ "';"
      , "x0 = zeros(" ++ show (length dvs) ++ ",1);"
      , "Aineq = [];"
      , "bineq = [];"
      , "Aeq = [];"
      , "beq = [];"
      , "lb = " ++ show lbs ++ "';"
      , "ub = " ++ show ubs ++ "';"
      ]


    dvsToIdx = fromJust . flip HM.lookup (HM.fromList (zip dvs [(1::Int)..]))
    toStruct = zipWith (\name' vars -> "ret." ++ name' ++ " = designVars(" ++ show (map dvsToIdx vars) ++ ");\n")

    structSource =
      unlines $
      ["function ret = " ++ name ++ "_struct(designVars,constants)"
      , "" ]
      ++ toStruct (head stateNames) (transpose states)
      ++ toStruct (head actionNames) (transpose actions)
      ++ toStruct (map show params) (map (\x -> [x]) params)

spring :: State (Step Double) ()
spring = do
  [x, v] <- setStates ["x","v"]
  [u] <- setActions ["u"]
  [k, b] <- setConstants ["k", "b"]
  setDxdt [v, -k*x - b*v + u]
  setDt 0.1
  setCost (2*x*x + 3*v*v + 10*u*u)

  setBound x (5,5) (TIMESTEP 0)
  setBound v (0,0) (TIMESTEP 0)
  
  setBound x (-5,5) ALWAYS
  setBound v (-10,10) ALWAYS
  setBound u (-200, 100) ALWAYS

  setBound x (0,0) (TIMESTEP (n'-1))
  setBound v (0,0) (TIMESTEP (n'-1))
  

n' :: Int
n' = 20

run :: IO ()
run = msCoctave spring eulerError' n' "../Documents/MATLAB/" "cartpole"
