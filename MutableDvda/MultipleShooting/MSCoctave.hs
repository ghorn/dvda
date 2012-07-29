{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}

module MutableDvda.MultipleShooting.MSCoctave ( msCoctave
                                              , run
                                              ) where

import qualified Data.HashSet as HS
import Data.List ( zipWith6, transpose, elemIndex )
import Data.Maybe ( catMaybes )

import qualified Dvda.HashMap as HM

import Dvda.Codegen ( writeSourceFile )
import MutableDvda.AD ( rad )
import MutableDvda.CGen -- ( showMex )
import MutableDvda.Expr ( Expr(..) )
import MutableDvda.FunGraph -- ( (:*)(..), previewGraph, toFunGraph )
import MutableDvda.MultipleShooting.MSMonad
import MutableDvda.MultipleShooting.Types

{-
    min f(x) st:
    
    c(x) <= 0
    ceq(x) == 0
    A*x <= b
    Aeq*x == beq
    lb <= x <= ub
-}
--type Integrator a = ([Expr a] -> [Expr a] -> [Expr a] -> [Expr a]
--                     -> ([Expr a] -> [Expr a] -> [Expr a])
--                     -> Expr a -> [Expr a])

type Integrator a = [Expr Double]
                   -> [Expr Double]
                   -> [Expr Double]
                   -> [Expr Double]
                   -> ([Expr Double]
                       -> [Expr Double] -> [Expr Double])
                   -> Expr Double
                   -> [Expr Double]

msCoctave
  :: (Double ~ a)
     => State (Step a) b
     -> Integrator a
     -> Int
     -> String
     -> FilePath
     -> IO ()
msCoctave userStep odeError n funDir name = do
  let steps = map (runOneStep userStep) [0..n-1]
      dts = map ((fromJustErr "dts error") . stepDt) steps
      
      fromLeft (Left x) = x
      fromLeft (Right _) = error "ERROR: fromLeft got Right"
      
      -- fromJust checked in runOneStep for states', actions', stateNames, actionNames
      states'  = map (fst . unzip . (fromJustErr "states' error") . fromLeft . stepStates ) steps
      actions' = map (fst . unzip . (fromJustErr "actions' error") . fromLeft . stepActions) steps
      stateNames  = map (snd . unzip . (fromJustErr "stateNames error") . fromLeft . stepStates ) steps
      actionNames = map (snd . unzip . (fromJustErr "actionNames error") . fromLeft . stepActions) steps
      
      -- ensure that state/action (names) are the same in all steps
      states = if all (head stateNames  ==) stateNames
               then states'
               else error "ERROR: different states in different timesteps"
      actions = if all (head actionNames  ==) actionNames
                then actions'
                else error "ERROR: different actions in different timesteps"
      
      params    = HS.toList $ foldr HS.union HS.empty (map stepParams    steps)
      constants = HS.toList $ foldr HS.union HS.empty (map stepConstants steps)
      
      boundMap = foldr HM.union HM.empty (map stepBounds steps)
      
      outputMap = foldl (HM.unionWith (++)) HM.empty (map stepOutputs steps)
      ------------------------------------------------------------------------------------
      cost = case catMaybes $ map stepCost steps of [] -> error "need to set cost function"
                                                    cs -> sum cs
      
      (ceq, cineq) = foldl f ([],[]) allConstraints
        where
          f (eqs,ineqs) (Constraint x EQ y) = (eqs ++ [x - y], ineqs)
          f (eqs,ineqs) (Constraint x LT y) = (eqs, ineqs ++ [x - y])
          f (eqs,ineqs) (Constraint x GT y) = (eqs, ineqs ++ [y - x])
      
          dodeConstraints = map (Constraint 0 EQ) $ concat $
                            zipWith6 odeError (init states) (init actions) (tail states) (tail actions)
                            (map (execDxdt userStep) [0..]) dts
      
          allConstraints = dodeConstraints ++ (concatMap stepConstraints steps) ++ periodicConstraints
      
          periodicConstraints
            | HS.size notXU > 0 = error $ "ERROR: can't set periodic constraints for non states/actions:" ++ show notXU
            | otherwise = foldl g' [] $ map f' (transpose states ++ transpose actions)
            where
              pcSets = map stepPeriodic steps
              dvSet = HS.fromList (concat states ++ concat actions)
              notXU = HS.difference (foldl HS.union HS.empty pcSets) dvSet
              pc0 = head pcSets
              pcf = last pcSets
      
              -- match up states/actions by making sure they're in the same state/action list
              f' xu = (HS.toList $ HS.filter (`elem` xu) pc0, HS.toList $ HS.filter (`elem` xu) pcf)
              g' acc ( [],   _) = acc
              g' acc (  _,  []) = acc
              g' acc ([x], [y]) = acc ++ [Constraint x EQ y]
              g'   _ (  _,   _) = error "ERROR: too many matching periodic constraints"
      
      -------------------------------------------------------------------------------------
      dvs = concat states ++ concat actions ++ params
      
  costSource <- do
    let costGrad = rad cost dvs
    showMex (name ++ "_cost") (dvs :* constants) (cost :* costGrad)
  
  constraintsSource <- do
    let cineqJacob = map (flip rad dvs) cineq
        ceqJacob   = map (flip rad dvs) ceq
    showMex (name ++ "_constraints") (dvs :* constants) (cineq :* ceq :* cineqJacob :* ceqJacob)
  
  timeSource <- showMex (name ++ "_time") (dvs :* constants) (init $ scanl (+) 0 dts)
  
  outputSource <- showMex (name ++ "_outputs") (dvs :* constants) (HM.elems outputMap)
  
  simSource <- do
    let x' = head states
        u' = head actions
        dxdt' = fromJustErr "dxdt' error" $ stepDxdt $ head steps
    showMex (name ++ "_sim") (x' :* u' :* params :* constants) dxdt'
      
  let (lbs, ubs, _) = unzip3 $ map getBnd dvs
        where
          getBnd dv = case HM.lookup dv boundMap of
            Nothing -> error $ "ERROR: please set bounds for " ++ show dv
            Just bnd -> bnd
      
      setupSource =
        unlines $
        [ "function [x0, Aineq, bineq, Aeq, beq, lb, ub] = "++ name ++"_setup()"
        , ""
--        , "x0 = " ++ show (vectorizeDvs dvsGuess) ++ "';"
        , "x0 = zeros(" ++ show (length dvs) ++ ",1);"
        , "Aineq = [];"
        , "bineq = [];"
        , "Aeq = [];"
        , "beq = [];"
        , "lb = " ++ show lbs ++ "';"
        , "ub = " ++ show ubs ++ "';"
        ]
      
      -- take vector of design variables and vector of constants and return nice matlab struct
      structSource =
        unlines $
        ["function ret = " ++ name ++ "_struct(designVars,constants)"
        , ""
        , "ret.time = " ++ name ++ "_time(designVars, constants);"
        , "outs = " ++ name ++ "_outputs(designVars, constants);"
        , concat $ zipWith (\name' k -> "ret." ++name'++ " = outs("++show k++",:);\n") (HM.keys outputMap) [(1::Int)..]
        ] ++
        toStruct dvs "designVars" (map show params) (map (\x -> [x]) params) ++
        toStruct constants "constants" (map show constants) (map (\x -> [x]) constants)
          where
            dvsToIdx dvs' = (fromJustErr "toStruct error") . (flip HM.lookup (HM.fromList (zip dvs' [(1::Int)..])))
      
            toStruct dvs' nm = zipWith (\name' vars -> "ret." ++ name' ++ " = " ++ nm ++ "(" ++ show (map (dvsToIdx dvs') vars) ++ ");\n")
  
  
      -- take nice matlab structs and return vectors of design variables and constants
      unstructSource =
        unlines $
        [ "function dvs = " ++ name ++ "_unstruct(dvStruct)\n"
        , "dvs = zeros(" ++ show (length dvs) ++ ", 1);"
        , ""
        , concatMap fromParam params
        , concat $ zipWith fromXUS (head  stateNames) (transpose states)
        , concat $ zipWith fromXUS (head actionNames) (transpose actions)
        ]
        where
          readName e = case e of
            ESym nm -> nm
            _ -> error "param not ESym"
          fromParam e = "dvs(" ++ show (1 + (fromJustErr "fromParam error" $ e `elemIndex` dvs)) ++ ") = dvStruct." ++ readName e ++ ";\n"
      
          fromXU nm e k =
            "dvs(" ++ show (1 + (fromJustErr "fromXU error" $ e `elemIndex` dvs)) ++ ") = dvStruct." ++ nm ++ "(" ++ show k ++ ");\n"
          fromXUS name' xs = (concat $ zipWith (fromXU name') xs [(1::Int)..]) ++ "\n"
      
      unstructConstsSource =
        unlines $
        [ "function constants = " ++ name ++ "_unstructConstants(constStruct)\n"
        , "constants = zeros(" ++ show (length constants) ++ ", 1);"
        , ""
        , concatMap fromConst constants
        ]
        where
          readName e = case e of
            ESym nm -> nm
            _ -> error "const not ESym"
          fromConst e = "constants(" ++ show (1 + (fromJustErr "fromConst error" $ e `elemIndex` constants)) ++ ") = constStruct." ++ readName e ++ ";\n"
      
      plotSource =
        unlines $
        [ "function " ++ name ++ "_plot(designVars, constants)\n"
        , "x = " ++ name ++ "_struct(designVars, constants);\n"
        , init $ unlines $ zipWith f (HM.keys outputMap) [(1::Int)..]
        ]
        where
          rows = ceiling $ sqrt $ (fromIntegral ::Int -> Double) $ HM.size outputMap
          cols = (HM.size outputMap `div` rows) + 1
          f name' k = unlines $
                      [ "subplot(" ++ show rows ++ "," ++ show cols ++ ","++show k++");"
                      , "plot( x.time, x." ++ name' ++ " );"
                      , "xlabel('time');"
                      , "ylabel('" ++ name'' ++ "');"
                      , "title('"  ++ name'' ++ "');"
                      ]
            where
              name'' = foldl (\acc x -> if x == '_' then acc ++ "\\_" else acc ++ [x]) "" name'

      mexAllSource = unlines $ map f ["cost", "constraints", "time", "outputs", "sim"]
        where
          f x = "disp('mexing " ++ file ++ "')\n"++"mex " ++ file
            where
              file = name ++ "_" ++ x ++ ".c"
  
  _ <- writeSourceFile           costSource funDir $ name ++ "_cost.c"
  _ <- writeSourceFile    constraintsSource funDir $ name ++ "_constraints.c"
  _ <- writeSourceFile           timeSource funDir $ name ++ "_time.c"
  _ <- writeSourceFile         outputSource funDir $ name ++ "_outputs.c"
  _ <- writeSourceFile            simSource funDir $ name ++ "_sim.c"
  
  _ <- writeSourceFile         mexAllSource funDir $ name ++ "_mex_all.m"
  _ <- writeSourceFile          setupSource funDir $ name ++ "_setup.m"
  _ <- writeSourceFile         structSource funDir $ name ++ "_struct.m"
  _ <- writeSourceFile unstructConstsSource funDir $ name ++ "_unstructConstants.m"
  _ <- writeSourceFile       unstructSource funDir $ name ++ "_unstruct.m"
  _ <- writeSourceFile           plotSource funDir $ name ++ "_plot.m"

  return ()
--  putStrLn $ "cost        " ++ showCollisions costFg
--  putStrLn $ "constraints " ++ showCollisions constraintsFg
--  putStrLn $ "time        " ++ showCollisions timeFg
--  putStrLn $ "output      " ++ showCollisions outputFg
--  putStrLn $ "sim         " ++ showCollisions simFg

fromJustErr :: String -> Maybe a -> a
fromJustErr _ (Just x) = x
fromJustErr message Nothing = error $ "fromJustErr got Nothing, message: \"" ++ message ++ "\""


spring :: State (Step Double) ()
spring = do
  [x, v] <- setStates ["x","v"]
  [u] <- setActions ["u"]
  [k, b] <- addConstants ["k", "b"]
  setDxdt [v, -k*x - b*v + u]
  setDt 0.1
  let cost = 2*x*x + 3*v*v + 10*u*u
--  let cost = 2*x*x -- + 3*v*v + 10*u*u
  setCost cost
  addOutput cost "cost"

  setBound x (5,5) (TIMESTEP 0)
  setBound v (0,0) (TIMESTEP 0)
  
  setBound x (-5,5) ALWAYS
  setBound v (-10,10) ALWAYS
  setBound u (-200, 100) ALWAYS

  setBound v (0,0) (TIMESTEP (n'-1))

  setPeriodic x

n' :: Int
n' = 30

run :: IO ()
--run = msCoctave spring simpsonsRuleError' n' "../Documents/MATLAB/" "spring"
run = msCoctave spring eulerError' n' "../Documents/MATLAB/" "spring"
