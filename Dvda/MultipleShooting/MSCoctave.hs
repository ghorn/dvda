{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}

module Dvda.MultipleShooting.MSCoctave ( msCoctave
                                       , run
                                       ) where

import qualified Data.HashSet as HS
import Data.List ( zipWith6, transpose, elemIndex )
import Data.Maybe ( fromJust, catMaybes )

import Dvda
import Dvda.Codegen ( writeSourceFile )
import Dvda.Expr ( Expr(..), Const(..), Sym(..) )
import qualified Dvda.HashMap as HM
import Dvda.MultipleShooting.MSMonad
import Dvda.MultipleShooting.Types
import Dvda.OctaveSyntax ( toOctaveSource )
import Dvda.SymMonad ( rad )

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
  _ <- writeSourceFile           costSource funDir $ name ++ "_cost.m"
  _ <- writeSourceFile    constraintsSource funDir $ name ++ "_constraints.m"
  _ <- writeSourceFile          setupSource funDir $ name ++ "_setup.m"
  _ <- writeSourceFile         structSource funDir $ name ++ "_struct.m"
  _ <- writeSourceFile unstructConstsSource funDir $ name ++ "_unstructConstants.m"
  _ <- writeSourceFile       unstructSource funDir $ name ++ "_unstruct.m"
  _ <- writeSourceFile           timeSource funDir $ name ++ "_time.m"
  _ <- writeSourceFile         outputSource funDir $ name ++ "_outputs.m"
  _ <- writeSourceFile           plotSource funDir $ name ++ "_plot.m"
  _ <- writeSourceFile            simSource funDir $ name ++ "_sim.m"
  putStrLn $ "cost        " ++ showCollisions costFg
  putStrLn $ "constraints " ++ showCollisions constraintsFg
  putStrLn $ "time        " ++ showCollisions timeFg
  putStrLn $ "output      " ++ showCollisions outputFg
  putStrLn $ "sim         " ++ showCollisions simFg
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

        dodeConstraints = map (Constraint (EConst (CSingleton Z 0)) EQ) $ concat $
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

    timeFg = runFunGraph $ do
      inputs_ (dvs :* constants)
      outputs_ $ init $ scanl (+) (EConst (CSingleton Z 0)) dts

    outputFg = runFunGraph $ do
      inputs_ (dvs :* constants)
      outputs_ $ HM.elems outputMap

    simFg = runFunGraph $ do
      let x' = head states
          u' = head actions
          dxdt' = fromJust $ stepDxdt $ head steps
      inputs_ (x' :* u' :* constants)
      outputs_ dxdt'

    costSource        = toOctaveSource        costFg (name ++ "_cost")
    constraintsSource = toOctaveSource constraintsFg (name ++ "_constraints")
    outputSource      = toOctaveSource      outputFg (name ++ "_outputs")
    timeSource        = toOctaveSource        timeFg (name ++ "_time")
    simSource         = toOctaveSource         simFg (name ++ "_sim")

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
          dvsToIdx dvs' = fromJust . flip HM.lookup (HM.fromList (zip dvs' [(1::Int)..]))
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
        fromParam e@(ESym _ (Sym nm)) =
          "dvs(" ++ show (1 + (fromJust $ e `elemIndex` dvs)) ++ ") = dvStruct." ++ nm ++ ";\n"
        fromParam _ = error "param not ESym"

        fromXU nm e k =
          "dvs(" ++ show (1 + (fromJust $ e `elemIndex` dvs)) ++ ") = dvStruct." ++ nm ++ "(" ++ show k ++ ");\n"
        fromXUS name' xs = (concat $ zipWith (fromXU name') xs [(1::Int)..]) ++ "\n"

    unstructConstsSource =
      unlines $
      [ "function constants = " ++ name ++ "_unstructConstants(constStruct)\n"
      , "constants = zeros(" ++ show (length constants) ++ ", 1);"
      , ""
      , concatMap fromConst constants
      ]
      where
        fromConst e@(ESym _ (Sym nm)) =
          "constants(" ++ show (1 + (fromJust $ e `elemIndex` constants)) ++ ") = constStruct." ++ nm ++ ";\n"
        fromConst _ = error "const not ESym"

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


spring :: State (Step Double) ()
spring = do
  [x, v] <- setStates ["x","v"]
  [u] <- setActions ["u"]
  [k, b] <- addConstants ["k", "b"]
  setDxdt [v, -k*x - b*v + u]
  setDt 0.1
  let cost = 2*x*x + 3*v*v + 10*u*u
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
n' = 20

run :: IO ()
run = msCoctave spring simpsonsRuleError' n' "../Documents/MATLAB/" "cartpole"
--run = msCoctave spring eulerError' n' "../Documents/MATLAB/" "cartpole"
