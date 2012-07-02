{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Dvda.MSCoctave ( writeOctaveSolver
                      ) where

import Text.Printf ( printf )
import Debug.Trace ( trace )
import qualified Data.IntMap as IM

import Dvda
import Dvda.SymMonad ( rad )
import Dvda.SparseLA ( svCats, sparseListFromSv )
import Dvda.MultipleShooting
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

writeOctaveSolver :: MultipleShooting Double -> [Bound Double] -> DesignVars Double -> [Constraint Double]
                     -> FilePath -> String -> IO ()
writeOctaveSolver ms bounds' dvsGuess moreConstraints funDir name = do
  let isEqConstraint (Constraint ConstraintEq _ _) = True
      isEqConstraint _ = False

      allConstraints = msDodeConstraints ms ++ moreConstraints

      vectorize = (\(x,y) -> (sparseListFromSv $ svCats x, sparseListFromSv $ svCats y)) . unzip . map (\(Constraint _ expr cnst) -> (expr, cnst))
      (ceq', beq') = vectorize $ filter isEqConstraint allConstraints
      (cineq', bineq') = vectorize $ filter (not . isEqConstraint) allConstraints
      ceq   = zipWith (-) ceq'   beq'
      cineq = zipWith (-) cineq' bineq'

      dvs = vectorizeDvs $ msDesignVars ms
      runtimeConstants = msRuntimeConstants ms

      costFg = runFunGraph $ do
        cost <- node (msObjFun ms)
        costGrad <- rad cost dvs
        inputs_ (dvs :* runtimeConstants)
        outputs_ (cost :* costGrad)

      constraintsFg = runFunGraph $ do
         cineqJacob <- mapM (flip rad dvs) cineq
         ceqJacob   <- mapM (flip rad dvs) ceq
         inputs_ (dvs :* runtimeConstants)
         outputs_ (cineq :* ceq :* cineqJacob :* ceqJacob)

      costSource        = toOctaveSource costFg        (name ++ "_cost")
      constraintsSource = toOctaveSource constraintsFg (name ++ "_constraints")

      defaultBounds = IM.fromList [(k, (-1e6, 1e6)) | k <- [0..numDvs ms - 1]]
      bounds = IM.union (IM.fromListWithKey resolve $ map toTuple bounds') defaultBounds
        where
          toTuple (Bound {var = v, lbound = lb, ubound = ub}) = (dvIdx ms v, (lb, ub))
          resolve k x@(lbx,ubx) y@(lby,uby) = trace msg ret
            where
              ret = (max lbx lby, min ubx uby)
              msg = printf "there are two bounds for \"%s\" (index %d): %s and %s, using %s"
                    (show (dvs !! k)) k (show x) (show y) (show ret)

      (lbs, ubs) = unzip $ map snd $ IM.assocs bounds
      
      setupSource =
        init $ unlines $
        [ "function [x0, Aineq, bineq, Aeq, beq, lb, ub] = "++ name ++"_setup()"
        , ""
        , "x0 = " ++ show (vectorizeDvs dvsGuess) ++ "';"
        , "Aineq = [];"
        , "bineq = [];"
        , "Aeq = [];"
        , "beq = [];"
--        , "lb = -1e6*ones(" ++ show (svSize x0) ++ ", 1);"
--        , "ub =  1e6*ones(" ++ show (svSize x0) ++ ", 1);"
        , "lb = " ++ show lbs ++ "';"
        , "ub = " ++ show ubs ++ "';"
        ]

  _ <- writeSourceFile        costSource funDir (name ++ "_cost.m")
  _ <- writeSourceFile constraintsSource funDir (name ++ "_constraints.m")
  _ <- writeSourceFile       setupSource funDir (name ++ "_setup.m")
  return ()





----  solver <- createSolver IpoptExactHessian (designVars ms) (objFun ms) (svCats $ map expression allConstraints)
--  solver <- createSolver Snopt (designVars ms) (objFun ms) (svCats $ map expression allConstraints)
--  
--  let solve :: [Bound] -> SparseVec Double -> IO (SparseVec Double, Double)
--      solve bounds xGuess = solveNlp solver xGuess (svFromList boxLbs, svFromList boxUbs) (nlLbs, nlUbs)
--        where
--          nlLbs = svCats $ map lcon allConstraints
--          nlUbs = svCats $ map ucon allConstraints
--          (boxLbs, boxUbs) = unzip $ map f [0..rows (designVars ms)-1]
--          f idx
--            | isNothing bnd = (-1e50, 1e50)
--            | otherwise     = (lbound (fromJust bnd), ubound (fromJust bnd))
--              where
--                bnd = find (\x -> idx == dvIdx x) bounds
--
--  return solve
