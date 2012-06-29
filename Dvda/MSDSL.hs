{-# OPTIONS_GHC -Wall #-}
{-# Language TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Dvda.MSDSL
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file dvda/LICENSE)
-- 
-- Maintainer  :  gregmainland@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Boilerplate reduction for multiple-shooting problem setup.
-----------------------------------------------------------------------------

module Dvda.MSDSL (
                   -- * Useful types
                  ODESystem(..)
                  , ParamNames
                  , ODE
                  , CostFun
                  , Costs(..)
                  , Constraints(..)
                   -- * Building a multiple shooting problem
                  , makeSystem
                  ) where
                  
import qualified Data.IntMap as IM
import Data.Array.Repa () -- Z(..) )

import Dvda
import Dvda.SparseLA
import Dvda.MS (dynamicsErrorsEuler, msProblem)
import Dvda.SymMonad ( KeyT )

-- | An initial value problem, ready for optimization
data ODESystem a = ODESystem { paramList :: [Expr Z a]
                             , dVars :: [Expr Z a]
                             , constraints :: Constraints a
                             , cost :: Expr Z a
                             }

-- | Type alias for labeling ODE parameters
type ParamNames = [String]
-- | Type alias for an ODE dynamics function (@x'(t) = f(x, t, u)@)
type ODE a = [Expr Z a] -> [Expr Z a] -> [Expr Z a]
-- | Type alias for a cost function; it takes in a vector of states
-- and emits a scalar cost value.
type CostFun a = [Expr Z a] -> Expr Z a

-- | A system has two possible costs, based on the state and the
-- action.
data Costs a = Costs { stateCost' :: CostFun a
                     , actionCost' :: CostFun a
                     }

-- | Type alias for a constraint function; it gives an equality or
-- inequality.
type ConstraintFun a = Expr Z a

-- | The optimization problem may be constrained by equality
-- constraints and inequality constraints.
data Constraints a = Constraints { ceq :: [ConstraintFun a]
                                 , cineq :: [ConstraintFun a]
                                 }

-- | Construct a cost functional from a set of state/action vectors
-- and the corresponding cost functions.
costFun :: [[[Expr Z Double]]] -> [CostFun Double] -> Expr Z Double
costFun ls fs = sum $ zipWith cm ls fs
    where cm xs f = sum (map f xs)

-- | Utility function to turn a @SparseMat@ into a list of @SparseVec@s of
-- scalar @Expr@s.
vecsOfMat :: Int -> SparseMat (Expr Z Double) -> [SparseVec (Expr Z Double)]
vecsOfMat nstep m = map (`getCol` m) [0..nstep-1]

-- | Utility function to turn a list of @SparseVec@s into nested lists
-- of scalar @Expr@s.
listsOfVecs :: [SparseVec (Expr Z Double)] -> [[Expr Z Double]]
listsOfVecs = map etl
    where etl (SparseVec _ xs) = IM.elems xs

-- | @mkSyms@ links a dynamics function, some size specifications and
-- costs and constraints into an @ODESystem@.  Call it thus: @mkSyms
-- dynamics nsteps nstates ninputs paramnames costs constraints@.
-- @dynamics@ is the dynamics function.  The @n@ arguments are how
-- many time steps, states and inputs you want in the problem.
-- @paramnames@ labels the parameters in the ODE over which to
-- optimize.  @costs@ and @constraints@ are self-explanatory.
mkSyms :: ODE Double -> Int -> Int -> Int 
       -> ParamNames -> Costs Double -> Constraints Double -> ODESystem Double 
mkSyms ode nstep ns ni pn (Costs sc ac) (Constraints ceq_ cineq_) = ODESystem pl dvars (Constraints (ceq_ ++ ceqdyn) cineq_) ccost
    where stateMat = smat "x" (ns, nstep)
          actionMat = smat "u" (ni, nstep)
          t = sym "time"
          ts = t / (fromIntegral nstep - 1)
          pl = map sym pn
          sv = vecsOfMat nstep stateMat
          sl = listsOfVecs sv
          av = vecsOfMat nstep actionMat
          al = listsOfVecs av
          ccost = costFun [sl, al] [sc, ac]
          dvars = t : (concat sl ++ concat al)
          ceqdyn = concat $ listsOfVecs $ dynamicsErrorsEuler sv av ode ts

-- | @problem'@ turns the big pile of @Expr@s that makes up an
-- @ODESystem@ into a DVDA @FunGraph@ for use in code generation.
problem' :: ODESystem Double -> FunGraph Double (KeyT ([Expr Z Double] :* [Expr Z Double]))
           (KeyT (Expr Z Double :* [Expr Z Double] :* [Expr Z Double] :* [[Expr Z Double]] :* [Expr Z Double] :* [[Expr Z Double]]))
problem' (ODESystem pl dv (Constraints ceq_ cineq_) cost_) = msProblem ceq_ cineq_ cost_ dv pl

-- | @makeSystem@ combines @problem'@ and @mkSyms@.
makeSystem :: ODE Double -> Int -> Int -> Int
           -> ParamNames -> Costs Double -> Constraints Double
           -> FunGraph Double (([Expr Z Double], [Int]) :* ([Expr Z Double], [Int])) ((Expr Z Double, Int) :* (([Expr Z Double], [Int]) :* (([Expr Z Double], [Int]) :* (([[Expr Z Double]], [[Int]]) :* (([Expr Z Double], [Int]) :* ([[Expr Z Double]], [[Int]]))))))
makeSystem o st ss is pn co cn = problem' $ mkSyms o st ss is pn co cn
