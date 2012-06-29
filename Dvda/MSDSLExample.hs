{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Dvda.MSDSLExample
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file dvda/LICENSE)
-- 
-- Maintainer  :  gregmainland@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Example multiple-shooting problem setup.
-----------------------------------------------------------------------------

module Dvda.MSDSLExample where

import Dvda.MSDSL
import Dvda.OctaveSyntax (showOctaveSource)

--------------------------------------------
nstep, stateSize, inputSize :: Int
stateCost, inputCost :: CostFun Double
costs :: Costs Double
parameterNames :: ParamNames
myODE :: ODE Double
myConstraints :: Constraints Double
run :: IO ()
--------------------------------------------

-- | Configure how many timesteps to optimize at, how many states
-- there are and how many inputs there are.
nstep = 2
stateSize = 2
inputSize = 1

-- | State cost function; penalize speeds and deviations from position
-- 0.
stateCost [x,v] = 2*x*x + 3*v*v
stateCost _ = error "goddammit"

-- | Input cost function; penalize input commands.
inputCost [u] = 7*u*u
inputCost _ = error "goddammit again"

-- | Join the costs together.
costs = Costs stateCost inputCost

-- | Label the parameters in the ODE
parameterNames = ["k", "b"]

-- | Specify the dynamics of the system we're trying to optimize
myODE xs us = [v, -k*x - b*v + u]
  where
    [x,v] = xs
    [u] = us
    k = 5
    b = 0.2

-- | No explicit constraints -- @makeSystem@ will take care of the
-- dynamics constraint for us.
myConstraints = Constraints [] []

-- | Go!
run = do 
  putStrLn $ showOctaveSource (makeSystem myODE nstep stateSize inputSize parameterNames costs myConstraints) "foo"
  print "yay"
