{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Dvda.CartpoleMs ( run
                       ) where

import Dvda
import Dvda.MultipleShooting
import Dvda.SparseLA
import Dvda.MSCoctave

cartpoleLength :: Expr Z Double
cartpoleLength = 2.2

cartpoleDxdt :: SparseVec (Expr Z Double) -> SparseVec (Expr Z Double) -> SparseVec (Expr Z Double)
cartpoleDxdt state action = state'
  where
    [_, x', theta, theta'] = denseListFromSv state
    [u] = denseListFromSv action
    
    -- constants
    g = 9.8;
    len = cartpoleLength
    mc = 2;
    mp = 1;

    x'' = 1/(mc+mp*sin(theta)*sin(theta))*(u+mp*sin(theta)*(len*theta'*theta'+g*cos(theta)))
    theta'' = 1/(len*(mc+mp*sin(theta)*sin(theta)))*(-u*cos(theta) - mp*len*theta'*theta'*cos(theta)*sin(theta) - (mc+mp)*g*sin(theta));

    state' = svFromList [x', x'', theta', theta'']


-- ode
cartpoleOde :: Ode Double
cartpoleOde = Ode cartpoleDxdt (4,1)

---- cost fcn
cpCost :: Cost Double
cpCost = Cost cpCost' (4,1)

cpCost' :: SparseVec (Expr Z Double) -> SparseVec (Expr Z Double) -> Expr Z Double
cpCost' state action = 10*x*x + x'*x' + 100*cos(theta) + theta'*theta' + 0.001*u*u
  where
    [x, x', theta, theta'] = denseListFromSv state
    [u] = denseListFromSv action


run :: IO ()
run = do
  let n = 30 :: Int

      tEnd =  sym "tEnd"
      dt = tEnd/(fromIntegral n - 1 )

      sys = simpleSystem cartpoleOde cpCost dt n

      x0 = svFromList [-10,0,0.01,0::Double]
      xf = svFromList [0,0,pi,0]
      xBounds = [(-10,10), (-50,50), (-4*pi,4*pi), (-20*pi, 20*pi)]

      ms = multipleShooting sys [tEnd] [] eulerError

      xGuess = interpolateInitialGuess x0 xf n
      uGuess = interpolateInitialGuess x0 xf n
      paramsGuess = [5]
      dvsGuess = DesignVars { dvStates = xGuess, dvActions = uGuess, dvParams = paramsGuess }

      states = dvStates $ msDesignVars ms
      actions = dvActions $ msDesignVars ms
      
      x0sym = head $ states
      xfsym = last $ states
      stateBounds  = concat $ map (\x -> boundIntervals x xBounds   ) states
      actionBounds = concat $ map (\u -> boundIntervals u [(-10,10)]) actions
      bounds = concat [ boundEqs x0sym x0
                      , boundEqs xfsym xf
                      , stateBounds
                      , actionBounds
                      , [boundInterval tEnd (1, 5)]
--                      , [boundEq tEnd (simDt * fromIntegral (n-1))]
                      ]


  writeOctaveSolver ms bounds dvsGuess [] "../Documents/MATLAB/" "cartpole"
  print "bye"
