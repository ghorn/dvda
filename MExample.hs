{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}

module Main where

import MutableDvda.MultipleShooting.MSCoctave
import MutableDvda.MultipleShooting.MSMonad
import MutableDvda.MultipleShooting.Types

--old then
--new
inputs: [x_0,v_0,x_1,v_1,u_0,u_1] :* [k,b]
inputs: [x_0,v_0,x_1,v_1,u_0,u_1] :* [b,k]

outputs: [-(x_1 - x_0 + 0.1 * v_0),-(v_1 - v_0 + 0.1 * (-k * x_0 - b * v_0 + u_0))] :* [[1.0,0.1,-1.0,0.0,0.0,0.0],[{ref:23},{ref:22},0.0,-1.0,0.1,0.0]]
outputs: [-(x_1 - x_0 + 0.1 * v_0),-(v_1 - v_0 + 0.1 * (-k * x_0 - b * v_0 + u_0))] :* [[1,0.1,-1,0,0,0],[-0.1 * k,-0.1 * b + 1,0,-1,0.1,0]]


outputs: [-(x_1 - x_0 + 0.1 * v_0),-(v_1 - v_0 + 0.1 * (-k * x_0 - b * v_0 + u_0))]
outputs: [-(x_1 - x_0 + 0.1 * v_0),-(v_1 - v_0 + 0.1 * (-k * x_0 - b * v_0 + u_0))]

outputs: [[1.0,0.1,-1.0,0.0,0.0,0.0],[{ref:23},    {ref:22},0.0,-1.0,0.1,0.0]]
outputs: [[1  ,0.1,-1  ,  0,  0,  0],[-0.1 * k,-0.1 * b + 1,  0,-1  ,0.1,  0]]

--import Dvda.MultipleShooting.MSCoctave
--import Dvda.MultipleShooting.MSMonad
--import Dvda.MultipleShooting.Types

--bg :: Floating a => a -> a
--bg x' = f''
--  where
--    y' = 2*x'
--    z' = 4*y'
----    x' = sym "x"
----    y' = sym "y"
----    z' = sym "z"
--    
--    f0 x y z = (z + x*y)*log(cos x / tanh y)**(z/exp y)
--    fx0 = f0 (f0 x' y' z') (f0 z' y' x') (f0 y' x' z')
--    fy0 = f0 (f0 z' x' y') (f0 x' z' y') (f0 z' z' y')
--    fz0 = f0 (f0 x' y' z') (f0 x' y' x') (f0 y' x' y')
--    f = f0 fx0 fy0 fz0
--    f' = f0 fy0 fz0 fx0
--    f'' = f0 f' f f'
--    f''' = f0 f'' f' f''
--    f'''' = f0 f''' f''' f'''
--    f''''' = f0 f'''' f'''' f''''
----    f = z' + x'*y'
--
----    fx = diff f x'
----    fy = diff f y'
----    fz = diff f z'
--
----f :: Expr Double
----f = x + x where x = sym "x"
--
--main :: IO ()
--main = do
----  (_, n, _) <- toGExprs [booboo]
--  let x = sym "x" :: Expr Double
--      g = bg x
--      g' = rad g [x]
--  fg <- toFunGraph x (g :* g') -- :(HM.elems radMap))
--  putStrLn $ "number of nodes: " ++ show (countNodes fg)
--
--  fg1 <- toFunGraph x (g :* g') -- :(HM.elems radMap))
--  putStrLn $ "number of nodes: " ++ show (countNodes fg1)
--
--  fg2 <- toFunGraph x (g :* g') -- :(HM.elems radMap))
--  putStrLn $ "number of nodes: " ++ show (countNodes fg2)
--
--  fg3 <- toFunGraph x (g :* g') -- :(HM.elems radMap))
--  putStrLn $ "number of nodes: " ++ show (countNodes fg3)
--
--  fg4 <- toFunGraph x (g :* g') -- :(HM.elems radMap))
--  putStrLn $ "number of nodes: " ++ show (countNodes fg4)
--
----  putStrLn "\ninputs:"
----  print (fgInputs fg)
----
----  putStrLn "\noutputs:"
----  print (fgOutputs fg)
--
--
--
--
--





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
  setBound x (0,0) (TIMESTEP (n'-1))

--  setPeriodic x

n' :: Int
n' = 2

main :: IO ()
--main = msCoctave spring simpsonsRuleError' n' "../Documents/MATLAB/" "spring"
main = msCoctave spring eulerError' n' "../Documents/MATLAB/" "spring"
