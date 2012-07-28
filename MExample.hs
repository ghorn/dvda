{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}

module Main where

import MutableDvda.AD
import MutableDvda.Expr
import MutableDvda.FunGraph

bg :: Floating a => a -> a
bg x' = f''
  where
    y' = 2*x'
    z' = 4*y'
--    x' = sym "x"
--    y' = sym "y"
--    z' = sym "z"
    
    f0 x y z = (z + x*y)*log(cos x / tanh y)**(z/exp y)
    fx0 = f0 (f0 x' y' z') (f0 z' y' x') (f0 y' x' z')
    fy0 = f0 (f0 z' x' y') (f0 x' z' y') (f0 z' z' y')
    fz0 = f0 (f0 x' y' z') (f0 x' y' x') (f0 y' x' y')
    f = f0 fx0 fy0 fz0
    f' = f0 fy0 fz0 fx0
    f'' = f0 f' f f'
    f''' = f0 f'' f' f''
    f'''' = f0 f''' f''' f'''
    f''''' = f0 f'''' f'''' f''''
--    f = z' + x'*y'

--    fx = diff f x'
--    fy = diff f y'
--    fz = diff f z'

--f :: Expr Double
--f = x + x where x = sym "x"

main :: IO ()
main = do
--  (_, n, _) <- toGExprs [booboo]
  let x = sym "x" :: Expr Double
      g = bg x
      g' = rad g [x]
  putStrLn "making graph..."
  fg <- toFunGraph x (g :* g') -- :(HM.elems radMap))

  putStrLn "\ninputs:"
  print (fgInputs fg)

  putStrLn "\noutputs:"
  print (fgOutputs fg)

  putStrLn $ "\nnumber of nodes: " ++ show (countNodes fg)
