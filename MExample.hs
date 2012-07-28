{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}

module Main where

import MutableDvda.AD
import MutableDvda.Expr
import MutableDvda.FunGraph
import MutableDvda.Graph

bg :: Floating a => a -> a
bg x' = f'''
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
--  print (bg x)
--  putStrLn "\nrunning rad..."
--  radMap <- rad f
--  sx <- fullShow (head $ HM.keys radMap)
--  sdx <- fullShow (head $ HM.elems radMap)
--  putStrLn $ "df/d" ++ sx ++ " = " ++ sdx
  putStrLn "\nmaking graph..."
  FunGraph gr ins outs <- toFunGraph x (g :* g') -- :(HM.elems radMap))

  putStrLn $ "\nlength graph:" ++ show (length gr)

  putStrLn "\ninputs:"
  print ins

  putStrLn "\noutputs:"
  print outs
