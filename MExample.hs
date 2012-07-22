{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}

module Main where

import Control.Monad ( liftM )

import MutableDvda.Expr
import MutableDvda.SharedVar
import MutableDvda.Utils

bg :: Floating a => a -> a
bg x' = f
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
--    f = z' + x'*y'

--    fx = diff f x'
--    fy = diff f y'
--    fz = diff f z'

booboo :: Expr Double
booboo = bg (sym "x")

main :: IO ()
main = do
--  (fullShow booboo) >>= putStrLn
  (toIO $ countNodes booboo) >>= print
--  putStrLn "--------------------------------------------------"
  bam <- toIO (backprop 1 booboo)
----  mapM (\(_,x) -> fullShow x) bam >>= mapM_ putStrLn
----  mapM_ putStrLn $ map (\(x,_) -> show x) bam
  toIO (fmap sum (mapM (\(_,x) -> countNodes x) bam)) >>= print
--  print $ length $ bam
