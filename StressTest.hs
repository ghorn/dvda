module Main where

import Numeric.Dvda

main :: IO ()
main = do
  let exampleExpr :: Expr Double
      exampleExpr = g (g x y z) (g z y x) (g z x y)
      g a b c = f (f a b c) (f b a c) (f c a b)
      f a b c = (c + a*b)*log(cos a / tanh b) -- **(c/exp b)
      
      x = sym "x"
      y = sym "y"
      z = sym "z"
      args = [x,y,z]

      dee = rad exampleExpr args
      sdee = show dee
  
  print exampleExpr
  (length sdee) `seq` putStrLn sdee

