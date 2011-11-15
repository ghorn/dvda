-- Fad.hs

{-# OPTIONS_GHC -Wall #-}

module Had.AD.Fad( fadExample
                 ) where

import Had.Expr
import Had.Simplify(pruneZeros)

data Dual a = Dual a a deriving (Show, Eq)

instance (Num a) => Num (Dual a) where
  (Dual x x') * (Dual y y') = Dual (x * y) (x*y' + x'*y)
  (Dual x x') + (Dual y y') = Dual (x + y) (x' + y')
  (Dual x x') - (Dual y y') = Dual (x - y) (x' - y')
  abs (Dual x _) = Dual (abs x) (signum x)
  signum (Dual _ _) = Dual 0 0
  fromInteger x = Dual (fromInteger x) 0
  
fadExample :: IO ()
fadExample = do
  let exampleExpr :: Dual (Expr Integer)
      exampleExpr = y*34 + 5 + y*y
        where
          y = Dual (sym "y") 1
      f = (\(Dual x _) -> x) exampleExpr
      g = pruneZeros $ (\(Dual _ x) -> x) exampleExpr

  print exampleExpr
  print $ f
  print $ g
  previewGraph $ exprToGraph f
  previewGraph $ exprToGraph g
