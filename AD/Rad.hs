-- Rad.hs

{-# OPTIONS_GHC -Wall #-}

module AD.Rad( diff
             , radExample
             ) where

import Expr

import Data.GraphViz

diff :: (Show a, Eq a, Num a) => Expr a -> Expr a
diff (Op2 op2Type x y) = op2DiffRule op2Type x y
diff (Source x) = Source $ sourceDiffRule x
diff (Elemwise elemwiseType x) = elemwiseDiffRule elemwiseType x

-- op2 differentiation rules
op2DiffRule :: Num a => Op2Type -> Expr a -> Expr a -> Expr a
op2DiffRule Mul x y = x*(diff y) + (diff x)*y
--op2DiffRule Div x y = (y*x' - x*y')/(y*y)
--  where
--    x' = diff x
--    y' = diff y
op2DiffRule Add x y = (diff x) + (diff y)
op2DiffRule Sub x y = (diff x) - (diff y)

-- source differentiation rules
sourceDiffRule :: (Show a, Eq a) => SourceType a -> SourceType a
sourceDiffRule (Sym name) = Sym ("d("++name++")")
sourceDiffRule (Number _) = Zero
sourceDiffRule (I _) = Zero
sourceDiffRule Zero = Zero

-- elementwise differentiation rules
elemwiseDiffRule :: Num a => ElemwiseType -> Expr a -> Expr a
elemwiseDiffRule Abs x = signum x
elemwiseDiffRule Sqr x = 2*x
elemwiseDiffRule Signum _ = Source Zero

  
radExample :: IO ()
radExample = do
  let exampleExpr :: Expr Integer
      exampleExpr = abs(y*34) + 5 + y
        where
          y = sym "y"
  print exampleExpr
  print $ diff exampleExpr
  preview $ exprToGr exampleExpr
  preview $ exprToGr $ diff exampleExpr
