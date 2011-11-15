-- Rad.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.AD.Rad( rad
                          , getSensitivities
                          ) where

import Numeric.Dvda.Expr.Expr
import Numeric.Dvda.Expr.Op2Type
import Numeric.Dvda.Expr.ElemwiseType
import Numeric.Dvda.Expr.SourceType
import Numeric.Dvda.Expr.ExprToGraph
import Numeric.Dvda.Simplify
import Numeric.Dvda.AD.Fad

rad :: Floating a => Expr a -> [Expr a] -> [Expr a]
rad expr args = map getXSens args
  where
    senss = getSensitivities expr 1
    getXSens x = fastSimplify $ sum $ map snd $ filter (\y -> x == fst y) senss


pert :: Dual a -> a
pert (Dual _ b) = b


getSensitivities :: Floating a => Expr a -> Expr a -> [(Expr a, Expr a)]
getSensitivities primal@(Source (Sym _)) sens = [(primal, fastSimplify sens)]
getSensitivities (Source _) _ = []
getSensitivities (Op2 op2t x y) sens = (getSensitivities x (sens*dfdx))++
                                       (getSensitivities y (sens*dfdy))
  where
    f = applyOp2 op2t
    dfdx = pert $ f (Dual x 1) (Dual y 0)
    dfdy = pert $ f (Dual x 0) (Dual y 1)
getSensitivities (Elemwise ewt x) sens = getSensitivities x (sens*dfdx)
  where
    f = applyElemwise ewt
    dfdx = pert $ f (Dual x 1)

  
radExample :: IO ()
radExample = do
  let exampleExpr :: Expr Double
      --      exampleExpr = abs(y*34) + 5 + x*y
      exampleExpr = z + x*y
        where
          x = sym "x"
          y = sym "y"
          z = sym "z"
  let args = map sym ["x", "y", "z"]

  print exampleExpr
  print $ rad exampleExpr args
  
  previewGraph $ exprToGraph exampleExpr
  previewGraph $ exprsToGraph (exampleExpr:(rad exampleExpr args))
