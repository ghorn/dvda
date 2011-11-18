-- Codegen.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Codegen.Codegen( test
                                   ) where

import Numeric.Dvda.Codegen.CFunction(exprsToCFunction)
import Numeric.Dvda.Expr.Expr(Expr(..), symbolic)
import Numeric.Dvda.Expr.ExprToGraph

test :: IO ()
test = do
  putStrLn $ exprsToCFunction someExprs
  previewGraph_ $ exprsToGraph someExprs

someExprs :: [Expr Double]
someExprs = [x*y/cos(2), x*y]
  where
    x = symbolic "x"
    y = symbolic "y"

