-- Function.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Function( Function(..)
                            , inputNames
                            ) where

import Numeric.Dvda.Expr.Expr
import Numeric.Dvda.Expr.SourceType

data Function a = Function { funInputs :: [Expr a]
                           , funOutputs :: [Expr a]
--                           , funEval :: [a] -> [a]
                           } deriving Show


inputNames :: (Eq a, Show a) => Function a -> [String]
inputNames fun@(Function {funInputs = inputs}) = map f inputs
  where
    f (Source (Sym name)) = name
    f _ = error "non-source Function input detected in " ++ show fun
