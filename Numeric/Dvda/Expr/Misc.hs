-- Misc.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Misc( outputNames
                             ) where

outputNames :: [String]
outputNames = map (\x -> "out"++show x) [(0::Integer)..]
