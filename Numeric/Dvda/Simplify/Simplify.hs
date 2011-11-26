-- Simplify.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Simplify.Simplify( simplify
                                     , removeIdentities
                                     ) where

import Numeric.Dvda.Expr

import Numeric.Dvda.Simplify.RemoveTimesOne
import Numeric.Dvda.Simplify.RemovePlusZero


simplify :: Num a => Expr a -> Expr a
simplify = removeIdentities


removeIdentities :: Num a => Expr a -> Expr a
removeIdentities x 
  | x == xPruned = x
  | otherwise    = removeIdentities xPruned
  where
    xPruned = removeTimesOne $ removePlusZero x
