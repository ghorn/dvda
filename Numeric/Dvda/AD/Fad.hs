-- Fad.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.AD.Fad( fad
                          ) where

import Numeric.Dvda.AD.Dual

-- | Differentiate an R -> R^n function
fad :: Num a => (Dual a -> [Dual b]) -> a -> [b]
fad f x = map pert $ f (Dual x 1)
  where
    pert (Dual _ p) = p
