{- |
   Module      : Numeric.Dvda.AD
   Description : Automatic differentiation functions

   Forward- and reverse-mode automatic differentiation.
 -}

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.AD( fad
                      , rad
                      ) where

import Numeric.Dvda.AD.Rad
import Numeric.Dvda.AD.Fad
