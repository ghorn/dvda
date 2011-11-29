-- Properties.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Properties(properties
                                   ) where

import Numeric.Dvda.Expr.Expr

properties :: [Bool]
properties = map (\f -> (f "x" :: Expr Double) == f "x" + 0) [sym, symVec 13, symMat (7,2)]
    
