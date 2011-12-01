{-# OPTIONS_GHC -Wall #-}

-- haddock doc example:
{- |
   Module      : Numeric.Dvda
   Description : Top level module
   Copyright   : (c) yo
   License     : license yo
   Maintainer  : email yo

   This is the top level module.
   Reference to 'Numeric.Dvda.Expr', why not?
 -}

module Numeric.Dvda( module Numeric.Dvda.AD
                   , module Numeric.Dvda.Expr
                   , module Numeric.Dvda.Vis
                   , module Numeric.Dvda.Function
                   , module Numeric.Dvda.Substitute
                   ) where

import Numeric.Dvda.AD
import Numeric.Dvda.Expr
import Numeric.Dvda.Vis
import Numeric.Dvda.Function
import Numeric.Dvda.Substitute
