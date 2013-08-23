{-# OPTIONS_GHC -Wall #-}

module Dvda.Algorithm
       ( Algorithm
       , constructAlgorithm
       , runAlgorithm
       , runAlgorithm'
       , squashIsSame
       ) where

import qualified Data.Vector.Generic as G

import Dvda.Algorithm.Construct ( Algorithm, constructAlgorithm, squashWorkVector )
import Dvda.Algorithm.Eval ( runAlgorithm, runAlgorithm' )

-- | test to see of SSA algorithm works the same as Live variables version
squashIsSame :: (Eq (v a), G.Vector v a) => v a -> Algorithm a -> Bool
squashIsSame x alg = runAlgorithm alg x == runAlgorithm (squashWorkVector alg) x
