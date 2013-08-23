{-# OPTIONS_GHC -Wall #-}

module Dvda.Algorithm
       ( Algorithm
       , constructAlgorithm
       , runAlgorithm
       , runAlgorithm'
       , toSymbolicAlg
       , squashIsSame
       ) where

import qualified Data.Vector.Generic as G

import Dvda.Algorithm.Construct ( Algorithm(..), AlgOp(..), constructAlgorithm, squashWorkVector )
import Dvda.Algorithm.Eval ( runAlgorithm, runAlgorithm' )
import Dvda.Expr ( Expr(..), GExpr(..) )

-- | test to see of SSA algorithm works the same as Live variables version
squashIsSame :: (Eq (v a), G.Vector v a) => v a -> Algorithm a -> Bool
squashIsSame x alg = runAlgorithm alg x == runAlgorithm (squashWorkVector alg) x

-- | Convert an algorithm into a symbolic algorithm.
--   Is there any reason to keep this when we have toFloatingAlg?
toSymbolicAlg :: Eq a => Algorithm a -> Algorithm (Expr a)
toSymbolicAlg (Algorithm ind outd ops ws) = Algorithm ind outd (map opToExpr ops) ws
  where
    opToExpr :: Eq a => AlgOp a -> AlgOp (Expr a)
    opToExpr (InputOp k idx) = InputOp k idx
    opToExpr (OutputOp k idx) = OutputOp k idx
    opToExpr (NormalOp k gexpr) = NormalOp k (g2e gexpr)
      where
        g2e :: Eq a => GExpr a b -> GExpr (Expr a) b
        g2e (GSym x) = GSym x
        g2e (GConst x) = GConst (EConst x)
        g2e (GNum x) = GNum x
        g2e (GFractional x) = GFractional x
        g2e (GFloating x) = GFloating x
