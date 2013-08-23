{-# OPTIONS_GHC -Wall #-}

module Dvda.Algorithm
       ( Algorithm
       , constructAlgorithm
       , runAlgorithm
       , runAlgorithm'
       , toSymbolic
       , squashIsSame
       ) where

import qualified Data.Vector.Generic as G

import Dvda.Algorithm.Construct ( Algorithm(..), AlgOp(..), constructAlgorithm, squashWorkVector )
import Dvda.Algorithm.Eval ( runAlgorithm, runAlgorithm' )
import Dvda.Expr ( Expr(..), GExpr(..) )

-- | test to see of SSA algorithm works the same as Live variables version
squashIsSame :: (Eq (v a), G.Vector v a) => v a -> Algorithm a -> Bool
squashIsSame x alg = runAlgorithm alg x == runAlgorithm (squashWorkVector alg) x

-- | convert an algorithm into a symbolic algorithm
toSymbolic :: Ord a => Algorithm a -> Algorithm (Expr a)
toSymbolic alg = alg { algOps = map opToExpr (algOps alg) }
  where
    opToExpr :: Ord a => AlgOp a -> AlgOp (Expr a)
    opToExpr (InputOp k idx) = InputOp k idx
    opToExpr (OutputOp k idx) = OutputOp k idx
    opToExpr (NormalOp k gexpr) = NormalOp k (g2e gexpr)
      where
        g2e :: Ord a => GExpr a b -> GExpr (Expr a) b
        g2e (GSym s) = GSym s
        g2e (GConst a) = GConst (EConst a)
        g2e (GNum x) = GNum x
        g2e (GFractional x) = GFractional x
        g2e (GFloating x) = GFloating x
