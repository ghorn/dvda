{-# OPTIONS_GHC -Wall #-}
{-# Language Rank2Types #-}
{-# Language FlexibleContexts #-}

module Dvda.Algorithm.Eval
       ( runAlgorithm
       , runAlgorithm'
       ) where

import Control.Monad.ST ( ST, runST )
import Data.Vector.Generic ( (!) )
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

import Dvda.Expr
import Dvda.Algorithm.Construct ( Algorithm(..), AlgOp(..), InputIdx(..), OutputIdx(..) )
import Dvda.Algorithm.FunGraph ( Node(..) )

newtype RtOp v a = RtOp (forall s. (G.Mutable v) s a -> v a -> (G.Mutable v) s a -> ST s ())

-- | purely run an algoritm
runAlgorithm :: G.Vector v a => Algorithm a -> v a -> v a
runAlgorithm alg =
  runAlg'' (algInDims alg) (algOutDims alg) (algWorkSize alg) (map toRtOp (algOps alg))
  where
    runAlg'' :: G.Vector v a => Int -> Int -> Int -> [RtOp v a] -> v a -> v a
    runAlg'' inSize outSize workSize ops inputVec
      | G.length inputVec /= inSize = error "runAlg: input dimension mismatch"
      | otherwise = runST $ do
        workVec <- GM.new workSize
        outputVec <- GM.new outSize
        mapM_ (\(RtOp op) -> op workVec inputVec outputVec) ops
        G.freeze outputVec

-- | run an algoritm in the ST monad, mutating a user-provided output vector
runAlgorithm' :: G.Vector v a => Algorithm a -> v a -> G.Mutable v s a -> ST s ()
runAlgorithm' alg =
  runAlg'' (algInDims alg) (algOutDims alg) (algWorkSize alg) (map toRtOp (algOps alg))
  where
    runAlg'' :: G.Vector v a => Int -> Int -> Int -> [RtOp v a] -> v a -> G.Mutable v s a -> ST s ()
    runAlg'' inSize outSize workSize ops inputVec outputVec
      | G.length inputVec /= inSize = error "runAlg': input dimension mismatch"
      | GM.length outputVec /= outSize = error "runAlg': output dimension mismatch"
      | otherwise = do
        workVec <- GM.new workSize
        mapM_ (\(RtOp op) -> op workVec inputVec outputVec) ops

bin :: GM.MVector (G.Mutable v) a => Node -> Node -> Node -> (a -> a -> a) -> RtOp v a
bin (Node k) (Node kx) (Node ky) f = RtOp $ \work _ _ -> do
  x <- GM.read work kx
  y <- GM.read work ky
  GM.write work k (f x y)

un :: GM.MVector (G.Mutable v) a => Node -> Node -> (a -> a) -> RtOp v a
un (Node k) (Node kx) f = RtOp $ \work _ _ -> GM.read work kx >>= GM.write work k . f

toRtOp :: G.Vector v a => AlgOp a -> RtOp v a
toRtOp (InputOp (Node k) (InputIdx i)) = RtOp $ \work input _ -> GM.write work k (input ! i)
toRtOp (OutputOp (Node k) (OutputIdx i)) = RtOp $ \work _ output -> do
  GM.read work k >>= GM.write output i
toRtOp (NormalOp (Node k) (GConst c)) =
  RtOp $ \work _ _ -> GM.write work k c
toRtOp (NormalOp (Node k) (GNum (FromInteger x))) =
  RtOp $ \work _ _ -> GM.write work k (fromIntegral x)
toRtOp (NormalOp (Node k) (GFractional (FromRational x))) =
  RtOp $ \work _ _ -> GM.write work k (fromRational x)

toRtOp (NormalOp k (GNum (Mul x y)))  = bin k x y (*)
toRtOp (NormalOp k (GNum (Add x y)))  = bin k x y (+)
toRtOp (NormalOp k (GNum (Sub x y)))  = bin k x y (-)
toRtOp (NormalOp k (GNum (Negate x))) = un k x negate
toRtOp (NormalOp k (GFractional (Div x y)))   = bin k x y (/)

toRtOp (NormalOp k (GNum (Abs x)))            = un k x abs
toRtOp (NormalOp k (GNum (Signum x)))         = un k x signum
toRtOp (NormalOp k (GFloating (Pow x y)))     = bin k x y (**)
toRtOp (NormalOp k (GFloating (LogBase x y))) = bin k x y logBase
toRtOp (NormalOp k (GFloating (Exp x)))       = un k x exp
toRtOp (NormalOp k (GFloating (Log x)))       = un k x log
toRtOp (NormalOp k (GFloating (Sin x)))       = un k x sin
toRtOp (NormalOp k (GFloating (Cos x)))       = un k x cos
toRtOp (NormalOp k (GFloating (ASin x)))      = un k x asin
toRtOp (NormalOp k (GFloating (ATan x)))      = un k x atan
toRtOp (NormalOp k (GFloating (ACos x)))      = un k x acos
toRtOp (NormalOp k (GFloating (Sinh x)))      = un k x sinh
toRtOp (NormalOp k (GFloating (Cosh x)))      = un k x cosh
toRtOp (NormalOp k (GFloating (Tanh x)))      = un k x tanh
toRtOp (NormalOp k (GFloating (ASinh x)))     = un k x asinh
toRtOp (NormalOp k (GFloating (ATanh x)))     = un k x atanh
toRtOp (NormalOp k (GFloating (ACosh x)))     = un k x acosh
toRtOp (NormalOp _ (GSym _)) = error "runAlg: there's symbol in my algorithm"
