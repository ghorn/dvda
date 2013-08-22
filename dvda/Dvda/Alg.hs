{-# OPTIONS_GHC -Wall #-}
{-# Language StandaloneDeriving #-}
{-# Language GADTs #-}
{-# Language TypeFamilies #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}

module Dvda.Alg ( Algorithm(..)
                , AlgOp(..)
                , toAlg
                ) where

import Control.Monad.ST ( runST )
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM
import Data.Hashable ( Hashable(..) )
import Data.Vector.Unboxed ( (!) )
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Dvda.Expr
import Dvda.FunGraph ( FunGraph(..), Node(..), toFunGraph )

newtype InputIdx = InputIdx Int deriving Show
newtype OutputIdx = OutputIdx Int deriving Show

data AlgOp a = InputOp  Node InputIdx
             | OutputOp Node OutputIdx
             | NormalOp Node (GExpr a Node)
             deriving Show

data Algorithm a = Algorithm { algInDims :: Int
                             , algOutDims :: Int
                             , algOps :: [AlgOp a]
                             , algWorkSize :: Int
                             }

graphToAlg :: [(Node,GExpr a Node)] -> V.Vector (Sym,InputIdx) -> V.Vector (Node,OutputIdx)
              -> [AlgOp a]
graphToAlg rgr0 inSyms outIdxs = f rgr0
  where
    inSymMap = HM.fromList $ (F.toList inSyms :: [(Sym,InputIdx)])
    outIdxMap = IM.fromList $ map (\(Node k, x) -> (k, x)) $ (F.toList outIdxs :: [(Node,OutputIdx)])

    f ((k@(Node k'),GSym s):xs) = case HM.lookup s inSymMap of
      Nothing -> error "toAlg: symbolic is not in inputs"
      Just inIdx -> case IM.lookup k' outIdxMap of
        -- sym is an input
        Nothing -> InputOp k inIdx : f xs
        -- sym is an input and an output
        Just outIdx -> InputOp k inIdx : OutputOp k outIdx : f xs
    f ((k@(Node k'),x):xs) = case IM.lookup k' outIdxMap of
      -- no input or output
      Nothing -> NormalOp k x : f xs
      -- output only
      Just outIdx -> NormalOp k x : OutputOp k outIdx : f xs
    f [] = []

toAlg :: (Eq a, Show a, Hashable a) => V.Vector (Expr a) -> V.Vector (Expr a) -> IO (Algorithm a)
toAlg inputVecs outputVecs = do
  fg <- toFunGraph inputVecs outputVecs
  let inputIdxs  = V.map (\(k,x) -> (x,  InputIdx k)) (V.indexed ( fgInputs fg))
      outputIdxs = V.map (\(k,x) -> (x, OutputIdx k)) (V.indexed (fgOutputs fg))
      workVectorSize n ((NormalOp (Node m) _):xs) = workVectorSize (max n m) xs
      workVectorSize n ((InputOp  (Node m) _):xs) = workVectorSize (max n m) xs
      workVectorSize n ((OutputOp (Node m) _):xs) = workVectorSize (max n m) xs
      workVectorSize n [] = n+1
      ops = graphToAlg (fgReified fg) inputIdxs outputIdxs
  return $ Algorithm { algInDims  = V.length inputIdxs
                        , algOutDims = V.length outputIdxs
                        , algOps = ops
                        , algWorkSize = workVectorSize (-1) ops
                        }

runAlg :: Algorithm Double -> U.Vector Double -> U.Vector Double
runAlg alg vIns
  | U.length vIns /= algInDims alg = error "runAlg: input dimension mismatch"
  | otherwise = runST $ do
    work <- UM.new (algWorkSize alg)
    vOuts <- UM.new (algOutDims alg)

    let bin (Node k) (Node kx) (Node ky) f = do
          x <- UM.read work kx
          y <- UM.read work ky
          UM.write work k (f x y)
        un (Node k) (Node kx) f = do
          x <- UM.read work kx
          UM.write work k (f x)
        runMe (InputOp (Node k) (InputIdx i)) = UM.write work k (vIns ! i)
        runMe (OutputOp (Node k) (OutputIdx i)) = do
          x <- UM.read work k
          UM.write vOuts i x
        runMe (NormalOp (Node k) (GConst c))        = UM.write work k c
        runMe (NormalOp (Node k) (GNum (FromInteger x))) = UM.write work k (fromIntegral x)
        runMe (NormalOp (Node k) (GFractional (FromRational x))) = UM.write work k (fromRational x)

        runMe (NormalOp k (GNum (Mul x y)))  = bin k x y (*)
        runMe (NormalOp k (GNum (Add x y)))  = bin k x y (+)
        runMe (NormalOp k (GNum (Sub x y)))  = bin k x y (-)
        runMe (NormalOp k (GNum (Negate x))) = un k x negate
        runMe (NormalOp k (GFractional (Div x y)))   = bin k x y (/)

        runMe (NormalOp k (GNum (Abs x)))            = un k x abs
        runMe (NormalOp k (GNum (Signum x)))         = un k x signum
        runMe (NormalOp k (GFloating (Pow x y)))     = bin k x y (**)
        runMe (NormalOp k (GFloating (LogBase x y))) = bin k x y logBase
        runMe (NormalOp k (GFloating (Exp x)))       = un k x exp
        runMe (NormalOp k (GFloating (Log x)))       = un k x log
        runMe (NormalOp k (GFloating (Sin x)))       = un k x sin
        runMe (NormalOp k (GFloating (Cos x)))       = un k x cos
        runMe (NormalOp k (GFloating (ASin x)))      = un k x asin
        runMe (NormalOp k (GFloating (ATan x)))      = un k x atan
        runMe (NormalOp k (GFloating (ACos x)))      = un k x acos
        runMe (NormalOp k (GFloating (Sinh x)))      = un k x sinh
        runMe (NormalOp k (GFloating (Cosh x)))      = un k x cosh
        runMe (NormalOp k (GFloating (Tanh x)))      = un k x tanh
        runMe (NormalOp k (GFloating (ASinh x)))     = un k x asinh
        runMe (NormalOp k (GFloating (ATanh x)))     = un k x atanh
        runMe (NormalOp k (GFloating (ACosh x)))     = un k x acosh
        runMe (NormalOp _ (GSym _)) = error "runAlg: there's symbol in my algorithm"
    mapM_ runMe (algOps alg)
    U.freeze vOuts

go :: IO ()
go = do
  let x = sym "x" :: Expr Double
      y = sym "y" :: Expr Double
      z = sym "z" :: Expr Double
      ins :: V.Vector (Expr Double)
      ins = V.fromList [x,y,z]
      outs :: V.Vector (Expr Double)
      outs = V.fromList [x*2, y*x - 6, 0, x]

  alg <- toAlg ins outs
  mapM_ print (algOps alg)
  let f = runAlg alg
      inputs = U.fromList [1,2,3]
  print (f inputs)

--  putStrLn "outputs:"
--  mapM_ print $ V.toList $ runVecs $ fgOutputs fg
