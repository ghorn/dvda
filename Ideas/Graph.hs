{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Ideas.Graph ( GExpr(..)
                   , FunGraph(..)
                   , Key
                   , insert
                   , emptyFunGraph
                   , getChildren
                   , previewGraph
                   , toFGLGraph
                   ) where

import Control.Monad.State
import Data.Graph.Inductive(Gr,mkGraph)
import Data.GraphViz(Labellable,toLabelValue,preview)
import Control.Concurrent(threadDelay)
import Data.Vector.Unboxed(Vector,Unbox)
import qualified Data.Vector.Unboxed as V(foldl)
import Data.Hashable(Hashable,hash,combine)
import qualified Data.HashMap.Strict as HM(HashMap,empty,size,lookup,insert,toList)

import Ideas.BinUn(BinOp, UnOp)

type Key = Int

data GExpr a = GBinary BinOp Key Key
             | GUnary UnOp Key
             | GSym [Int] String
             | GSingleton [Int] a
             | GScale Key Key
             | GDot Key Key
             | GDeriv Key Key
             | GGrad Key Key
             | GJacob Key Key
             | GConst [Int] (Vector a) deriving (Show, Eq)
                                                
instance (Unbox a, Hashable a) => Hashable (GExpr a) where
  hash (GBinary op k1 k2) = 24 `combine` hash op `combine` hash k1 `combine` hash k2
  hash (GUnary op k)      = 25 `combine` hash op `combine` hash k
  hash (GSym d name)      = 26 `combine` hash d `combine` hash name
  hash (GSingleton d x)   = 27 `combine` hash d `combine` hash x
  hash (GScale k1 k2)     = 28 `combine` hash k1 `combine` hash k2
  hash (GDot k1 k2)       = 29 `combine` hash k1 `combine` hash k2
  hash (GDeriv k1 k2)     = 30 `combine` hash k1 `combine` hash k2
  hash (GGrad k1 k2)      = 31 `combine` hash k1 `combine` hash k2
  hash (GJacob k1 k2)     = 32 `combine` hash k1 `combine` hash k2
  hash (GConst d v)       = V.foldl (\acc x -> acc `combine` hash x) (33 `combine` hash d) v


instance Show a => Labellable (GExpr a) where
  toLabelValue (GBinary op _ _) = toLabelValue $ show op
  toLabelValue (GUnary op _)    = toLabelValue $ show op
  toLabelValue (GSym [] name)   = toLabelValue name
  toLabelValue (GSym d name)    = toLabelValue $ name ++ "{" ++ show d ++ "}"
  toLabelValue (GSingleton _ x) = toLabelValue $ show x
  toLabelValue (GScale _ _)     = toLabelValue "scale"
  toLabelValue (GDot _ _)       = toLabelValue "dot"
  toLabelValue (GDeriv _ _)     = toLabelValue "deriv"
  toLabelValue (GGrad _ _)      = toLabelValue "grad"
  toLabelValue (GJacob _ _)     = toLabelValue "jacob"
  toLabelValue (GConst _ _)     = toLabelValue "const"

data FunGraph a = FunGraph (HM.HashMap (GExpr a) Key) [Key] [Key] deriving (Show, Eq)

getChildren :: GExpr a -> [Key]
getChildren (GBinary _ k1 k2) = [k1,k2]
getChildren (GUnary _ k) = [k]
getChildren (GSym _ _ ) = []
getChildren (GSingleton _ _) = []
getChildren (GScale k1 k2) = [k1,k2]
getChildren (GDot k1 k2) = [k1,k2]
getChildren (GDeriv k1 k2) = [k1,k2]
getChildren (GGrad k1 k2) = [k1,k2]
getChildren (GJacob k1 k2) = [k1,k2]
getChildren (GConst _ _) = []

emptyFunGraph :: FunGraph a
emptyFunGraph = FunGraph HM.empty [] []

-- | Try to insert a GExpr into the hashmap performing CSE.
--   If the GExpr is not yet in the map, insert it.
--   Otherwise don't insert, just return existing key.
insert :: (Unbox a, Hashable a, Eq a, MonadState (FunGraph a) m) => GExpr a -> m Int
insert gexpr = do
  FunGraph xs ins outs <- get
  let k = HM.size xs
      ins' = case gexpr of (GSym _ _) -> ins++[k] -- add Sym to FunGraph inputs
                           _          -> ins
  case HM.lookup gexpr xs of Nothing -> do put (FunGraph (HM.insert gexpr k xs) ins' outs)
                                           return k
                             Just k' -> return k'

previewGraph :: Show a => FunGraph a -> IO ()
previewGraph fungraph = do
  preview $ toFGLGraph fungraph
  threadDelay 10000

toFGLGraph :: FunGraph a -> Gr (GExpr a) String
toFGLGraph (FunGraph gexprs _ _) = mkGraph lnodes ledges
  where
    lnodes = map (\(x,y) -> (y,x)) $ HM.toList gexprs
    ledges = concatMap (\(k,ge) -> map (\ch -> (ch,k,"")) (getChildren ge)) lnodes
