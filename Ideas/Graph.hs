{-# OPTIONS_GHC -Wall #-}

module Ideas.Graph( GExpr(..)
                  , FunGraph(..)
                  , Key
                  , getChildren
                  , previewGraph
                  , toFGLGraph
                  ) where

import Data.Graph.Inductive(Gr,mkGraph)
import Data.GraphViz(Labellable,toLabelValue,preview)
import Control.Concurrent(threadDelay)
import Data.Vector.Unboxed(Vector)
import Data.IntMap(IntMap,assocs)

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

instance Show a => Labellable (GExpr a) where
  toLabelValue (GBinary op _ _) = toLabelValue $ show op
  toLabelValue (GUnary op _)    = toLabelValue $ show op
  toLabelValue (GSym [] name)   = toLabelValue $ name
  toLabelValue (GSym d name)    = toLabelValue $ name ++ "{" ++ show d ++ "}"
  toLabelValue (GSingleton _ x) = toLabelValue $ show x
  toLabelValue (GScale _ _)     = toLabelValue $ "scale"
  toLabelValue (GDot _ _)       = toLabelValue $ "dot"
  toLabelValue (GDeriv _ _)     = toLabelValue $ "deriv"
  toLabelValue (GGrad _ _)      = toLabelValue $ "grad"
  toLabelValue (GJacob _ _)     = toLabelValue $ "jacob"
  toLabelValue (GConst _ _)     = toLabelValue $ "const"
                                 
data FunGraph a = FunGraph (IntMap (GExpr a)) [Key] [Key] deriving (Show, Eq)

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

previewGraph :: Show a => FunGraph a -> IO ()
previewGraph fungraph = do
  preview $ toFGLGraph fungraph
  threadDelay 10000

toFGLGraph :: FunGraph a -> Gr (GExpr a) String
toFGLGraph (FunGraph gexprs _ _) = mkGraph lnodes ledges
  where
    lnodes = assocs gexprs
    ledges = concat $ map (\(k,ge) -> map (\ch -> (ch,k,"")) (getChildren ge)) lnodes
