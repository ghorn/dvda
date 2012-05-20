{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}

module Dvda.Graph ( GExpr(..)
                  , FunGraph(..)
                  , DerivMap
                  , MkDeriv
                  , LazyDeriv(..)
                  , Key
                  , emptyFunGraph
                  , fgLookup
                  , fgReverseLookup
                  , fgGExprFromKey
                  , previewGraph
                  , toFGLGraph
                  , collisions
                  , showCollisions
                  , funGraphSummary
                  , funGraphSummary'
                  ) where

import Control.Monad.State.Lazy ( StateT )
import Data.Functor.Identity ( Identity )
--import Control.Monad.State ( MonadState, State, get, put, liftM, runState )
import Data.Graph.Inductive ( Gr, mkGraph )
import Data.GraphViz ( Labellable, toLabelValue, preview )
import Control.Concurrent ( threadDelay )
import Data.Vector.Unboxed ( Vector, Unbox )
import qualified Data.Vector.Unboxed as V( foldl )
import Data.Hashable ( Hashable, hash, combine )
import Data.List ( sort )
import Data.Maybe ( fromJust )
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM

import Dvda.BinUn( BinOp, UnOp, isCommutative )

type Key = Int

type DerivMap a b c = HM.HashMap (GExpr a) (LazyDeriv a b c)
type MkDeriv a b c = StateT (FunGraph a b c) Identity Key

data LazyDeriv a b c = Unevaluated (MkDeriv a b c)
                     | Evaluated Key
                       
data FunGraph a b c = FunGraph
                      (HM.HashMap (GExpr a) (Key, DerivMap a b c)) -- main lookup
                      (IM.IntMap (GExpr a)) -- internal for reverse lookup
                      (b,[Key])
                      (c,[Key]) deriving (Show)--, Eq)
                                         
instance (Show a, Unbox a) => Show (LazyDeriv a b c) where
  show (Unevaluated _) = "Unevaluated"
  show (Evaluated x) = "Evaluted " ++ show x

fgLookup :: (Eq a, Hashable a, Unbox a) => GExpr a -> FunGraph a b c -> Maybe (Key, DerivMap a b c)
fgLookup gexpr (FunGraph hm _ _ _) = HM.lookup gexpr hm

fgGExprFromKey :: (Eq a, Hashable a, Unbox a) => Key -> FunGraph a b c -> Maybe (GExpr a)
fgGExprFromKey k (FunGraph _ im _ _) = IM.lookup k im

fgReverseLookup :: (Eq a, Hashable a, Unbox a) => Key -> FunGraph a b c -> Maybe (Key, DerivMap a b c)
fgReverseLookup k fg = do
  gexpr <- fgGExprFromKey k fg
  fgLookup gexpr fg

funGraphSummary :: (Show a, Unbox a, Show b, Show c) => FunGraph a b c -> String
funGraphSummary (FunGraph hm _ (b,bkeys) (c,ckeys)) =
  init $ unlines [ "input dims: " ++ show b
                 , "input nodes:" ++ show bkeys
                 , "output dims: " ++ show c
                 , "output nodes:" ++ show ckeys
                 , "number of nodes: " ++ show (HM.size hm)
                 , "graph: " ++ show hm
                 ]

-- more extensive
funGraphSummary' :: (Show a, Unbox a, Show b, Show c) => FunGraph a b c -> String
funGraphSummary' (FunGraph hm im (b,bkeys) (c,ckeys)) =
  init $ unlines [ "input dims: " ++ show b
                 , "input nodes:" ++ show bkeys
                 , "output dims: " ++ show c
                 , "output nodes:" ++ show ckeys
                 , "number of nodes: " ++ show (HM.size hm)
                 , "graph:" 
                 , init $ unlines (map show (IM.toList im))
                 , "outputs:"
                 , init $ unlines (map (show . (\k -> fromJust (IM.lookup k im))) ckeys)
                 ]

data GExpr a = GBinary BinOp Key Key
             | GUnary UnOp Key
             | GSym [Int] String
             | GSingleton [Int] a
             | GScale Key Key
             | GDot Key Key
--             | GDeriv Key Key
--             | GGrad Key Key
--             | GJacob Key Key
             | GConst [Int] (Vector a) deriving (Show, Eq)
                                                
instance (Unbox a, Hashable a) => Hashable (GExpr a) where
  -- if the binary operator is commutative then always put the lesser hash first
  -- so that e.g. x*y and y*x are not computed twice
  hash (GBinary op k1 k2) = 24 `combine` hash op `combine` hk1' `combine` hk2'
    where
      hk1 = hash k1
      hk2 = hash k2
      (hk1', hk2')
        | isCommutative op && hk2 < hk1 = (hk2, hk1)
        | otherwise = (hk1, hk2)
  --  hash (GBinary op k1 k2) = 24 `combine` hash op `combine` hash k1 `combine` hash k2
  hash (GUnary op k)      = 25 `combine` hash op `combine` hash k
  hash (GSym d name)      = 26 `combine` hash d `combine` hash name
  hash (GSingleton d x)   = 27 `combine` hash d `combine` hash x
  hash (GScale k1 k2)     = 28 `combine` hash k1 `combine` hash k2
  hash (GDot k1 k2)       = 29 `combine` hash k1 `combine` hash k2
--  hash (GDeriv k1 k2)     = 30 `combine` hash k1 `combine` hash k2
--  hash (GGrad k1 k2)      = 31 `combine` hash k1 `combine` hash k2
--  hash (GJacob k1 k2)     = 32 `combine` hash k1 `combine` hash k2
  hash (GConst d v)       = V.foldl (\acc x -> acc `combine` hash x) (33 `combine` hash d) v


instance Show a => Labellable (GExpr a) where
  toLabelValue (GBinary op _ _) = toLabelValue $ show op
  toLabelValue (GUnary op _)    = toLabelValue $ show op
  toLabelValue (GSym [] name)   = toLabelValue name
  toLabelValue (GSym d name)    = toLabelValue $ name ++ "{" ++ (tail . init . show . reverse) d ++ "}"
  toLabelValue (GSingleton _ x) = toLabelValue $ show x
  toLabelValue (GScale _ _)     = toLabelValue "scale"
  toLabelValue (GDot _ _)       = toLabelValue "dot"
--  toLabelValue (GDeriv _ _)     = toLabelValue "deriv"
--  toLabelValue (GGrad _ _)      = toLabelValue "grad"
--  toLabelValue (GJacob _ _)     = toLabelValue "jacob"
  toLabelValue (GConst _ _)     = toLabelValue "const"

collisions :: (Hashable a, Unbox a) => FunGraph a b c -> (Int, Int, Double)
collisions (FunGraph gr _ _ _) = (numCollisions, numTotal, (fromIntegral numCollisions)/(fromIntegral numTotal))
  where
    allHashes = sort $ map (hash . fst) $ HM.toList gr
    numTotal = length allHashes
    numCollisions = countCollisions 0 allHashes
      where
        countCollisions n (x:y:ys)
          | x == y    = countCollisions (n+1) (y:ys)
          | otherwise = countCollisions n     (y:ys)
        countCollisions n [_] = n
        countCollisions n []  = n

showCollisions :: (Hashable a, Unbox a) => FunGraph a b c -> String
showCollisions gr = show numCollisions ++ '/' : show numTotal ++ " collisions ("++show (100*frac)++" %)"
  where
    (numCollisions, numTotal, frac) = collisions gr

emptyFunGraph :: FunGraph a b c
emptyFunGraph = FunGraph HM.empty IM.empty (inerr,inerr) (outerr,outerr)
  where
    inerr = error "must specify inputs"
    outerr = error "must specify outputs"


previewGraph :: Show a => FunGraph a b c -> IO ()
previewGraph fungraph = do
  preview $ toFGLGraph fungraph
  threadDelay 10000

toFGLGraph :: FunGraph a b c -> Gr (GExpr a) String
toFGLGraph (FunGraph gexprs _ _ _) = mkGraph lnodes ledges
  where
    lnodes = map (\(x,(y,_)) -> (y,x)) $ HM.toList gexprs
--    lnodes = IM.toList gexprs
    ledges = concatMap (\(k,ge) -> map (\ch -> (ch,k,"")) (getChildren ge)) lnodes
      where
        getChildren (GBinary _ k1 k2) = [k1,k2]
        getChildren (GUnary _ k) = [k]
        getChildren (GSym _ _ ) = []
        getChildren (GSingleton _ _) = []
        getChildren (GScale k1 k2) = [k1,k2]
        getChildren (GDot k1 k2) = [k1,k2]
--        getChildren (GDeriv k1 k2) = [k1,k2]
--        getChildren (GGrad k1 k2) = [k1,k2]
--        getChildren (GJacob k1 k2) = [k1,k2]
        getChildren (GConst _ _) = []
