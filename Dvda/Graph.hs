{-# OPTIONS_GHC -Wall #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}

module Dvda.Graph ( FunGraph(..)
                  , DynamicExpr(..)
                  , DvdaDim(..)
                  , FgNode
                  , SymSet
                  , emptyFunGraph
                  , fgLookup
                  , fgExprFromKey
                  , insert
                  , previewGraph
                  , toFGLGraph
                  , collisions
                  , showCollisions
                  , funGraphSummary
                  , funGraphSummary'
                  , showNodes
                  , fullShowNodes
                  , asIfExpr
                  ) where

import Data.Graph.Inductive ( Gr, mkGraph )
import Data.GraphViz ( Labellable, toLabelValue, preview )
import Data.GraphViz.Attributes.Complete ( Label )
import Control.Concurrent ( threadDelay )
import Data.List ( sort )
import Data.Hashable ( Hashable, hash, combine )
import Data.Maybe ( fromJust )
import Data.IntMap ( Key )
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import Numeric.LinearAlgebra ( Element )
import Data.Array.Repa ( Shape(rank,listOfShape), DIM0, DIM1, DIM2, Z(..) )
import Control.Monad.State ( State, get, put )

import Dvda.Expr ( Expr(..), Const(..), dim, fullShow' )

--------------------- dynamic Expr stuff ---------------------------
data DynamicExpr a = DynamicExpr0 (Expr DIM0 a)
                   | DynamicExpr1 (Expr DIM1 a)
                   | DynamicExpr2 (Expr DIM2 a) deriving Show
                                                         
asIfExpr :: (forall sh . Expr sh a -> b) -> DynamicExpr a -> b
asIfExpr f (DynamicExpr0 e) = f e
asIfExpr f (DynamicExpr1 e) = f e
asIfExpr f (DynamicExpr2 e) = f e
                                                         
instance (Element a, Hashable a) => Hashable (DynamicExpr a) where
  hash (DynamicExpr0 expr) = 0 `combine` (hash expr)
  hash (DynamicExpr1 expr) = 1 `combine` (hash expr)
  hash (DynamicExpr2 expr) = 2 `combine` (hash expr)

deriving instance (Eq a, Element a) => Eq (DynamicExpr a)

type SymSet a = HS.HashSet (DynamicExpr a)
type FgNode a = (Key, SymSet a)

data FunGraph a b c = FunGraph
                      (HM.HashMap (DynamicExpr a) (FgNode a)) -- main lookup
                      (IM.IntMap (DynamicExpr a)) -- internal for reverse lookup
                      b
                      c --  deriving Show
                                         
instance (Hashable a, Hashable b, Hashable c, Element a)  => Hashable (FunGraph a b c) where
  hash (FunGraph _ im inskeys outskeys) = hash (IM.toList im, inskeys, outskeys)

class Shape sh => DvdaDim sh where
  makeDynamic :: Expr sh a -> DynamicExpr a
  fromDynamic :: sh -> DynamicExpr a -> Expr sh a

instance DvdaDim DIM0 where
  makeDynamic = DynamicExpr0
  fromDynamic _ (DynamicExpr0 expr) = expr
  fromDynamic _ _ = error "DIM0: fromDynamic error"
instance DvdaDim DIM1 where
  makeDynamic = DynamicExpr1
  fromDynamic _ (DynamicExpr1 expr) = expr
  fromDynamic _ _ = error "DIM1: fromDynamic error"
instance DvdaDim DIM2 where
  makeDynamic = DynamicExpr2
  fromDynamic _ (DynamicExpr2 expr) = expr
  fromDynamic _ _ = error "DIM2: fromDynamic error"

fgLookup :: (Eq a, Hashable a, Element a, DvdaDim sh) => Expr sh a -> FunGraph a b c -> Maybe (FgNode a)
fgLookup (ERef sh k) fg = fgReverseLookup sh k fg
fgLookup expr (FunGraph hm _ _ _) = HM.lookup (makeDynamic expr) hm

fgReverseLookup :: (Eq a, Hashable a, Element a, DvdaDim sh) => sh -> Key -> FunGraph a b c -> Maybe (FgNode a)
fgReverseLookup sh k fg = do
  expr <- fgExprFromKey sh k fg
  fgLookup expr fg

fgExprFromKey :: DvdaDim sh => sh -> Key -> FunGraph a b c -> Maybe (Expr sh a)
fgExprFromKey sh k (FunGraph _ im _ _) = fmap (fromDynamic sh) (IM.lookup k im)

               
symSet :: (Eq a, Hashable a, Element a, DvdaDim sh) =>
          FunGraph a b c -> Expr sh a -> HS.HashSet (DynamicExpr a)
symSet _ e@(ESym _ _)          = HS.singleton (makeDynamic e)
symSet fg (ERef sh k)          = snd $ fromJust $ fgReverseLookup sh k fg
symSet _ (EDimensionless _)    = HS.empty
symSet _ (EConst _)            = HS.empty
symSet fg (EUnary _ x)         = symSet fg x
symSet fg (EBinary _ x y)      = (symSet fg x) `HS.union` (symSet fg y)
symSet fg (EScale x y)         = (symSet fg x) `HS.union` (symSet fg y)
symSet _ (EDeriv _ _) = error "don't take symSet of EDeriv"
symSet _ (EGrad _ _)  = error "don't take symSet of EGrad"
symSet _ (EJacob _ _) = error "don't take symSet of EJacob"

-- | Try to insert the Expr into the hashmap performing CSE.
--   If the Expr is not yet in the map, insert it and return new key.
--   Otherwise don't insert, just return existing key.
insert :: (Hashable a, Eq a, Element a, DvdaDim sh) => Expr sh a -> State (FunGraph a b c) (Expr sh a)
insert (ERef _ _) = error "don't insert ERef into graph, ya goon"
insert (EConst _) = error "don't insert EConst into graph, ya goon"
insert expr = do
  let dexpr = makeDynamic expr
  fg@(FunGraph hm im ins outs) <- get
  case fgLookup expr fg of
    Just (k',_) -> return (ERef (dim expr) k')
    Nothing -> do let k = HM.size hm
                      hm' = HM.insert dexpr (k, symSet fg expr) hm
                      im' = IM.insert k dexpr im
                  put (FunGraph hm' im' ins outs)
                  return (ERef (dim expr) k)


funGraphSummary :: (Show a, Element a, Show b, Show c) => FunGraph a b c -> String
funGraphSummary (FunGraph hm _ b c) =
  init $ unlines [ "inputs: " ++ show b
                 , "outputs: " ++ show c
                 , "number of nodes: " ++ show (HM.size hm)
                 ]

fullShowNodes :: (Show a, Element a) => FunGraph a b c -> String
fullShowNodes fg@(FunGraph _ im _ _) = init $ unlines $ map (\(a,b) -> show a ++ ": " ++ fs (fromDynamic Z b)) (IM.toList im)
  where
    fs expr = fullShow' (Just (\sh k -> fromJust $ fgExprFromKey sh k fg)) expr

showNodes :: (Show a, Element a) => FunGraph a b c -> String
showNodes (FunGraph _ im _ _) = init $ unlines (map show (IM.toList im))

-- more extensive
funGraphSummary' :: (Show a, Element a, Show b, Show c) => FunGraph a b c -> String
funGraphSummary' fg@(FunGraph _ im _ _) =
  init $ unlines $ [ "graph:" 
                 , init $ unlines (map show (IM.toList im))
                 , ""
                 ] ++ [funGraphSummary fg]

collisions :: (Hashable a, Element a) => FunGraph a b c -> (Int, Int, Double)
collisions (FunGraph gr _ _ _) = (numCollisions, numTotal, fromIntegral numCollisions / fromIntegral numTotal)
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

showCollisions :: (Hashable a, Element a) => FunGraph a b c -> String
showCollisions gr = show numCollisions ++ '/' : show numTotal ++ " collisions ("++show (100*frac)++" %)"
  where
    (numCollisions, numTotal, frac) = collisions gr

emptyFunGraph :: FunGraph a b c
emptyFunGraph = FunGraph HM.empty IM.empty inerr outerr
  where
    inerr = error "must specify inputs"
    outerr = error "must specify outputs"


previewGraph :: Show a => FunGraph a b c -> IO ()
previewGraph fungraph = do
  preview $ toFGLGraph fungraph
  threadDelay 10000

toFGLGraph :: FunGraph a b c -> Gr (DynamicExpr a) String
toFGLGraph (FunGraph hm _ _ _) = mkGraph lnodes ledges
  where
    lnodes = map (\(x,(y,_)) -> (y,x)) $ HM.toList hm
--    lnodes = IM.toList im
    ledges = concatMap (\(k,ge) -> map (\ch -> (ch,k,"")) (asIfExpr gc ge)) lnodes
      where
        gc :: Expr sh a -> [Key]
        gc (EBinary _ x y) = gc x ++ gc y
        gc (EUnary _ x) = gc x
        gc (ERef _ k) = [k]
        gc (ESym _ _) = []
        gc (EDimensionless _) = []
        gc (EScale x y) = gc x ++ gc y
        gc (EConst _) = []
        gc (EDeriv _ _) = error "don't call getChildren on EDeriv"
        gc (EJacob _ _) = error "don't call getChildren on EJacob"
        gc (EGrad _ _)  = error "don't call getChildren on EGrad"


instance Show a => Labellable (DynamicExpr a) where
  toLabelValue (DynamicExpr0 e) = tlv e
  toLabelValue (DynamicExpr1 e) = tlv e
  toLabelValue (DynamicExpr2 e) = tlv e
  
tlv :: (Show a, Shape sh) => Expr sh a -> Data.GraphViz.Attributes.Complete.Label
tlv (EBinary op _ _) = toLabelValue $ show op
tlv (EUnary op _)    = toLabelValue $ show op
tlv (ESym sh name) 
  | rank sh == 0 = toLabelValue name
  | otherwise    = toLabelValue $ name ++ "{" ++ (tail . init . show . reverse) (listOfShape sh) ++ "}"
tlv (EScale {})        = toLabelValue "scale"
tlv (EConst (CSingleton _ c)) = toLabelValue $ show c
tlv (EConst (CVec _ _)) = toLabelValue "vec"
tlv (EConst (CMat _ _)) = toLabelValue "mat"
tlv (EConst (CTensor _ _)) = toLabelValue "tensor"
tlv _ = error "don't try to preview one of those, ya goon"
