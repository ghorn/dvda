{-# OPTIONS_GHC -Wall #-}

module Dvda.Graph ( FunGraph(..)
                  , FgNode
                  , SymSet
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

import Data.Graph.Inductive ( Gr, mkGraph )
import Data.GraphViz ( preview )
import Control.Concurrent ( threadDelay )
import Data.Hashable ( Hashable, hash )
import Data.List ( sort )
import Data.Maybe ( fromJust )
import Data.IntMap ( Key )
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import Numeric.LinearAlgebra ( Element )

import Dvda.GExpr ( GExpr(..), getChildren )

type SymSet a = HS.HashSet (GExpr a)
type FgNode a = (Key, SymSet a)

data FunGraph a b c = FunGraph
                      (HM.HashMap (GExpr a) (FgNode a)) -- main lookup
                      (IM.IntMap (GExpr a)) -- internal for reverse lookup
                      (b,[Key])
                      (c,[Key]) --  deriving Show
                                         
instance (Hashable a, Element a)  => Hashable (FunGraph a b c) where
  hash (FunGraph _ im (_, inskeys) (_, outskeys)) = hash (IM.toList im, inskeys, outskeys)
  
fgLookup :: (Eq a, Hashable a, Element a) => GExpr a -> FunGraph a b c -> Maybe (FgNode a)
fgLookup gexpr (FunGraph hm _ _ _) = HM.lookup gexpr hm

fgReverseLookup :: (Eq a, Hashable a, Element a) => Key -> FunGraph a b c -> Maybe (FgNode a)
fgReverseLookup k fg = do
  gexpr <- fgGExprFromKey k fg
  fgLookup gexpr fg

fgGExprFromKey :: (Eq a, Hashable a) => Key -> FunGraph a b c -> Maybe (GExpr a)
fgGExprFromKey k (FunGraph _ im _ _) = IM.lookup k im

funGraphSummary :: (Show a, Element a, Show b, Show c) => FunGraph a b c -> String
funGraphSummary (FunGraph hm _ (b,bkeys) (c,ckeys)) =
  init $ unlines [ "input dims: " ++ show b
                 , "input nodes:" ++ show bkeys
                 , "output dims: " ++ show c
                 , "output nodes:" ++ show ckeys
                 , "number of nodes: " ++ show (HM.size hm)
                 , "graph: " ++ show hm
                 ]

-- more extensive
funGraphSummary' :: (Show a, Element a, Show b, Show c) => FunGraph a b c -> String
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
