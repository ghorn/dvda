-- GNode.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Internal.GNode( getIdx
                                  , exprOfGNode
                                  , GNode(..)
                                  , topSort
                                  ) where

import Data.Graph.Inductive(Node)
import qualified Data.IntMap as IntMap
import Data.List(sortBy)
import Data.Maybe

data GNode a = GSource Node a
             | GOutput Node a Node Node String -- last node is the output index
             | GBroadcast Node a Node
             | GUnary  Node a Node
             | GBinary Node a (Node, Node) deriving (Show, Eq)

exprOfGNode :: GNode a -> a
exprOfGNode (GSource _ x) = x
exprOfGNode (GOutput _ x _ _ _) = x
exprOfGNode (GBroadcast _ x _) = x
exprOfGNode (GUnary _ x _) = x
exprOfGNode (GBinary _ x _) = x

getIdx :: GNode a -> Node
getIdx (GSource n _) = n
getIdx (GOutput n _ _ _ _) = n
getIdx (GBroadcast n _ _) = n
getIdx (GUnary n _ _) = n
getIdx (GBinary n _ _) = n

-- | sort a list of gnodes by whether they are descendents of each other
topSort :: [GNode a] -> [GNode a]
topSort gnodes = sortBy gCompare gnodes
  where
    gCompare (GOutput _ _ x _ _) (GOutput _ _ y _ _) = compare x y
    gCompare (GOutput _ _ _ _ _) _ = GT
    gCompare _ (GOutput _ _ _ _ _) = LT

    gCompare (GSource x _) (GSource y _) = compare x y
    gCompare (GSource _ _) _ = LT
    gCompare _ (GSource _ _) = GT
    gCompare x y 
      | getIdx x `isDescendentOf` getIdx y = LT
      | getIdx y `isDescendentOf` getIdx x = GT
      | otherwise       = EQ

    isDescendentOf x y 
      | isNothing parentOfX     = False
      | y == fromJust parentOfX = True
      | otherwise               = fromJust parentOfX `isDescendentOf` y
      where
        parentOfX = IntMap.lookup x parentMap

    parentMap = IntMap.fromList $ foldr f [] gnodes
      where
        f (GSource _ _) acc = acc
        f (GBroadcast self _ child) acc = acc ++ [(child, self)]
        f (GUnary self _ child) acc = acc ++ [(child, self)]
        f (GOutput self _ child _ _) acc = acc ++ [(child, self)]
        f (GBinary self _ (cx,cy)) acc = acc ++ [(cx, self), (cy, self)]
