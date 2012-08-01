{-# OPTIONS_GHC -Wall #-}

module Dvda.CSE ( cse
                ) where

import Control.Monad.ST ( ST, runST )
import Data.Foldable ( toList )
import Data.Hashable ( Hashable )
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Tuple ( swap )

import Dvda.Expr ( GExpr(..), Floatings(..), Fractionals(..), Nums(..) )
import Dvda.FunGraph

import qualified Data.HashTable.Class as HT
import qualified Data.HashTable.ST.Cuckoo as C
type HashTable s v k = C.HashTable s v k

cse :: (Eq a, Hashable a) => FunGraph a -> FunGraph a
cse fg = nodelistToFunGraph (map swap htList) (fgInputs fg) outputIndices
  where
    (htList, im) = cse' (fgLookupGExpr fg) (fgOutputs fg)
    -- since the fgInputs are all symbolic (GSym _) there is no need for mapping old inputs to new inputs
    outputIndices = let
      oldIndexToNewIndex k = case IM.lookup k im of
        Just k' -> k'
        Nothing -> error $
                   "CSE error, in mapping old output indices to new, found an old one which was missing from" ++
                   "the old --> new Int mapping"
      in map (fmap oldIndexToNewIndex) (fgOutputs fg)

cse' ::
  (Eq a, Hashable a)
  => (Int -> Maybe (GExpr a Int))
  -> [MVS Int]
  -> ([(GExpr a Int, Int)], IntMap Int)
cse' lookupFun outputIndices = runST $ do
  ht <- HT.new
  let -- folding function
      f (im,n) [] = return (im,n)
      f (im0,n0) (k:ks) = do
        (_,im,n) <- insertOldNode k lookupFun ht im0 n0
        f (im,n) ks
  -- outputs
  (oldToNewIdx,_) <- f (IM.empty,0) (concatMap toList outputIndices)
  htList <- HT.toList ht
  return (htList, oldToNewIdx)

  
---- | take in an Int that represents a node in the original graph
---- see if that int has been inserted in the new graph
insertOldNode ::
  (Eq a, Hashable a)
  => Int -- ^ Int to be inserted
  -> (Int -> Maybe (GExpr a Int)) -- ^ function to lookup old GExpr from old Int reference
  -> HashTable s (GExpr a Int) Int -- ^ hashmap of new GExprs to their new Int references
  -> IntMap Int -- ^ intmap of old int reference to new int references
  -> Int -- ^ next free index
  -> ST s (Int, IntMap Int, Int)
insertOldNode kOld lookupOldGExpr ht oldNodeToNewNode0 nextFreeInt0 =
  case IM.lookup kOld oldNodeToNewNode0 of
    -- if the int has already been inserted in the new graph, return it
    Just k -> return (k, oldNodeToNewNode0, nextFreeInt0)
    -- if the int has not yet been inserted, then insert it
    -- get the old GExpr to which this node corresponds
    Nothing ->  case lookupOldGExpr kOld of
      Nothing -> error $ "in CSE, insertOldNode got an old key \"" ++ show kOld ++
                 "\" with was not found in the old graph"
      -- insert this old GExpr
      Just oldGExpr -> do
        (k, oldNodeToNewNode1, nextFreeInt1) <- insertOldGExpr oldGExpr lookupOldGExpr ht oldNodeToNewNode0 nextFreeInt0
        return (k, IM.insert kOld k oldNodeToNewNode1, nextFreeInt1)

insertOldGExpr ::
  (Eq a, Hashable a)
  => GExpr a Int -- ^ GExpr to be inserted
  -> (Int -> Maybe (GExpr a Int)) -- ^ function to lookup old GExpr from old Int reference
  -> HashTable s (GExpr a Int) Int -- ^ hashmap of new GExprs to their new Int references
  -> IntMap Int -- ^ intmap of old int reference to new int references
  -> Int -- ^ next free index
  -> ST s (Int, IntMap Int, Int)

insertOldGExpr g@(GSym _)                       = \_ ->  cseInsert g
insertOldGExpr g@(GConst _)                     = \_ ->  cseInsert g
insertOldGExpr g@(GNum (FromInteger _))         = \_ ->  cseInsert g
insertOldGExpr g@(GFractional (FromRational _)) = \_ ->  cseInsert g

insertOldGExpr (GNum (Mul x y))          = insertOldGExprBinary GNum Mul x y
insertOldGExpr (GNum (Add x y))          = insertOldGExprBinary GNum Add x y
insertOldGExpr (GNum (Sub x y))          = insertOldGExprBinary GNum Sub x y
insertOldGExpr (GFractional (Div x y))   = insertOldGExprBinary GFractional Div x y
insertOldGExpr (GFloating (Pow x y))     = insertOldGExprBinary GFloating Pow x y
insertOldGExpr (GFloating (LogBase x y)) = insertOldGExprBinary GFloating LogBase x y
                                         
insertOldGExpr (GNum (Negate x))         = insertOldGExprUnary  GNum Negate x
insertOldGExpr (GNum (Abs x))            = insertOldGExprUnary  GNum Abs x
insertOldGExpr (GNum (Signum x))         = insertOldGExprUnary  GNum Signum x
insertOldGExpr (GFloating (Exp x))       = insertOldGExprUnary  GFloating Exp x
insertOldGExpr (GFloating (Log x))       = insertOldGExprUnary  GFloating Log x
insertOldGExpr (GFloating (Sin x))       = insertOldGExprUnary  GFloating Sin x
insertOldGExpr (GFloating (Cos x))       = insertOldGExprUnary  GFloating Cos x
insertOldGExpr (GFloating (ASin x))      = insertOldGExprUnary  GFloating ASin x
insertOldGExpr (GFloating (ATan x))      = insertOldGExprUnary  GFloating ATan x
insertOldGExpr (GFloating (ACos x))      = insertOldGExprUnary  GFloating ACos x
insertOldGExpr (GFloating (Sinh x))      = insertOldGExprUnary  GFloating Sinh x
insertOldGExpr (GFloating (Cosh x))      = insertOldGExprUnary  GFloating Cosh x
insertOldGExpr (GFloating (Tanh x))      = insertOldGExprUnary  GFloating Tanh x
insertOldGExpr (GFloating (ASinh x))     = insertOldGExprUnary  GFloating ASinh x
insertOldGExpr (GFloating (ATanh x))     = insertOldGExprUnary  GFloating ATanh x
insertOldGExpr (GFloating (ACosh x))     = insertOldGExprUnary  GFloating ACosh x

insertOldGExprBinary ::
  (Eq a, Hashable a)
  => (f -> GExpr a Int)
  -> (Int -> Int -> f)
  -> Int -> Int
  -> (Int -> Maybe (GExpr a Int)) -- ^ function to lookup old GExpr from old Int reference
  -> HashTable s (GExpr a Int) Int -- ^ hashmap of new GExprs to their new Int references
  -> IntMap Int -- ^ intmap of old int reference to new int references
  -> Int -- ^ next free index
  -> ST s (Int, IntMap Int, Int)
insertOldGExprBinary gnum mul kxOld kyOld lookupOldGExpr ht oldNodeToNewNode0 nextFreeInt0 = do
  (kx, oldNodeToNewNode1,nextFreeInt1) <- insertOldNode kxOld lookupOldGExpr ht oldNodeToNewNode0 nextFreeInt0
  (ky, oldNodeToNewNode2,nextFreeInt2) <- insertOldNode kyOld lookupOldGExpr ht oldNodeToNewNode1 nextFreeInt1
  let newGExpr = gnum (mul kx ky)
  cseInsert newGExpr ht oldNodeToNewNode2 nextFreeInt2

insertOldGExprUnary ::
  (Eq a, Hashable a)
  => (f -> GExpr a Int)
  -> (Int -> f)
  -> Int
  -> (Int -> Maybe (GExpr a Int)) -- ^ function to lookup old GExpr from old Int reference
  -> HashTable s (GExpr a Int) Int -- ^ hashmap of new GExprs to their new Int references
  -> IntMap Int -- ^ intmap of old int reference to new int references
  -> Int -- ^ next free index
  -> ST s (Int, IntMap Int, Int)
insertOldGExprUnary gnum neg kxOld lookupOldGExpr ht oldNodeToNewNode0 nextFreeInt0 = do
  (kx, oldNodeToNewNode1,nextFreeInt1) <- insertOldNode kxOld lookupOldGExpr ht oldNodeToNewNode0 nextFreeInt0
  let newGExpr = gnum (neg kx)
  cseInsert newGExpr ht oldNodeToNewNode1 nextFreeInt1

cseInsert :: (Eq a, Hashable a) => GExpr a Int -> HashTable s (GExpr a Int) Int -> IntMap Int -> Int
             -> ST s (Int, IntMap Int, Int)
cseInsert gexpr ht oldNodeToNewNode0 nextFreeInt0 = do
  lu <- HT.lookup ht gexpr
  case lu of
    Just k -> return (k, oldNodeToNewNode0, nextFreeInt0)
    Nothing -> do
      HT.insert ht gexpr nextFreeInt0
      return (nextFreeInt0, oldNodeToNewNode0, nextFreeInt0+1)
        
