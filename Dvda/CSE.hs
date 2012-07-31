{-# OPTIONS_GHC -Wall #-}

module Dvda.CSE ( cse
                ) where

--import Control.Monad.ST -- ( runST )
import Data.Foldable ( toList )
import Data.Hashable ( Hashable )
import Dvda.HashMap ( HashMap )
import qualified Dvda.HashMap as HM
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM

import Dvda.Expr ( GExpr(..), Floatings(..), Fractionals(..), Nums(..) )
import Dvda.FunGraph

--import qualified Data.HashTable.ST.Cuckoo as HT
--type HashTable s v k = HT.HashTable s v k

cse ::
  (Eq a, Hashable a)
  => (Int -> GExpr a Int)
  -> [MVS Int]
  -> (HashMap (GExpr a Int) Int, IntMap Int)
cse lookupFun outputIndices = (hm,oldToNewIdx)
  where
    -- folding function
    f k (hm0,im0,n0) = (\(_,hm',im',n') -> (hm',im',n')) $ insertOldNode k lookupFun hm0 im0 n0
    -- outputs
    (hm,oldToNewIdx,_) = foldr f (HM.empty,IM.empty,0) (concatMap toList outputIndices)
        

-- | take in an Int that represents a node in the original graph
-- see if that int has been inserted in the new graph
insertOldNode ::
  (Eq a, Hashable a)
  => Int -- ^ Int to be inserted
  -> (Int -> GExpr a Int) -- ^ function to lookup old GExpr from old Int reference
  -> HashMap (GExpr a Int) Int -- ^ hashmap of new GExprs to their new Int references
  -> IntMap Int -- ^ intmap of old int reference to new int references
  -> Int -- ^ next free index
  -> (Int, HashMap (GExpr a Int) Int, IntMap Int, Int)
insertOldNode kOld lookupOldGExpr hm0 oldNodeToNewNode0 nextFreeInt0 =
  case IM.lookup kOld oldNodeToNewNode0 of
    -- if the int has already been inserted in the new graph, return it
    Just k -> (k, hm0, oldNodeToNewNode0, nextFreeInt0)
    -- if the int has not yet been inserted, then insert it
    Nothing -> (k, hm1, IM.insert kOld k oldNodeToNewNode1, nextFreeInt1)
      where
        -- get the old GExpr to which this node corresponds
        oldGExpr = lookupOldGExpr kOld
        -- insert this old GExpr
        (k, hm1, oldNodeToNewNode1, nextFreeInt1) =
          insertOldGExpr oldGExpr lookupOldGExpr hm0 oldNodeToNewNode0 nextFreeInt0

insertOldGExpr ::
  (Eq a, Hashable a)
  => GExpr a Int -- ^ GExpr to be inserted
  -> (Int -> GExpr a Int) -- ^ function to lookup old GExpr from old Int reference
  -> HashMap (GExpr a Int) Int -- ^ hashmap of new GExprs to their new Int references
  -> IntMap Int -- ^ intmap of old int reference to new int references
  -> Int -- ^ next free index
  -> (Int, HashMap (GExpr a Int) Int, IntMap Int, Int)

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
  -> (Int -> GExpr a Int) -- ^ function to lookup old GExpr from old Int reference
  -> HashMap (GExpr a Int) Int -- ^ hashmap of new GExprs to their new Int references
  -> IntMap Int -- ^ intmap of old int reference to new int references
  -> Int -- ^ next free index
  -> (Int, HashMap (GExpr a Int) Int, IntMap Int, Int)
insertOldGExprBinary gnum mul kxOld kyOld lookupOldGExpr hm0 oldNodeToNewNode0 nextFreeInt0 = let
  (kx, hm1, oldNodeToNewNode1,nextFreeInt1) = insertOldNode kxOld lookupOldGExpr hm0 oldNodeToNewNode0 nextFreeInt0
  (ky, hm2, oldNodeToNewNode2,nextFreeInt2) = insertOldNode kyOld lookupOldGExpr hm1 oldNodeToNewNode1 nextFreeInt1
  newGExpr = gnum (mul kx ky)
  in
   cseInsert newGExpr hm2 oldNodeToNewNode2 nextFreeInt2

insertOldGExprUnary ::
  (Eq a, Hashable a)
  => (f -> GExpr a Int)
  -> (Int -> f)
  -> Int
  -> (Int -> GExpr a Int) -- ^ function to lookup old GExpr from old Int reference
  -> HashMap (GExpr a Int) Int -- ^ hashmap of new GExprs to their new Int references
  -> IntMap Int -- ^ intmap of old int reference to new int references
  -> Int -- ^ next free index
  -> (Int, HashMap (GExpr a Int) Int, IntMap Int, Int)
insertOldGExprUnary gnum neg kxOld lookupOldGExpr hm0 oldNodeToNewNode0 nextFreeInt0 = let
  (kx, hm1, oldNodeToNewNode1,nextFreeInt1) = insertOldNode kxOld lookupOldGExpr hm0 oldNodeToNewNode0 nextFreeInt0
  newGExpr = gnum (neg kx)
  in
   cseInsert newGExpr hm1 oldNodeToNewNode1 nextFreeInt1

cseInsert :: (Eq a, Hashable a) => GExpr a Int -> HashMap (GExpr a Int) Int -> IntMap Int -> Int
             -> (Int, HashMap (GExpr a Int) Int, IntMap Int, Int)
cseInsert gexpr hm0 oldNodeToNewNode0 nextFreeInt0 = case HM.lookup gexpr hm0 of
  Just k -> (k, hm0, oldNodeToNewNode0, nextFreeInt0)
  Nothing -> (nextFreeInt0, hm1, oldNodeToNewNode0, nextFreeInt0+1)
    where
      hm1 = HM.insert gexpr nextFreeInt0 hm0
