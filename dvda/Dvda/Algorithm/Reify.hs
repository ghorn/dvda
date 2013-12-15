{-# OPTIONS_GHC -Wall #-}
{-# Language BangPatterns #-}
{-# Language ScopedTypeVariables #-}

-- | This file is a modified version from Andy Gill's data-reify package
--   It is modified to use Data.HashTable.IO, which gives a speed improvement
--   at the expense of portability. This also gives me a more convenient
--   sandbox to investigate other performance tweaks, though it is unclear
--   if I have made anything any faster.

module Dvda.Algorithm.Reify
       ( ReifyGraph(..)
       , Node(..)
       , reifyGraph
       ) where

import Control.Monad.State.Strict ( StateT(..), runStateT )
import Data.Hashable ( Hashable(..) )
import Control.Applicative ( pure )
import Data.Traversable ( Traversable )
import qualified Data.Traversable as T
import System.Mem.StableName ( StableName, makeStableName, hashStableName )
import Unsafe.Coerce ( unsafeCoerce )

import Dvda.Expr

import qualified Data.HashTable.IO as H
type HashTable k v = H.CuckooHashTable k v

newtype Node = Node Int deriving (Ord, Eq)

instance Show Node where
  show (Node k) = '@' : show k

data ReifyGraph e = ReifyGraph [(Node,e Node)]

mapAccumM' :: (Monad m, Functor m, Traversable t) =>
             (a -> b -> m (c, a)) -> a -> t b -> m (t c, a)
mapAccumM' f = flip (runStateT . T.traverse (StateT . flip f))
--{-# INLINE mapAccumM' #-}

mapAccumM :: (Monad m, Functor m, Traversable t) =>
             (a -> b -> m (a, c)) -> a -> t b -> m (t c, a)
mapAccumM f' = mapAccumM' f
  where
    f acc z = do
      (x,y) <- f' acc z
      return (y,x)
--{-# INLINE mapAccumM #-}

mapDeRef :: (acc -> Expr a -> IO (acc, Node)) -> acc -> Expr a -> IO (acc, GExpr a Node)
mapDeRef _ acc0 (ESym name) = pure (acc0, GSym name)
mapDeRef _ acc0 (EConst c)  = pure (acc0, GConst c)
mapDeRef f acc0 (ENum (Mul x y)) = do
  (acc1, fx) <- f acc0 x
  (acc2, fy) <- f acc1 y
  return (acc2, GNum (Mul fx fy))
mapDeRef f acc0 (ENum (Add x y)) = do
  (acc1, fx) <- f acc0 x
  (acc2, fy) <- f acc1 y
  return (acc2, GNum (Add fx fy))
mapDeRef f acc0 (ENum (Sub x y)) = do
  (acc1, fx) <- f acc0 x
  (acc2, fy) <- f acc1 y
  return (acc2, GNum (Sub fx fy))
mapDeRef f acc0 (ENum (Negate x)) = do
  (acc1, fx) <- f acc0 x
  return (acc1, GNum (Negate fx))
mapDeRef f acc0 (ENum (Abs x)) = do
  (acc1, fx) <- f acc0 x
  return (acc1, GNum (Abs fx))
mapDeRef f acc0 (ENum (Signum x)) = do
  (acc1, fx) <- f acc0 x
  return (acc1, GNum (Signum fx))
mapDeRef _ acc0 (ENum (FromInteger k)) = pure (acc0, GNum (FromInteger k))
mapDeRef f acc0 (EFractional (Div x y)) = do
  (acc1, fx) <- f acc0 x
  (acc2, fy) <- f acc1 y
  return (acc2, GFractional (Div fx fy))
mapDeRef _ acc0 (EFractional (FromRational x)) = pure (acc0, GFractional (FromRational x))
mapDeRef f acc0 (EFloating (Pow x y))     = do
  (acc1, fx) <- f acc0 x
  (acc2, fy) <- f acc1 y
  return (acc2, GFloating (Pow fx fy))
mapDeRef f acc0 (EFloating (LogBase x y)) = do
  (acc1, fx) <- f acc0 x
  (acc2, fy) <- f acc1 y
  return (acc2, GFloating (LogBase fx fy))
mapDeRef f acc0 (EFloating (Exp   x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (Exp fx))
mapDeRef f acc0 (EFloating (Log   x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (Log   fx))
mapDeRef f acc0 (EFloating (Sin   x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (Sin   fx))
mapDeRef f acc0 (EFloating (Cos   x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (Cos   fx))
mapDeRef f acc0 (EFloating (Tan   x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (Tan   fx))
mapDeRef f acc0 (EFloating (ASin  x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (ASin  fx))
mapDeRef f acc0 (EFloating (ATan  x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (ATan  fx))
mapDeRef f acc0 (EFloating (ACos  x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (ACos  fx))
mapDeRef f acc0 (EFloating (Sinh  x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (Sinh  fx))
mapDeRef f acc0 (EFloating (Cosh  x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (Cosh  fx))
mapDeRef f acc0 (EFloating (Tanh  x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (Tanh  fx))
mapDeRef f acc0 (EFloating (ASinh x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (ASinh fx))
mapDeRef f acc0 (EFloating (ATanh x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (ATanh fx))
mapDeRef f acc0 (EFloating (ACosh x))     = do
  (acc1, fx) <- f acc0 x
  return (acc1, GFloating (ACosh fx))
-- {-# INLINE mapDeRef #-}


reifyGraph :: forall t a . Traversable t => t (Expr a) -> IO (ReifyGraph (GExpr a), t Node)
reifyGraph m = do
  ht <- H.new :: IO (HashTable DynStableName Node)
  let findNodes :: ([(Node, GExpr a Node)],Node) -> Expr a ->
                    IO (([(Node, GExpr a Node)],Node), Node)
      findNodes !(!tab0, nextUnique@(Node nextUnique')) expr = do
        stableName <- makeDynStableName expr
        lu <- H.lookup ht stableName
        case lu of
          Just var -> return ((tab0,nextUnique), var)
          Nothing -> do
            let var = nextUnique
            H.insert ht stableName var
            ((tab1,nextNextUnique), res) <- mapDeRef findNodes (tab0, Node (nextUnique' + 1)) expr
            let tab2 :: [(Node,GExpr a Node)]
                tab2 = (var,res) : tab1
            return ((tab2,nextNextUnique), var)
      -- {-# INLINE findNodes #-}

  (root, (pairs,_)) <- mapAccumM findNodes ([], Node 0) m
  return (ReifyGraph pairs, root)


-- Stable names that not use phantom types.
-- As suggested by Ganesh Sittampalam.
newtype DynStableName = DynStableName (StableName ()) deriving Eq

instance Hashable DynStableName where
  hashWithSalt salt = (salt `hashWithSalt`) . hashDynStableName
hashDynStableName :: DynStableName -> Int
hashDynStableName (DynStableName sn) = hashStableName sn

makeDynStableName :: a -> IO DynStableName
makeDynStableName !a = do
  st <- makeStableName a
  return $ DynStableName (unsafeCoerce st)
--{-# INLINE makeDynStableName #-}
