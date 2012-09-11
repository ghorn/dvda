{-# OPTIONS_GHC -Wall #-}
{-# Language TypeOperators #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}

module Dvda.HList ( (:*)(..)
                  , MVS(..)
                  , MVSList(..)
                  ) where

import Control.Applicative ( (<$>), liftA2 )
import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import Data.Traversable ( Traversable )
import qualified Data.Traversable as T

---- | matrix or vector or scalar
data MVS a = Mat [[a]] | Vec [a] | Sca a deriving Show

instance Functor MVS where
  fmap f (Sca x)  = Sca (f x)
  fmap f (Vec xs) = Vec (map f xs)
  fmap f (Mat xs) = Mat (map (map f) xs)

instance Foldable MVS where
  foldr f x0 (Sca x)  = foldr f x0 [x]
  foldr f x0 (Vec xs) = foldr f x0 xs
  foldr f x0 (Mat xs) = foldr f x0 (concat xs)

instance Traversable MVS where
  traverse f (Sca x)  = Sca <$> f x
  traverse f (Vec xs) = Vec <$> T.traverse f xs
  traverse f (Mat xs) = Mat <$> T.traverse (T.traverse f) xs

class MVSList a b where
  toMVSList :: a b -> [MVS b]

instance MVSList MVS a where
  toMVSList x = [x]
instance (MVSList f a, MVSList g a) => MVSList (f :* g) a where
  toMVSList (x :* y) = toMVSList x ++ toMVSList y

data (f :* g) a = (f a) :* (g a) deriving Show
infixr 6 :*
instance (Functor f, Functor g) => Functor (f :* g) where
  fmap f (x :* y) = fmap f x :* fmap f y
instance (Foldable f, Foldable g) => Foldable (f :* g) where
  foldr f acc0 (x :* y) = F.foldr f (F.foldr f acc0 y) x
instance (Traversable f, Traversable g) => Traversable (f :* g) where
  traverse f (x :* y) = liftA2 (:*) (T.traverse f x) (T.traverse f y)
