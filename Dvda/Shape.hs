{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveDataTypeable #-}

-- | This was blatantly lifted from Repa, it includes trimmed down Data.Array.Repa.Shape and Data.Array.Repa.Index
module Dvda.Shape
       ( Shape
       , rank
       , size
       , listOfShape
       , shapeOfList
         -- * Index types
       , Z (..)
       , (:.) (..)

         -- * Common dimensions.
       , DIM0, DIM1, DIM2, DIM3, DIM4, DIM5
       ) where

import Data.Data ( Data, Typeable, Typeable2 )

-- Shape ----------------------------------------------------------------------
-- | Class of types that can be used as array shapes and indices.
class (Eq sh, Typeable sh, Data sh) => Shape sh where

        -- | Get the number of dimensions in a shape.
        rank    :: sh -> Int

        -- | Get the total number of elements in an array with this shape.
        size    :: sh -> Int

        -- | Check whether this shape is small enough so that its flat
        --      indices an be represented as `Int`. If this returns `False` then your
        --      array is too big. Mostly used for writing QuickCheck tests.
        sizeIsValid :: sh -> Bool

        -- | Convert a shape into its list of dimensions.
        listOfShape     :: sh -> [Int]

        -- | Convert a list of dimensions to a shape
        shapeOfList     :: [Int] -> sh

        -- | Ensure that a shape is completely evaluated.
        infixr 0 `deepSeq`
        deepSeq :: sh -> a -> a


stage :: String
stage   = "Data.Array.Repa.Index"

-- | An index of dimension zero
data Z  = Z
        deriving (Show, Read, Eq, Ord)
deriving instance Typeable Z
deriving instance Data Z

-- | Our index type, used for both shapes and indices.
infixl 3 :.
data tail :. head
        = !tail :. !head
        deriving (Show, Read, Eq, Ord)
deriving instance Typeable2 (:.)
deriving instance (Data a, Data b) => Data (a :. b)


-- Common dimensions
type DIM0       = Z
type DIM1       = DIM0 :. Int
type DIM2       = DIM1 :. Int
type DIM3       = DIM2 :. Int
type DIM4       = DIM3 :. Int
type DIM5       = DIM4 :. Int


-- Shape ----------------------------------------------------------------------
instance Shape Z where
        {-# INLINE [1] rank #-}
        rank _                  = 0

        {-# INLINE [1] size #-}
        size _                  = 1

        {-# INLINE [1] sizeIsValid #-}
        sizeIsValid _           = True

        {-# NOINLINE listOfShape #-}
        listOfShape _           = []

        {-# NOINLINE shapeOfList #-}
        shapeOfList []          = Z
        shapeOfList _           = error $ stage ++ ".fromList: non-empty list when converting to Z."

        {-# INLINE deepSeq #-}
        deepSeq Z x             = x


instance Shape sh => Shape (sh :. Int) where
        {-# INLINE [1] rank #-}
        rank   (sh  :. _)
                = rank sh + 1

        {-# INLINE [1] size #-}
        size  (sh1 :. n)
                = size sh1 * n

        {-# INLINE [1] sizeIsValid #-}
        sizeIsValid (sh1 :. n)
                | size sh1 > 0
                = n <= maxBound `div` size sh1

                | otherwise
                = False

        {-# NOINLINE listOfShape #-}
        listOfShape (sh :. n)
         = n : listOfShape sh

        {-# NOINLINE shapeOfList #-}
        shapeOfList xx
         = case xx of
                []      -> error $ stage ++ ".toList: empty list when converting to  (_ :. Int)"
                x:xs    -> shapeOfList xs :. x

        {-# INLINE deepSeq #-}
        deepSeq (sh :. n) x = deepSeq sh (n `seq` x)
