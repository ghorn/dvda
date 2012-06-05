{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}

module Dvda.Dot ( Dot(..)
                ) where

import Data.Array.Repa(DIM0,DIM1,DIM2,Z(..),(:.)(..), listOfShape, Shape)


class (Shape sh1, Shape sh2, Shape (DotT sh1 sh2)) => Dot sh1 sh2 where
  type DotT sh1 sh2
  dotDims :: sh1 -> sh2 -> DotT sh1 sh2

instance Dot DIM2 DIM2 where -- matrix-matrix
  type DotT DIM2 DIM2 = DIM2
  dotDims sh1 sh2 
    | c1 == r2  = Z :. r1 :. c2
    | otherwise = error $ "MM dimension mismatch: " ++ show sh1' ++ ", " ++ show sh2'
    where
      sh1'@[r1,c1] = reverse $ listOfShape sh1
      sh2'@[r2,c2] = reverse $ listOfShape sh2
  
instance Dot DIM1 DIM1 where -- vector-vector
  type DotT DIM1 DIM1 = DIM0
  dotDims sh1 sh2 
    | r1 == r2  = Z
    | otherwise = error $ "VV dimension mismatch: " ++ show sh1' ++ ", " ++ show sh2'
    where
      sh1'@[r1] = listOfShape sh1
      sh2'@[r2] = listOfShape sh2

instance Dot DIM2 DIM1 where -- matrix-vector
  type DotT DIM2 DIM1 = DIM1
  dotDims sh1 sh2 
    | c1 == r2  = Z :. r1
    | otherwise = error $ "MV dimension mismatch: " ++ show sh1' ++ ", " ++ show sh2'
    where
      sh1'@[r1,c1] = reverse $ listOfShape sh1
      sh2'@[r2]    = reverse $ listOfShape sh2

instance Dot DIM1 DIM2 where -- vector-matrix
  type DotT DIM1 DIM2 = DIM1
  dotDims sh1 sh2 
    | c1 == r2  = Z :. c2
    | otherwise = error $ "VM dimension mismatch: " ++ show sh1' ++ ", " ++ show sh2'
    where
      sh1'@[c1]    = reverse $ listOfShape sh1
      sh2'@[r2,c2] = reverse $ listOfShape sh2

