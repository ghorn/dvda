{-# OPTIONS_GHC -Wall #-}

module Dvda.HomoDim ( HomoDim(..)
                    , homoOfShape
                    , shapeOfHomo
                    ) where

import Control.DeepSeq
import Data.Array.Repa ( Shape(..) )
import Data.Hashable ( Hashable, hash )

newtype HomoDim = HomoDim [Int] deriving (Eq, Show)

homoOfShape :: Shape sh => sh -> HomoDim
homoOfShape = shapeOfList . listOfShape

shapeOfHomo :: Shape sh => HomoDim -> sh
shapeOfHomo = shapeOfList . listOfShape

instance Hashable HomoDim where
  hash (HomoDim xs) = hash xs

instance Shape HomoDim where
  listOfShape (HomoDim xs) = xs
  shapeOfList = HomoDim
  deepSeq xs y = (listOfShape xs) `deepseq` y
  rank = length . listOfShape
  size = product . listOfShape
  zeroDim = shapeOfList []
  addDim x y = shapeOfList $ zipWith (+) (listOfShape x) (listOfShape y)
  unitDim = error "need to finish instancing Shape HomoDim"
  intersectDim = error "need to finish instancing Shape HomoDim"
  sizeIsValid = error "need to finish instancing Shape HomoDim"
  toIndex = error "need to finish instancing Shape HomoDim"
  fromIndex = error "need to finish instancing Shape HomoDim"
  inShapeRange = error "need to finish instancing Shape HomoDim"
