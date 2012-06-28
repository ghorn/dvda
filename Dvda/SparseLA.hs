{-# OPTIONS_GHC -Wall #-}

module Dvda.SparseLA ( SparseVec(..)
                     , SparseMat(..)
                     , svFromList
                     , smFromLists
                     , svFromSparseList
                     , smFromSparseList
                     , svSize
                     , smSize
                     , svMap
                     , smMap
                     , svBinary
                     , smBinary
                     , svAdd
                     , svSub
                     , svMul
                     , smAdd
                     , smSub
                     , smMul
                     , svScale
                     , smScale
                     , getRow
                     , getCol
                     , svCat
                     , svCats
                     , sVV
                     , sMV
                     ) where

import Data.List ( foldl' )
import Data.Maybe ( fromJust, fromMaybe ) --, isNothing )
--import qualified Data.Traversable as T
import Data.Array.Repa ( DIM1, DIM2, (:.)(..), Z(..) )
import qualified Data.Array.Repa as Repa
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM

-- map from row to (map from col to value)
data SparseMat a = SparseMat DIM2 (IntMap (IntMap a))

instance Show a => Show (SparseMat a) where
  show (SparseMat s xs) = "SparseMat " ++ show vals ++ " " ++ show (rows, cols)
    where
      [cols,rows] = Repa.listOfShape s
      vals = concatMap f (IM.toList xs)
      f (row,m) = map g (IM.toList m)
        where
          g (col, val) = ((row, col), val)
    
instance Num a => Num (SparseMat a) where
  x + y = fromJust $ smAdd x y
  x - y = fromJust $ smSub x y
  x * y = fromJust $ smMul x y
  abs = smMap abs
  signum = smMap signum

smSize :: SparseMat a -> (Int,Int)
smSize (SparseMat sh _) = (\[cols,rows] -> (rows,cols)) $ Repa.listOfShape sh

smMap :: (a -> b) -> SparseMat a -> SparseMat b
smMap f (SparseMat sh maps) = SparseMat sh (IM.map (IM.map f) maps)

smFromLists :: [[a]] -> SparseMat a
smFromLists blah = smFromSparseList sparseList (rows, cols)
  where
    rows = length blah
    cols = length (head blah)
    sparseList = concat $ zipWith (\row xs -> zipWith (\col x -> ((row,col),x)) [0..] xs) [0..] blah

smFromSparseList :: [((Int,Int),a)] -> (Int,Int) -> SparseMat a
smFromSparseList xs' (rows,cols) = SparseMat (Repa.shapeOfList [cols, rows]) (foldr f IM.empty xs')
  where
    f ((row,col), val) = IM.insertWith g row (IM.singleton col val)
      where
        g = IM.union
--        g = IM.unionWith (error $ "smFromList got 2 values for entry: "++show (row,col))

---- more efficient using mergeWithKey, but needs containers 0.5 so wait till ghc 7.6 :(
-- smBinary :: (a -> b -> c) -> (IntMap a -> IntMap c) -> (IntMap b -> IntMap c)
--             -> SparseMat a -> SparseMat b -> Maybe (SparseMat c)
-- smBinary fBoth fLeft fRight (SparseMat shx xs) (SparseMat shy ys)
--   | shx /= shy = Nothing
--   | isNothing merged = Nothing
--   | otherwise = Just $ SparseMat shx (fromJust merged)
--   where
--     merged = T.sequence $ IM.mergeWithKey f (IM.map (Just . fLeft)) (IM.map (Just . fRight)) xs ys
--       where
--         cols = Repa.shapeOfList [head $ Repa.listOfShape shx]
--         f _ x y = case svBinary fBoth fLeft fRight (SparseVec cols x) (SparseVec cols y) of
--           Just (SparseVec _ im) -> Just (Just im)
--           Nothing -> Just Nothing

smBinary :: (a -> a -> a) -> (IntMap a -> IntMap a) -> (IntMap a -> IntMap a)
            -> SparseMat a -> SparseMat a -> Maybe (SparseMat a)
smBinary fBoth fLeft fRight (SparseMat shx xs) (SparseMat shy ys)
  | shx /= shy = Nothing
  | otherwise = Just $ SparseMat shx merged
  where
    merged = IM.unionWith f (IM.map fLeft xs) (IM.map fRight ys)
      where
        cols = Repa.shapeOfList [head $ Repa.listOfShape shx]
        f x y = case svBinary fBoth fLeft fRight (SparseVec cols x) (SparseVec cols y) of
          Just (SparseVec _ im) -> im
          Nothing -> error "goons everywhere"

--------------------------------------------------------------------------------------
data SparseVec a = SparseVec DIM1 (IntMap a)

svSize :: SparseVec a -> Int
svSize (SparseVec sh _) = head $ Repa.listOfShape sh

instance Show a => Show (SparseVec a) where
  show sv@(SparseVec _ xs) = "SparseVec " ++ show vals ++ " " ++ show rows
    where
      rows = svSize sv
      vals = IM.toList xs

instance Num a => Num (SparseVec a) where
  x + y = fromJust $ svAdd x y
  x - y = fromJust $ svSub x y
  x * y = fromJust $ svMul x y
  abs = svMap abs
  signum = svMap signum

svFromList :: [a] -> SparseVec a
svFromList xs = svFromSparseList (zip [0..] xs) (length xs)

svFromSparseList :: [(Int,a)] -> Int -> SparseVec a
svFromSparseList xs rows = SparseVec (Repa.shapeOfList [rows]) (IM.fromList xs)

svMap :: (a -> b) -> SparseVec a -> SparseVec b
svMap f (SparseVec sh maps) = SparseVec sh (IM.map f maps)

svBinary :: (a -> b -> c) -> (IntMap a -> IntMap c) -> (IntMap b -> IntMap c)
            -> SparseVec a -> SparseVec b -> Maybe (SparseVec c)
svBinary fBoth fLeft fRight (SparseVec shx xs) (SparseVec shy ys)
  | shx /= shy = Nothing
  | otherwise = Just $ SparseVec shx merged
  where
    -- more efficient using mergeWithKey, but needs containers 0.5 so wait till ghc 7.6 :(
--    merged = IM.mergeWithKey (\_ x y -> Just (fBoth x y)) fLeft fRight xs ys
    merged = IM.unionWithKey f (fLeft xs) (fRight ys)
      where
        f k _ _ = fBoth (fromJust $ IM.lookup k xs) (fromJust $ IM.lookup k ys)

---------------------------------------------------------------------------
svAdd :: Num a => SparseVec a -> SparseVec a -> Maybe (SparseVec a)
svAdd = svBinary (+) id id

svSub :: Num a => SparseVec a -> SparseVec a -> Maybe (SparseVec a)
svSub = svBinary (-) id (IM.map negate)

svMul :: Num a => SparseVec a -> SparseVec a -> Maybe (SparseVec a)
svMul = svBinary (*) (\_ -> IM.empty) (\_ -> IM.empty)


smAdd :: Num a => SparseMat a -> SparseMat a -> Maybe (SparseMat a)
smAdd = smBinary (+) id id

smSub :: Num a => SparseMat a -> SparseMat a -> Maybe (SparseMat a)
smSub = smBinary (-) id (IM.map negate)

smMul :: Num a => SparseMat a -> SparseMat a -> Maybe (SparseMat a)
smMul = smBinary (*) (\_ -> IM.empty) (\_ -> IM.empty)

--------------------------------------------------------------------------

svScale :: Num a => a -> SparseVec a -> SparseVec a
svScale x (SparseVec sh xs) = SparseVec sh (IM.map (x *) xs)

smScale :: Num a => a -> SparseMat a -> SparseMat a
smScale x (SparseMat sh xs) = SparseMat sh (IM.map (IM.map (x *)) xs)


--------------------------------------------------------------------------
getRow :: Int -> SparseMat a -> SparseVec a
getRow row sm@(SparseMat sh xs)
  | row >= (\(rows,_) -> rows) (smSize sm) =
    error $ "getRow saw out of bounds index " ++ show row ++ " for matrix size " ++ show (smSize sm)
  | otherwise = SparseVec cols out
  where
    out = fromMaybe IM.empty (IM.lookup row xs)
    cols = Repa.shapeOfList [head $ Repa.listOfShape sh]

getCol :: Int -> SparseMat a -> SparseVec a
getCol col sm@(SparseMat sh xs)
  | col >= (\(_,cols) -> cols) (smSize sm) =
    error $ "getCol saw out of bounds index " ++ show col ++ " for matrix size " ++ show (smSize sm)
  | otherwise = SparseVec rows out
  where
    out = IM.mapMaybe (IM.lookup col) xs
    rows = Repa.shapeOfList [last $ Repa.listOfShape sh]

---------------------------------------------------------------------------
sVV :: Num a => SparseVec a -> SparseVec a -> Maybe a
sVV x y = fmap (\(SparseVec _ xs) -> sum (IM.elems xs)) (svMul x y)

sMV :: Num a => SparseMat a -> SparseVec a -> Maybe (SparseVec a)
sMV (SparseMat shm ms) vec@(SparseVec shv _)
  | mcols /= vsize = Nothing
  | otherwise = Just $ SparseVec (Repa.shapeOfList [mrows]) out
  where
    [mrows,mcols] = reverse $ Repa.listOfShape shm
    vsize = Repa.size shv
    out = IM.mapMaybe f ms
      where
        f im = sVV (SparseVec (Repa.shapeOfList [mcols]) im) vec

---------------------------------------------------------------------------
svCat :: SparseVec a -> SparseVec a -> SparseVec a
svCat svx@(SparseVec _ xs) svy@(SparseVec _ ys) = SparseVec (Repa.shapeOfList [shx + shy]) (IM.union xs newYs)
  where
    shx = svSize svx
    shy = svSize svy
    newYs = IM.fromList $ map (\(k,x) -> (k+shx, x)) $ IM.toList ys

svCats :: [SparseVec a] -> SparseVec a
svCats [] = SparseVec (Z :. 0) IM.empty
svCats (xs0:xs) = foldl' svCat xs0 xs

--mx' :: SparseMat Double
--mx' = smFromList [((0,0), 10), ((0,2), 20), ((1,0), 30)] (2,3)
--
--my' :: SparseMat Double
--my' = smFromList [((0,0), 1), ((0,1), 7)] (2,3)
--
--x' :: SparseVec Int
--x' = svFromList [(0,10), (1, 20)] 4
--
--y' :: SparseVec Int
--y' = svFromList [(0,7), (3, 30)] 4
