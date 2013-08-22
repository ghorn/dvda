{-# OPTIONS_GHC -Wall #-}

module Dvda.Alg ( Algorithm(..)
                , AlgOp(..)
                , InputIdx(..)
                , OutputIdx(..)
                , toAlg
                , runAlg
                , squashWorkVec
                , squashIsSame
                ) where

import Control.Monad.ST ( runST )
import qualified Data.Foldable as F
import Data.Maybe ( fromMaybe )
import qualified Data.Traversable as T
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM
import Data.Hashable ( Hashable(..) )
import Data.Vector.Generic ( (!) )
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

import Dvda.Expr
import Dvda.FunGraph ( FunGraph(..), Node(..), toFunGraph )

newtype InputIdx = InputIdx Int deriving Show
newtype OutputIdx = OutputIdx Int deriving Show

data AlgOp a = InputOp  Node InputIdx
             | OutputOp Node OutputIdx
             | NormalOp Node (GExpr a Node)
             deriving Show

data Algorithm a = Algorithm { algInDims :: Int
                             , algOutDims :: Int
                             , algOps :: [AlgOp a]
                             , algWorkSize :: Int
                             }

newtype LiveNode = LiveNode Int deriving Show
newtype NodeMap a = NodeMap (IM.IntMap a) deriving Show
nmEmpty :: NodeMap a
nmEmpty = NodeMap IM.empty

nmInsertWith :: (a -> a -> a) -> Node -> a -> NodeMap a -> NodeMap a
nmInsertWith f (Node k) v (NodeMap im) = NodeMap (IM.insertWith f k v im)

nmLookup :: Node -> NodeMap a -> Maybe a
nmLookup (Node k) (NodeMap im) = IM.lookup k im

nmInsert :: Node -> a -> NodeMap a -> NodeMap a
nmInsert (Node k) val (NodeMap im) = NodeMap $ IM.insert k val im

squashWorkVec' :: NodeMap Int -> NodeMap LiveNode -> [LiveNode] -> [AlgOp a] -> [AlgOp a]
squashWorkVec' accessMap liveMap0 (LiveNode pool0:pools) (InputOp k inIdx:xs) =
  InputOp (Node pool0) inIdx : squashWorkVec' accessMap liveMap pools xs
  where
    -- input to the the first element of the live pool
    -- update the liveMap to reflect this
    -- update the pool to reflect this
    liveMap = nmInsertWith err k (LiveNode pool0) liveMap0
    err = error "SSA node written to more than once"
squashWorkVec' accessMap0 liveMap0 pool0 (OutputOp k outIdx:xs) =
  OutputOp (Node lk) outIdx : squashWorkVec' accessMap liveMap0 pool xs
  where
    -- output from node looked up from live variables
    (LiveNode lk) = fromMaybe noLiveErr (nmLookup k liveMap0)
      where noLiveErr = error "OutputOp couldn't find node in live map"
    -- decrement access map, if references are now zero, add live node back to pool
    (accessMap, pool) = case nmLookup k accessMap0 of
      Just 0 -> error "squashWorkVec': accessed something with 0 references"
      Just 1 -> (nmInsert k 0 accessMap0, LiveNode lk:pool0)
      Just n -> (nmInsert k (n-1) accessMap0, pool0)
      Nothing -> error "squashWorkVec': node not in access map"
squashWorkVec' accessMap0 liveMap0 pool0 (NormalOp k gexpr0:xs) =
  NormalOp (Node retLiveK) gexpr : squashWorkVec' accessMap liveMap pool xs
  where
    decrement (am0, p0) depk = case nmLookup depk am0 of
      Just 0 -> error "squashWorkVec': accessed something with 0 references"
      Just 1 -> ((nmInsert depk     0 am0, LiveNode lk:p0), Node lk)
      Just n -> ((nmInsert depk (n-1) am0,             p0), Node lk)
      Nothing -> error "squashWorkVec': node not in access map"
      where
        LiveNode lk = fromMaybe (error "depsLiveKs missing") (nmLookup depk liveMap0)

    ((accessMap, LiveNode retLiveK:pool), gexpr) =
      T.mapAccumL decrement (accessMap0, pool0) gexpr0
    liveMap = nmInsert k (LiveNode retLiveK) liveMap0
squashWorkVec' _ _ _ [] = []
squashWorkVec' _ _ [] _ = error "squashWorkVec': empty pool"


squashWorkVec :: Algorithm a -> Algorithm a
squashWorkVec alg = Algorithm { algOps = newAlgOps
                              , algInDims = algInDims alg
                              , algOutDims = algOutDims alg
                              , algWorkSize = workVectorSize newAlgOps
                              }
  where
    addOne k = nmInsertWith (+) k (1::Int)
    countAccesses accMap  (InputOp _ _:xs) = countAccesses accMap xs
    countAccesses accMap  (OutputOp k _:xs) = countAccesses (addOne k accMap) xs
    countAccesses accMap0 (NormalOp _ gexpr:xs) = countAccesses accMap xs
      where
        accMap = F.foldr addOne accMap0 gexpr
    countAccesses accMap [] = accMap
    accesses = countAccesses nmEmpty (algOps alg)

    newAlgOps = squashWorkVec' accesses nmEmpty (map LiveNode [0..]) (algOps alg)


graphToAlg :: [(Node,GExpr a Node)] -> V.Vector (Sym,InputIdx) -> V.Vector (Node,OutputIdx)
              -> [AlgOp a]
graphToAlg rgr0 inSyms outIdxs = f rgr0
  where
    inSymMap = HM.fromList (F.toList inSyms :: [(Sym,InputIdx)])
    outIdxMap = IM.fromList (map (\(Node k, x) -> (k, x)) (F.toList outIdxs :: [(Node,OutputIdx)]))

    f ((k@(Node k'),GSym s):xs) = case HM.lookup s inSymMap of
      Nothing -> error "toAlg: symbolic is not in inputs"
      Just inIdx -> case IM.lookup k' outIdxMap of
        -- sym is an input
        Nothing -> InputOp k inIdx : f xs
        -- sym is an input and an output
        Just outIdx -> InputOp k inIdx : OutputOp k outIdx : f xs
    f ((k@(Node k'),x):xs) = case IM.lookup k' outIdxMap of
      -- no input or output
      Nothing -> NormalOp k x : f xs
      -- output only
      Just outIdx -> NormalOp k x : OutputOp k outIdx : f xs
    f [] = []

workVectorSize :: [AlgOp a] -> Int
workVectorSize = workVectorSize' (-1)
  where
    workVectorSize' n (NormalOp (Node m) _:xs) = workVectorSize' (max n m) xs
    workVectorSize' n (InputOp  (Node m) _:xs) = workVectorSize' (max n m) xs
    workVectorSize' n (OutputOp (Node m) _:xs) = workVectorSize' (max n m) xs
    workVectorSize' n [] = n+1

toAlg :: (Eq a, Show a, Hashable a) => V.Vector (Expr a) -> V.Vector (Expr a) -> IO (Algorithm a)
toAlg inputVecs outputVecs = do
  fg <- toFunGraph inputVecs outputVecs
  let inputIdxs  = V.map (\(k,x) -> (x,  InputIdx k)) (V.indexed ( fgInputs fg))
      outputIdxs = V.map (\(k,x) -> (x, OutputIdx k)) (V.indexed (fgOutputs fg))
      ops = graphToAlg (fgReified fg) inputIdxs outputIdxs
  return Algorithm { algInDims  = V.length inputIdxs
                   , algOutDims = V.length outputIdxs
                   , algOps = ops
                   , algWorkSize = workVectorSize ops
                   }

runAlg ::  G.Vector v a => Algorithm a -> v a -> v a
runAlg alg vIns
  | G.length vIns /= algInDims alg = error "runAlg: input dimension mismatch"
  | otherwise = runST $ do
    let asTypeOf' :: G.Vector v a => v a -> m ((G.Mutable v) s a) -> m ((G.Mutable v) s a)
        asTypeOf' _ x = x
    work <- asTypeOf' vIns $ GM.new (algWorkSize alg)
    vOuts <- GM.new (algOutDims alg)

    let bin (Node k) (Node kx) (Node ky) f = do
          x <- GM.read work kx
          y <- GM.read work ky
          GM.write work k (f x y)
        un (Node k) (Node kx) f = do
          x <- GM.read work kx
          GM.write work k (f x)
        runMe (InputOp (Node k) (InputIdx i)) = GM.write work k (vIns ! i)
        runMe (OutputOp (Node k) (OutputIdx i)) = do
          x <- GM.read work k
          GM.write vOuts i x
        runMe (NormalOp (Node k) (GConst c))        = GM.write work k c
        runMe (NormalOp (Node k) (GNum (FromInteger x))) = GM.write work k (fromIntegral x)
        runMe (NormalOp (Node k) (GFractional (FromRational x))) = GM.write work k (fromRational x)

        runMe (NormalOp k (GNum (Mul x y)))  = bin k x y (*)
        runMe (NormalOp k (GNum (Add x y)))  = bin k x y (+)
        runMe (NormalOp k (GNum (Sub x y)))  = bin k x y (-)
        runMe (NormalOp k (GNum (Negate x))) = un k x negate
        runMe (NormalOp k (GFractional (Div x y)))   = bin k x y (/)

        runMe (NormalOp k (GNum (Abs x)))            = un k x abs
        runMe (NormalOp k (GNum (Signum x)))         = un k x signum
        runMe (NormalOp k (GFloating (Pow x y)))     = bin k x y (**)
        runMe (NormalOp k (GFloating (LogBase x y))) = bin k x y logBase
        runMe (NormalOp k (GFloating (Exp x)))       = un k x exp
        runMe (NormalOp k (GFloating (Log x)))       = un k x log
        runMe (NormalOp k (GFloating (Sin x)))       = un k x sin
        runMe (NormalOp k (GFloating (Cos x)))       = un k x cos
        runMe (NormalOp k (GFloating (ASin x)))      = un k x asin
        runMe (NormalOp k (GFloating (ATan x)))      = un k x atan
        runMe (NormalOp k (GFloating (ACos x)))      = un k x acos
        runMe (NormalOp k (GFloating (Sinh x)))      = un k x sinh
        runMe (NormalOp k (GFloating (Cosh x)))      = un k x cosh
        runMe (NormalOp k (GFloating (Tanh x)))      = un k x tanh
        runMe (NormalOp k (GFloating (ASinh x)))     = un k x asinh
        runMe (NormalOp k (GFloating (ATanh x)))     = un k x atanh
        runMe (NormalOp k (GFloating (ACosh x)))     = un k x acosh
        runMe (NormalOp _ (GSym _)) = error "runAlg: there's symbol in my algorithm"
    mapM_ runMe (algOps alg)
    G.freeze vOuts

squashIsSame :: (Eq (v a), G.Vector v a) => v a -> Algorithm a -> Bool
squashIsSame x alg = runAlg alg x == runAlg (squashWorkVec alg) x
