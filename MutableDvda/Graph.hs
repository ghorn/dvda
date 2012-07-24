{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}


module MutableDvda.Graph ( GExpr(..)
                         , GraphRef(..)
                         , toGExprs
                         , unsafeToGExprs
                         ) where

import Data.Hashable ( Hashable, hash, combine )
import Control.Concurrent.MVar

import Dvda.HashMap ( HashMap )
import qualified Dvda.HashMap as HM

import MutableDvda.Expr

data GExpr a where
  GSym :: String -> GExpr a
  GConst :: a -> GExpr a
  GNum :: Nums GraphRef -> GExpr a
  GFractional :: Fractionals GraphRef -> GExpr a
  GFloating :: Floatings GraphRef -> GExpr a

deriving instance Show a => Show (GExpr a)
deriving instance Eq a => Eq (GExpr a)

instance Hashable a => Hashable (GExpr a) where
  hash (GSym name)     = hash "GSym"        `combine` hash name
  hash (GConst x)      = hash "GConst"      `combine` hash x
  hash (GNum x)        = hash "GNum"        `combine` hash x
  hash (GFractional x) = hash "GFractional" `combine` hash x
  hash (GFloating x)   = hash "GFloating"   `combine` hash x

resetGraphRefs :: MVar (Expr a) -> IO ()
resetGraphRefs mv = do
  expr <- takeMVar mv
  case expr of (EGraphRef e _) -> putMVar mv e
               _               -> error "shouldn't ever try to resetGraphRefs on non EGraphRef"

-- | This version consumes the Exprs as a side effect, so only use it internally if you generate the Exprs youself and can discard them after calling unsafeToGExprs
unsafeToGExprs :: (Eq a, Hashable a) => [Expr a] -> IO ([GraphRef], Int, HashMap (GExpr a) GraphRef, [MVar (Expr a)])
unsafeToGExprs exprs = f ([], 0, HM.empty, []) exprs
  where
    f ret [] = return ret
    f (grefs, n0, hm0, mv0) (e:es) = do
      (gref, n, hm, mv) <- insert hm0 n0 e
      f (grefs ++ [gref], n, hm, mv0 ++ mv) es

-- | This version is way slower but it is safe to use multiple times.
toGExprs :: (Hashable a, Eq a) => [Expr a] -> IO ([GraphRef], Int, HashMap (GExpr a) GraphRef)
toGExprs exprs = do
  (grefs, n, hm, mvs) <- unsafeToGExprs exprs
  mapM_ resetGraphRefs mvs
  return (grefs,n,hm)

insert :: (Eq a, Hashable a) => HashMap (GExpr a) GraphRef
          -> Int -> Expr a -> IO (GraphRef, Int, HashMap (GExpr a) GraphRef, [MVar (Expr a)])
insert hm n (EGraphRef _ gr)               = return (gr, n, hm, [])
insert hm n (ESym name)                    = return $ tupleAppend [] $ cseInsert hm n (GSym name)
insert hm n (EConst c)                     = return $ tupleAppend [] $ cseInsert hm n (GConst c)
insert hm n (ENum (Mul x y))               = binaryInsert hm n GNum Mul x y
insert hm n (ENum (Add x y))               = binaryInsert hm n GNum Add x y
insert hm n (ENum (Sub x y))               = binaryInsert hm n GNum Sub x y
insert hm n (ENum (Negate x))              = unaryInsert hm n GNum Negate x
insert hm n (ENum (Abs x))                 = unaryInsert hm n GNum Abs x
insert hm n (ENum (Signum x))              = unaryInsert hm n GNum Signum x
insert hm n (ENum (FromInteger x))         = return $ tupleAppend [] $ cseInsert hm n (GNum (FromInteger x))
insert hm n (EFractional (Div x y))        = binaryInsert hm n GFractional Div x y
insert hm n (EFractional (FromRational x)) = return $ tupleAppend [] $ cseInsert hm n (GFractional (FromRational x))
insert hm n (EFloating (Pow x y))          = binaryInsert hm n GFloating Pow x y
insert hm n (EFloating (LogBase x y))      = binaryInsert hm n GFloating LogBase x y
insert hm n (EFloating (Exp x))            = unaryInsert hm n GFloating Exp x
insert hm n (EFloating (Log x))            = unaryInsert hm n GFloating Log x
insert hm n (EFloating (Sin x))            = unaryInsert hm n GFloating Sin x
insert hm n (EFloating (Cos x))            = unaryInsert hm n GFloating Cos x
insert hm n (EFloating (ASin x))           = unaryInsert hm n GFloating ASin x
insert hm n (EFloating (ATan x))           = unaryInsert hm n GFloating ATan x
insert hm n (EFloating (ACos x))           = unaryInsert hm n GFloating ACos x
insert hm n (EFloating (Sinh x))           = unaryInsert hm n GFloating Sinh x
insert hm n (EFloating (Cosh x))           = unaryInsert hm n GFloating Cosh x
insert hm n (EFloating (Tanh x))           = unaryInsert hm n GFloating Tanh x
insert hm n (EFloating (ASinh x))          = unaryInsert hm n GFloating ASinh x
insert hm n (EFloating (ATanh x))          = unaryInsert hm n GFloating ATanh x
insert hm n (EFloating (ACosh x))          = unaryInsert hm n GFloating ACosh x
insert hm0 n0 (ERef mv) = do
  e <- takeMVar mv
  case e of (EGraphRef _ gr) -> do -- reuse node
              putMVar mv e
              return (gr, n0, hm0, [])
            _ -> do -- insert and memoize new node
              (gr, n, hm, mvs) <- insert hm0 n0 e
              putMVar mv (EGraphRef e gr)
              return (gr, n, hm, mv:mvs)

tupleAppend :: d -> (a,b,c) -> (a,b,c,d)
tupleAppend w (x,y,z) = (x,y,z,w)

binaryInsert
  :: (Eq a, Hashable a) =>
     HashMap (GExpr a) GraphRef
     -> Int
     -> (t -> GExpr a)
     -> (GraphRef -> GraphRef -> t)
     -> Expr a
     -> Expr a
     -> IO (GraphRef, Int, HashMap (GExpr a) GraphRef, [MVar (Expr a)])
binaryInsert hm0 n0 gnum mul x y = do
  (kx,n1,hm1,mv1) <- insert hm0 n0 x
  (ky,n2,hm2,mv2) <- insert hm1 n1 y
  return $ tupleAppend (mv1++mv2) $ cseInsert hm2 n2 (gnum (mul kx ky))

unaryInsert
  :: (Eq a, Hashable a) =>
     HashMap (GExpr a) GraphRef
     -> Int
     -> (nf GraphRef -> GExpr a)
     -> (GraphRef -> nf GraphRef)
     -> Expr a
     -> IO (GraphRef, Int, HashMap (GExpr a) GraphRef, [MVar (Expr a)])
unaryInsert hm0 n0 gnum mul x = do
  (k,n1,hm1,mv1) <- insert hm0 n0 x
  return $ tupleAppend mv1 $ cseInsert hm1 n1 (gnum (mul k))
  
cseInsert :: (Eq k, Hashable k) => HashMap k GraphRef -> Int -> k -> (GraphRef, Int, HashMap k GraphRef)
cseInsert hm n gexpr = case HM.lookup gexpr hm of
  Nothing -> (GraphRef n, n+1, HM.insert gexpr (GraphRef n) hm)
  Just k -> (k, n, hm)
