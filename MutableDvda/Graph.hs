{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
 {-# Language FlexibleContexts #-}
{-# Language StandaloneDeriving #-}


module MutableDvda.Graph ( GExpr(..)
                         , GraphRef(..)
                         , toGExprs
                         ) where

import Data.Hashable ( Hashable, hash, combine )

import Dvda.HashMap ( HashMap )
import qualified Dvda.HashMap as HM

import MutableDvda.Expr
import MutableDvda.SharedVar

data GraphRef = GraphRef Int deriving Eq
instance Hashable GraphRef where
  hash (GraphRef k) = hash "GraphRef" `combine` k

data GExpr a where
  GRef :: Int -> GExpr a
  GSym :: String -> GExpr a
  GConst :: a -> GExpr a
  GNum :: Nums GraphRef -> GExpr a
  GFractional :: Fractionals GraphRef -> GExpr a
  GFloating :: Floatings GraphRef -> GExpr a

deriving instance Eq a => Eq (GExpr a)

instance Hashable a => Hashable (GExpr a) where
  hash (GRef _)      = error "don't hash a GRef"
  hash (GSym name)     = hash "GSym"        `combine` hash name
  hash (GConst x)      = hash "GConst"      `combine` hash x
  hash (GNum x)        = hash "GNum"        `combine` hash x
  hash (GFractional x) = hash "GFractional" `combine` hash x
  hash (GFloating x)   = hash "GFloating"   `combine` hash x


toGExprs :: (Hashable a, Eq a) => [Expr a] -> SVMonad ([GraphRef], Int, HashMap (GExpr a) GraphRef)
toGExprs exprs = f ([], 0, HM.empty) exprs
  where
    f ret [] = return ret
    f (grefs, n0, hm0) (e:es) = do
      (gref, n, hm) <- insert hm0 n0 e
      f (grefs ++ [gref], n, hm) es

insert :: (Eq a, Hashable a) => HashMap (GExpr a) GraphRef
          -> Int -> Expr a -> SVMonad (GraphRef, Int, HashMap (GExpr a) GraphRef)
insert hm n (ESym name)                    = return $ cseInsert hm n (GSym name)
insert hm n (EConst c)                     = return $ cseInsert hm n (GConst c)
insert hm n (ENum (Mul x y))               = binaryInsert hm n GNum Mul x y
insert hm n (ENum (Add x y))               = binaryInsert hm n GNum Add x y
insert hm n (ENum (Sub x y))               = binaryInsert hm n GNum Sub x y
insert hm n (ENum (Negate x))              = unaryInsert hm n GNum Negate x
insert hm n (ENum (Abs x))                 = unaryInsert hm n GNum Abs x
insert hm n (ENum (Signum x))              = unaryInsert hm n GNum Signum x
insert hm n (ENum (FromInteger x))         = return $ cseInsert hm n (GNum (FromInteger x))
insert hm n (EFractional (Div x y))        = binaryInsert hm n GFractional Div x y
insert hm n (EFractional (FromRational x)) = return $ cseInsert hm n (GFractional (FromRational x))
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
insert hm n e@(ERef _) = do -- if the 
  e' <- readExpr e
  insert hm n e'

binaryInsert
  :: (Eq a, Hashable a) =>
     HashMap (GExpr a) GraphRef
     -> Int
     -> (t -> GExpr a)
     -> (GraphRef -> GraphRef -> t)
     -> Expr a
     -> Expr a
     -> SVMonad (GraphRef, Int, HashMap (GExpr a) GraphRef)
binaryInsert hm0 n0 gnum mul x y = do
  (kx,n1,hm1) <- insert hm0 n0 x
  (ky,n2,hm2) <- insert hm1 n1 y
  return $ cseInsert hm2 n2 (gnum (mul kx ky))
unaryInsert
  :: (Eq a, Hashable a) =>
     HashMap (GExpr a) GraphRef
     -> Int
     -> (nf GraphRef -> GExpr a)
     -> (GraphRef -> nf GraphRef)
     -> Expr a
     -> SVMonad (GraphRef, Int, HashMap (GExpr a) GraphRef)
unaryInsert hm0 n0 gnum mul x = do
  (k,n1,hm1) <- insert hm0 n0 x
  return $ cseInsert hm1 n1 (gnum (mul k))
  
cseInsert :: (Eq k, Hashable k) => HashMap k GraphRef -> Int -> k -> (GraphRef, Int, HashMap k GraphRef)
cseInsert hm n gexpr = case HM.lookup gexpr hm of
  Nothing -> (GraphRef n, n+1, HM.insert gexpr (GraphRef n) hm)
  Just k -> (k, n, hm)
