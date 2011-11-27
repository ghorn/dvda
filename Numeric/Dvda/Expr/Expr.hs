-- Expr.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE PolymorphicComponents, ExistentialQuantification #-}
-- {-# LANGUAGE FlexibleInstances, RankNTypes #-}  -- , ExistentialQuantification #-}
-- {-# LANGUAGE FlexibleInstances, RankNTypes, ExistentialQuantification #-}
-- {-# LANGUAGE TypeOperators #-} -- needed for einsum

module Numeric.Dvda.Expr.Expr( Expr(..)
                             , dot
                             , applyDot
                             , symbolic
                             , symTensor
                             ) where

import Data.Ratio(numerator, denominator)
import Data.GraphViz(Labellable(..))
import Data.Text.Lazy(pack)
import qualified Data.Array.Repa as R

import Numeric.Dvda.Expr.BinaryType
import Numeric.Dvda.Expr.UnaryType
import Numeric.Dvda.Expr.Scalar


data Expr sh a = Sym { name :: String
                     , dim :: sh
                     }
               | EScalar (Scalar a)
               | Broadcast { arg' :: Scalar a
                           , dim :: sh
                           }
               | Tensor (R.Array sh a)
               | Unary { unaryType :: UnaryType
                       , arg :: Expr sh a
                       }
               | Binary { binaryType :: BinaryType
                        , arg1 :: Expr sh a
                        , arg2 :: Expr sh a
                        }
               | forall sh1 sh2 . (R.Shape sh1, R.Shape sh2, Dottable sh1 sh2 sh) =>
                 Dot { arg1' :: Expr sh1 a
                     , arg2' :: Expr sh2 a
                     , dim :: sh
                     }


getDim :: Expr sh a -> sh
getDim (Sym { dim = d}) = d
getDim (EScalar _) = error "can't call getDim on EScalar"
getDim (Broadcast {dim = d}) = d
getDim (Tensor a) = R.extent a
getDim (Unary { arg = a }) = getDim a
getDim (Binary { arg1 = a }) = getDim a
getDim (Dot { dim = d }) = d


class Dottable a b c | a b -> c where
  dot :: (Num e, R.Elt e) => Expr a e -> Expr b e -> Expr c e
  applyDot :: (Num e, R.Elt e) => R.Array a e -> R.Array b e -> R.Array c e

-- vector `dot` vector = scalar
instance Dottable R.DIM1 R.DIM1 R.DIM0 where
  {-# INLINE dot #-}
  dot x y = Dot { arg1' = x, arg2' = y, dim = R.Z }
  applyDot x y = R.force $ R.sum $ R.zipWith (*) x y

---- matrix `dot` vector = vector
--instance (Num a, R.Elt a) => Dottable (Expr R.DIM2 a) (Expr R.DIM1 a) (Expr R.DIM1 a) where
--  {-# INLINE dot #-}
--  dot x y
--    | n == n' = Dot { arg1' = x, arg2' = y, dim = (R.Z :. m) }
--    | otherwise = error $ unlines $ [ "??? Error using ==> dot"
--                                    , "Inner matrix dimensions must agree."
--                                    , "dot( " ++ show (m,n) ++ ", " ++ show n' ++ ")"
--                                    ]
--    where
--      (R.Z :. n :. m) = getDim x
--      (R.Z :. n') = getDim y
---- vector `dot` matrix = vector
--instance (Num a, R.Elt a) => Dottable (Expr R.DIM1 a) (Expr R.DIM2 a) (Expr R.DIM1 a) where
--  {-# INLINE dot #-}
--  dot x y
--    | m == m' = Dot { arg1' = x, arg2' = y, dim = (R.Z :. n) }
--    | otherwise = error $ unlines $ [ "??? Error using ==> dot"
--                                    , "Inner matrix dimensions must agree."
--                                    , "dot( " ++ show m' ++ ", " ++ show (m,n) ++ ")"
--                                    ]
--    where
--      (R.Z :. m') = getDim x
--      (R.Z :. n :. m) = getDim y
--
---- matrix `dot` matrix = matrix
--instance Dottable R.DIM2 R.DIM2 R.DIM2 where
--  {-# INLINE dot #-}
--  dot x y
--    | nx == my = Dot { arg1' = x, arg2' = y, dim = (R.Z :. mx :. ny) }
--    | otherwise = error $ unlines $ [ "??? Error using ==> dot"
--                                    , "Inner matrix dimensions must agree."
--                                    , "dot( " ++ show (mx, nx) ++ ", " ++ show (my,ny) ++ ")"
--                                    ]
--    where
--      (R.Z :. nx :. mx) = getDim x
--      (R.Z :. ny :. my) = getDim y
--  applyDot = mmMult





--f :: Expr R.DIM0 Double
--f = (x `dot` y) + 4.3
----f = head [4.3, (x `dot` y)]
--  where
--    x = Tensor $ R.fromList (R.Z :. (3::Int)) [0,1,2::Double]
--    y = Tensor $ R.fromList (R.Z :. (3::Int)) [0,1,2::Double]
--

-- Eq instance
instance (R.Shape sh, R.Elt a, Eq a) => Eq (Expr sh a) where
  (==) (Sym {name = n1, dim = dim1}) (Sym {name = n2, dim = dim2}) = and [n1 == n2, dim1 == dim2]
  (==) (EScalar x) (EScalar y) = (x == y)
  (==) (Tensor x) (Tensor y) = (x == y)
  (==) (Broadcast {arg' = a1, dim = dim1}) (Broadcast {arg' = a2, dim = dim2}) = and [a1 == a2, dim1 == dim2]
  (==) (Unary { unaryType = t0, arg = a1 }) (Unary { unaryType = t1, arg = a2 }) = and [t0 == t1, a1 == a2]
  (==) (Binary { binaryType = bt0, arg1 = a01, arg2 = a02 }) (Binary { binaryType = bt1, arg1 = a11, arg2 = a12 }) = and [bt0 == bt1, a01 == a11, a02 == a12]
--  (==) x@(Dot {arg1' = dx1, arg2' = dx2}) y@(Dot {arg1' = dy1, arg2' = dy2}) = and [dx1 == dy1, dx2 == dy2, getDim x == getDim y]
  (==) _ _ = False
  

instance (R.Shape sh, R.Elt a) => Show (Expr sh a) where
  show sym@(Sym {}) = show (name sym)
  show (EScalar s) = show s
  show (Broadcast {arg' = a, dim = d}) = "broadcast(" ++ R.showShape d ++ ": "++ show a ++ ")"
  show (Tensor t) = show t
  show ew@(Unary {}) = show (unaryType ew) ++ "(" ++ show (arg ew) ++ ")"
  show binary@(Binary {}) = "( " ++ show (arg1 binary) ++" "++ show (binaryType binary) ++" "++ show (arg2 binary) ++ " )"
  show (Dot {arg1' = a1, arg2' = a2}) = "dot( " ++ show a1 ++ ", " ++ show a2 ++ " )"


isScalar :: Expr sh a -> Bool
isScalar (EScalar _) = True
isScalar _ = False

toBinary :: Eq sh => (Expr sh a, Expr sh a) -> BinaryType -> Expr sh a 
toBinary (x,y) binType
  | or [isScalar x, isScalar y, getDim x == getDim y] = Binary {binaryType = binType, arg1 = x, arg2 = y}
  | otherwise      = error $ "type mismatch in toBinary (" ++ show binType ++ ")"

broadcast :: Scalar a -> Expr sh a -> Expr sh a
broadcast scalar expr = Broadcast { arg' = scalar
                                  , dim = getDim expr
                                  }

instance (R.Shape sh, R.Elt a, Num a) => Num (Expr sh a) where
  -- (+)
  (EScalar x) + (EScalar y) = EScalar (x + y)
  (EScalar x) + y = (broadcast x y) + y
  x + (EScalar y) = x + (broadcast y x)
  x + y = toBinary (x,y) Add
  
  -- (*)
  (EScalar x) * (EScalar y) = EScalar (x * y)
  (EScalar x) * y = (broadcast x y) * y
  x * (EScalar y) = x * (broadcast y x)
  x * y = toBinary (x,y) Mul

  -- (-)
  (EScalar x) - (EScalar y) = EScalar (x - y)
  (EScalar x) - y = (broadcast x y) - y
  x - (EScalar y) = x - (broadcast y x)
  x - y = toBinary (x,y) Sub

  negate x = Unary {unaryType = Neg, arg = x}
  abs x = Unary {unaryType = Abs, arg = x}
  signum x = Unary {unaryType = Signum, arg = x}
  fromInteger x = EScalar $ I (fromInteger x)

instance (Fractional a, R.Elt a, R.Shape sh) => Fractional (Expr sh a) where
  (EScalar x) / (EScalar y) = EScalar (x / y)
  (EScalar x) / y = (broadcast x y) / y
  x / (EScalar y) = x / (broadcast y x)
  x / y = toBinary (x,y) Div

  fromRational x = num / den
    where
      num = fromIntegral $ numerator x
      den = fromIntegral $ denominator x


instance (Floating a, R.Elt a, R.Shape sh) => Floating (Expr sh a) where
  pi = EScalar $ N pi
  
  exp x  = Unary { unaryType = Exp,  arg = x }
  sqrt x = Unary { unaryType = Sqrt, arg = x }
  log x  = Unary { unaryType = Log,  arg = x }
  
  x**y = toBinary (x,y) Pow
  logBase x y = toBinary (x,y) LogBase
  
  sin x = Unary { unaryType = Sin, arg = x }
  cos x = Unary { unaryType = Cos, arg = x }
  tan x = Unary { unaryType = Tan, arg = x }
                   
  asin x = Unary { unaryType = ASin, arg = x }
  acos x = Unary { unaryType = ACos, arg = x }
  atan x = Unary { unaryType = ATan, arg = x }

  sinh x = Unary { unaryType = Sinh, arg = x }
  cosh x = Unary { unaryType = Cosh, arg = x }
  tanh x = Unary { unaryType = Tanh, arg = x }

  asinh x = Unary { unaryType = ASinh, arg = x }
  acosh x = Unary { unaryType = ACosh, arg = x }
  atanh x = Unary { unaryType = ATanh, arg = x }


instance (Show a, Num a, R.Elt a, Show sh, R.Shape sh) => Labellable (Expr sh a) where
  toLabelValue go = toLabelValue $ pack $ show go ++ "["++show (getDim go)++"]"

symbolic :: String -> Expr R.Z a
symbolic name' = Sym {name = name', dim = R.Z}

symTensor :: R.Shape sh => String -> [Int] -> Expr sh a
symTensor name' dim' = Sym {name = name', dim = R.shapeOfList dim'}
