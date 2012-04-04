{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}

module Ideas.Expr ( Expr(..)
                  , symE
                  , vsymE
                  , msymE
                  , dot
                  , diff
                  , grad
                  , jacob
                  , hess
                  , dim
                  , Dot(..)
                  ) where

import Data.Array.Repa(DIM0,DIM1,DIM2,Z(..),(:.)(..), showShape, listOfShape,shapeOfList,Shape) -- hiding ((++))
import Data.Vector.Unboxed(Vector, toList, Unbox)
import qualified Data.Vector.Unboxed as V(zipWith, map)
import Ideas.BinUn

dim :: Expr d a -> d
dim (ESym d _) = d
dim (EConst d _) = d
dim (EDimensionless _) = error "EDimensionless doesn't have a dimension, ya goon"
dim (ESingleton d _) = d
dim (EUnary _ x) = dim x
dim (EBinary _ x1 _) = dim x1
dim (EScale _ y) = dim y
dim (EDot x y) = dotDims (dim x) (dim y)
dim (ERef d _) = d
dim (EDeriv _ _) = Z
dim (EGrad _ args) = dim args
dim (EJacob x args) = shapeOfList $ listOfShape (dim x) ++ listOfShape (dim args)

data Expr d a where
  ESym :: d -> String -> Expr d a
  EConst :: Unbox a => d -> Vector a -> Expr d a
  EDimensionless :: a -> Expr d a
  ESingleton :: d -> a -> Expr d a
  EUnary :: UnOp -> Expr d a -> Expr d a
  EBinary :: BinOp -> Expr d a -> Expr d a -> Expr d a
  EScale :: Expr DIM0 a -> Expr d a -> Expr d a
  EDot :: (Dot d1 d2, d ~ DotT d1 d2) => Expr d1 a -> Expr d2 a -> Expr d a
  ERef :: d -> Int -> Expr d a

  EDeriv :: (d ~ DIM0) => Expr DIM0 a -> Expr DIM0 a -> Expr d a
  EGrad  :: (d ~ DIM1) => Expr DIM0 a -> Expr DIM1 a -> Expr d a
  EJacob :: (d ~ DIM2) => Expr DIM1 a -> Expr DIM1 a -> Expr d a

-- | first layer of binary simplification: infer dimension of EDimensionless if possible
makeBinary :: Shape d => BinOp -> (a -> a -> a) -> Expr d a -> Expr d a -> Expr d a
-- | can't infer dimension, just apply operation
makeBinary _  f (EDimensionless x) (EDimensionless y) = EDimensionless (f x y)
-- | infer dimension, then call makeBinary' for further simplification
makeBinary op f (EDimensionless x) y = makeBinary' op f (ESingleton (dim y) x) y
makeBinary op f x (EDimensionless y) = makeBinary' op f x (ESingleton (dim x) y)
-- | dimension inferred, call makeBinary'
makeBinary op f x y = makeBinary' op f x y

-- | second layer of binary simplification: check dimensions
makeBinary' :: Shape d => BinOp -> (a -> a -> a) -> Expr d a -> Expr d a -> Expr d a
makeBinary' op f x y
  | dx == dy  = makeBinary'' op f x y
  | otherwise = error $ "Binary op \""++ sop ++"\" dimension mismatch ya goon (" ++ sdx ++ ", " ++ sdy ++ ")"
  where
    dx = dim x
    dy = dim y
    sdx = showShape dx
    sdy = showShape dy
    sop = show op

-- | third layer of binary simplification: make reasonable simplifications
makeBinary'' :: Shape d => BinOp -> (a -> a -> a) -> Expr d a -> Expr d a -> Expr d a
-- | apply operation to constant vectors
makeBinary'' _ f (EConst d x) (EConst _ y) = EConst d (V.zipWith f x y)
-- | broadcast constant operations
makeBinary'' _ f (ESingleton _ x) (EConst d y) = EConst d (V.map (f x) y)
makeBinary'' _ f (EConst d x) (ESingleton _ y) = EConst d (V.map (`f` y) x)
-- | otherwise make symbolic binary
makeBinary'' op _ x y = EBinary op x y


-- | apply unary operations on constants
makeUnary :: Shape d => UnOp -> (a -> a) -> Expr d a -> Expr d a
makeUnary _ f (EDimensionless x) = EDimensionless (f x)
makeUnary _ f (ESingleton d x) = ESingleton d (f x)
makeUnary _ f (EConst d x) = EConst d (V.map f x)
makeUnary op _ x = EUnary op x

instance (Shape d, Num a) => Num (Expr d a) where
  (*) = makeBinary Mul (*)
  (+) = makeBinary Add (+)
  (-) = makeBinary Sub (-)
  abs = makeUnary Abs abs
  signum = makeUnary Signum signum
  fromInteger = EDimensionless . fromInteger

instance (Shape d, Fractional a) => Fractional (Expr d a) where
  (/) = makeBinary Div (/)
  fromRational = EDimensionless . fromRational

instance (Shape d, Floating a) => Floating (Expr d a) where
  pi    = EDimensionless pi
  (**)  = makeBinary Pow (**)
  exp   = makeUnary Exp exp
  log   = makeUnary Log log
  sin   = makeUnary Sin sin
  cos   = makeUnary Cos cos
  asin  = makeUnary ASin asin
  atan  = makeUnary ATan atan
  acos  = makeUnary ACos acos
  sinh  = makeUnary Sinh sinh
  cosh  = makeUnary Cosh cosh
  asinh = error "no instance for asinh"
  atanh = error "no instance for atanh"
  acosh = error "no instance for acosh"

class (Shape d1, Shape d2) => Dot d1 d2 where
  type DotT d1 d2
  dotDims :: d1 -> d2 -> DotT d1 d2
  
dot :: (Dot d1 d2, DotT d1 d2 ~ d) => Expr d1 a -> Expr d2 a -> Expr d a
dot = EDot

instance Dot DIM2 DIM2 where -- matrix-matrix
  type DotT DIM2 DIM2 = DIM2
  dotDims d1 d2 
    | c1 == r2  = shapeOfList [r1, c2]
    | otherwise = error "MM dimension mismatch"
    where
      [r1,c1] = listOfShape d1
      [r2,c2] = listOfShape d2
  
instance Dot DIM1 DIM1 where -- vector-vector
  type DotT DIM1 DIM1 = DIM0
  dotDims d1 d2 
    | r1 == r2  = Z
    | otherwise = error "VV dimension mismatch"
    where
      [r1] = listOfShape d1
      [r2] = listOfShape d2

instance Dot DIM2 DIM1 where -- matrix-vector
  type DotT DIM2 DIM1 = DIM1
  dotDims d1 d2 
    | c1 == r2  = shapeOfList [r1]
    | otherwise = error "MV dimension mismatch"
    where
      [r1,c1] = listOfShape d1
      [r2]    = listOfShape d2

instance Dot DIM1 DIM2 where -- vector-matrix
  type DotT DIM1 DIM2 = DIM1
  dotDims d1 d2 
    | c1 == r2  = shapeOfList [c2]
    | otherwise = error "VM dimension mismatch"
    where
      [c1]    = listOfShape d1
      [r2,c2] = listOfShape d2

paren :: Show a => a -> String
paren x = "( "++show x++" )"

instance (Shape d, Show a) => Show (Expr d a) where
  show (ESingleton _ x) = show x
  show (EDimensionless x) = show x
  show (ESym d name) = name++"{"++showShape d++"}"
  show (EConst d x) = "{" ++ showShape d ++ ", "++show (toList x)++"}" 
  show (EUnary op x) = showUnary x op
  show (EBinary op x y) = paren x ++ showBinary op ++ paren y
  show (EScale s x) = paren s ++ "*" ++ paren x
  show (EDot _ _) = "EDot ?? ??"
  show (ERef d k) = "{ref:" ++ showShape d ++ ":" ++ show k ++ "}"
  show (EDeriv x y) = "deriv(" ++ show x ++ ", " ++ show y ++ ")"
  show (EGrad  x y) = "grad("  ++ show x ++ ", " ++ show y ++ ")"
  show (EJacob x y) = "jacob(" ++ show x ++ ", " ++ show y ++ ")"

symE :: String -> Expr DIM0 a
symE = ESym Z

vsymE :: Int -> String -> Expr DIM1 a
vsymE k = ESym (Z :. k)

msymE :: (Int,Int) -> String -> Expr DIM2 a
msymE (r,c) = ESym (Z :. r :. c)

diff :: Expr DIM0 a -> Expr DIM0 a -> Expr DIM0 a
diff = EDeriv

grad :: Expr DIM0 a -> Expr DIM1 a -> Expr DIM1 a
grad = EGrad

jacob :: Expr DIM1 a -> Expr DIM1 a -> Expr DIM2 a
jacob = EJacob

hess :: Expr DIM0 a -> Expr DIM1 a -> Expr DIM2 a
hess expr args = jacob (grad expr args) args