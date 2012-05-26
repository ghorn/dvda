{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module Dvda.Expr ( Expr(..)
                 , FromGExpr
                 , sym
                 , vsym
                 , msym
                 , vec
                 , mat
                 , scale
                 , dot
                 , diff
                 , grad
                 , jacob
                 , hess
                 , dim
                 , exprOfGExpr
                 ) where

import Data.Array.Repa(DIM0,DIM1,DIM2,Z(..),(:.)(..), listOfShape, Shape(rank), shapeOfList)
import qualified Data.Vector.Unboxed as V
import Data.IntMap ( Key )

import Dvda.Dot ( Dot(..), dotDims )
import Dvda.BinUn ( BinOp(..), UnOp(..), showBinary, showUnary )
import Dvda.GExpr ( GExpr(..) )
import Dvda.HomoDim ( HomoDim, shapeOfHomo )

showShapeR :: Shape sh => sh -> String
showShapeR = show . reverse . listOfShape

class Shape sh => FromGExpr sh where
  fromMM :: HomoDim -> HomoDim -> Key -> Key -> Expr sh a
  fromMV :: HomoDim -> HomoDim -> Key -> Key -> Expr sh a
  fromVM :: HomoDim -> HomoDim -> Key -> Key -> Expr sh a
  fromVV :: HomoDim -> HomoDim -> Key -> Key -> Expr sh a
  fromMM shx shy = error $ "sorry, no fromMM instance for: " ++ show shx ++ ", " ++ show shy
  fromMV shx shy = error $ "sorry, no fromMV instance for: " ++ show shx ++ ", " ++ show shy
  fromVM shx shy = error $ "sorry, no fromVM instance for: " ++ show shx ++ ", " ++ show shy
  fromVV shx shy = error $ "sorry, no fromVV instance for: " ++ show shx ++ ", " ++ show shy

instance FromGExpr DIM2 where
  fromMM shx shy kx ky = EDot (ERef (shapeOfHomo shx :: DIM2) kx) (ERef (shapeOfHomo shy :: DIM2) ky)

instance FromGExpr DIM1 where
  fromMV shx shy kx ky = EDot (ERef (shapeOfHomo shx :: DIM2) kx) (ERef (shapeOfHomo shy :: DIM1) ky)
  fromVM shx shy kx ky = EDot (ERef (shapeOfHomo shx :: DIM1) kx) (ERef (shapeOfHomo shy :: DIM2) ky)

instance FromGExpr DIM0 where
  fromVV shx shy kx ky = EDot (ERef (shapeOfHomo shx :: DIM1) kx) (ERef (shapeOfHomo shy :: DIM1) ky)

dim :: Expr sh a -> sh
dim (ESym sh _) = sh
dim (EConst sh _) = sh
dim (EDimensionless _) = error "EDimensionless doesn't have a dimension, ya goon"
dim (ESingleton sh _) = sh
dim (EUnary _ x) = dim x
dim (EBinary _ x1 _) = dim x1
dim (EScale _ y) = dim y
dim (EDot x y) = dotDims (dim x) (dim y)
dim (ERef sh _) = sh
dim (EDeriv _ _) = Z
dim (EGrad _ args) = dim args
dim (EJacob x args) = Z :. head (listOfShape (dim x)) :. head (listOfShape (dim args))

exprOfGExpr :: (Shape sh, V.Unbox a, FromGExpr sh) => GExpr a -> Expr sh a
exprOfGExpr (GBinary sh' op kx ky) = EBinary op (ERef sh kx) (ERef sh ky)
  where
    sh = shapeOfHomo sh'
exprOfGExpr (GUnary sh op kx) = EUnary op (ERef (shapeOfHomo sh) kx)
exprOfGExpr (GSym sh name) = ESym (shapeOfHomo sh) name
exprOfGExpr (GSingleton sh a) = ESingleton (shapeOfHomo sh) a
exprOfGExpr (GScale sh kx ky) = EScale (ERef Z kx) (ERef (shapeOfHomo sh) ky)
exprOfGExpr (GConst sh v) = EConst (shapeOfHomo sh) v
exprOfGExpr (GDot shx shy kx ky) = case (rank shx, rank shy) of
  (2,2) -> fromMM shx shy kx ky
  (2,1) -> fromMV shx shy kx ky
  (1,2) -> fromVM shx shy kx ky
  (1,1) -> fromVV shx shy kx ky
  nm    -> error $ "can't convert GDot of rank: " ++ show nm ++ " to Expr"

data Expr sh a where
  ESym :: sh -> String -> Expr sh a
  EConst :: V.Unbox a => sh -> V.Vector a -> Expr sh a
  EDimensionless :: a -> Expr sh a
  ESingleton :: sh -> a -> Expr sh a
  EUnary :: UnOp -> Expr sh a -> Expr sh a
  EBinary :: BinOp -> Expr sh a -> Expr sh a -> Expr sh a
  EScale :: Expr DIM0 a -> Expr sh a -> Expr sh a
  EDot :: Dot sh1 sh2 => Expr sh1 a -> Expr sh2 a -> Expr (DotT sh1 sh2) a
  ERef :: sh -> Int -> Expr sh a

  EDeriv :: sh ~ DIM0 => Expr DIM0 a -> Expr DIM0 a -> Expr sh a
  EGrad  :: sh ~ DIM1 => Expr DIM0 a -> Expr DIM1 a -> Expr sh a
  EJacob :: sh ~ DIM2 => Expr DIM1 a -> Expr DIM1 a -> Expr sh a

isVal :: Eq a => a -> Expr sh a -> Bool
isVal x (EDimensionless y) = x == y
isVal x (ESingleton _ y) = x == y
isVal _ _ = False

-- | first layer of binary simplification: infer dimension of EDimensionless if possible
makeBinary :: (Num a, Eq a, Shape sh) => BinOp -> (a -> a -> a) -> Expr sh a -> Expr sh a -> Expr sh a
-- | can't infer dimension, just apply operation
makeBinary _  f (EDimensionless x) (EDimensionless y) = EDimensionless (f x y)
-- | infer dimension, then call makeBinary' for further simplification
makeBinary op f (EDimensionless x) y = makeBinary' op f (ESingleton (dim y) x) y
makeBinary op f x (EDimensionless y) = makeBinary' op f x (ESingleton (dim x) y)
-- | dimension inferred, call makeBinary'
makeBinary op f x y = makeBinary' op f x y

-- | second layer of binary simplification: check dimensions
makeBinary' :: (Num a, Eq a, Shape sh) => BinOp -> (a -> a -> a) -> Expr sh a -> Expr sh a -> Expr sh a
makeBinary' op f x y
  | shx == shy  = makeBinary'' op f x y
  | otherwise = error $ "Binary op \""++ sop ++"\" dimension mismatch ya goon (" ++ sdx ++ ", " ++ sdy ++ ")"
  where
    shx = dim x
    shy = dim y
    sdx = showShapeR shx
    sdy = showShapeR shy
    sop = show op

-- | third layer of binary simplification: 0*x == x*0 == 0
-- |                                       1*x == x*1 == x
-- |                                       0+x == x+0 == x
-- |                                       x/0 == error
-- |                                       x/1 == x
-- |                                       0/x == 0
-- |                                       x - 0 == 0
-- |                                       0 - x == neg x
makeBinary'' :: (Num a, Eq a, Shape sh) => BinOp -> (a -> a -> a) -> Expr sh a -> Expr sh a -> Expr sh a
makeBinary'' Mul f x y
  | isVal 0 x = x
  | isVal 0 y = y
  | isVal 1 x = y
  | isVal 1 y = x
  | otherwise = makeBinary''' Mul f x y
makeBinary'' Add f x y
  | isVal 0 x = y
  | isVal 0 y = x
  | otherwise = makeBinary''' Add f x y
makeBinary'' Div f x y
  | isVal 0 y = error "divide by zero"
  | isVal 1 y = x
  | isVal 0 x = x
  | otherwise = makeBinary''' Div f x y
makeBinary'' Sub f x y
  | isVal 0 x = negate y
  | isVal 0 y = x
  | otherwise = makeBinary''' Sub f x y

-- | apply operation to constant vectors
makeBinary'' _ f (EConst sh x) (EConst _ y) = EConst sh (V.zipWith f x y)
-- | broadcast constant operations
makeBinary'' _ f (ESingleton _ x) (EConst sh y) = EConst sh (V.map (f x) y)
makeBinary'' _ f (EConst sh x) (ESingleton _ y) = EConst sh (V.map (`f` y) x)
-- | otherwise make symbolic binary
makeBinary'' op _ x y = EBinary op x y

-- | fourth layer of binary simplification: make reasonable simplifications
makeBinary''' :: Shape sh => BinOp -> (a -> a -> a) -> Expr sh a -> Expr sh a -> Expr sh a
-- | apply operation to constant vectors
makeBinary''' _ f (EConst sh x) (EConst _ y) = EConst sh (V.zipWith f x y)
-- | broadcast constant operations
makeBinary''' _ f (ESingleton _ x) (EConst sh y) = EConst sh (V.map (f x) y)
makeBinary''' _ f (EConst sh x) (ESingleton _ y) = EConst sh (V.map (`f` y) x)
-- | otherwise make symbolic binary
makeBinary''' op _ x y = EBinary op x y


-- | apply unary operations on constants
makeUnary :: Shape sh => UnOp -> (a -> a) -> Expr sh a -> Expr sh a
makeUnary _ f (EDimensionless x) = EDimensionless (f x)
makeUnary _ f (ESingleton sh x) = ESingleton sh (f x)
makeUnary _ f (EConst sh x) = EConst sh (V.map f x)
makeUnary op _ x = EUnary op x

instance (Shape sh, Num a, Eq a) => Num (Expr sh a) where
  (*) = makeBinary Mul (*)
  (+) = makeBinary Add (+)
  (-) = makeBinary Sub (-)
  abs = makeUnary Abs abs
  signum = makeUnary Signum signum
  fromInteger = EDimensionless . fromInteger

instance (Shape sh, Fractional a, Eq a) => Fractional (Expr sh a) where
  (/) = makeBinary Div (/)
  fromRational = EDimensionless . fromRational

instance (Shape sh, Floating a, Eq a) => Floating (Expr sh a) where
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

paren :: Show a => a -> String
paren x = "( "++show x++" )"

instance (Shape sh, Show a) => Show (Expr sh a) where
  show (ESingleton _ x) = show x
  show (EDimensionless x) = show x
  show (ESym sh name) = name++"{"++showShapeR sh++"}"
  show (EConst sh x) = "{" ++ showShapeR sh ++ ", "++show (V.toList x)++"}" 
  show (EUnary op x) = showUnary x op
  show (EBinary op x y) = paren x ++ showBinary op ++ paren y
  show (EScale s x) = paren s ++ "*" ++ paren x
  show (EDot _ _) = "EDot ?? ??"
  show (ERef sh k) = "{ref:" ++ showShapeR sh ++ ":" ++ show k ++ "}"
  show (EDeriv x y) = "deriv(" ++ show x ++ ", " ++ show y ++ ")"
  show (EGrad  x y) = "grad("  ++ show x ++ ", " ++ show y ++ ")"
  show (EJacob x y) = "jacob(" ++ show x ++ ", " ++ show y ++ ")"

sym :: String -> Expr DIM0 a
sym = ESym Z

vsym :: Int -> String -> Expr DIM1 a
vsym k = ESym (Z :. k)

msym :: (Int,Int) -> String -> Expr DIM2 a
msym (r,c) = ESym (Z :. r :. c)

vec :: V.Unbox a => [a] -> Expr DIM1 a
vec xs = EConst (shapeOfList [length xs]) (V.fromList xs)

mat :: V.Unbox a => (Int,Int) -> [a] -> Expr DIM2 a
mat (r,c) xs 
  | r*c == length xs = EConst (shapeOfList [c,r]) (V.fromList xs)
  | otherwise = error "bad dims in mat"

scale :: Expr DIM0 a -> Expr sh a -> Expr sh a
scale = EScale

dot :: (Dot sh1 sh2, DotT sh1 sh2 ~ sh) => Expr sh1 a -> Expr sh2 a -> Expr sh a
dot = EDot

diff :: Expr DIM0 a -> Expr DIM0 a -> Expr DIM0 a
diff = EDeriv

grad :: Expr DIM0 a -> Expr DIM1 a -> Expr DIM1 a
grad = EGrad

jacob :: Expr DIM1 a -> Expr DIM1 a -> Expr DIM2 a
jacob = EJacob

hess :: Expr DIM0 a -> Expr DIM1 a -> Expr DIM2 a
hess expr args = jacob (grad expr args) args
