-- Expr.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Rank2Types #-}

module Numeric.Dvda.Expr.Expr( Expr(..)
                             , sym
                             , symVec
                             , symMat
                             , getSyms
                             , vec
                             , mat
                             , subs
                             , Children(..)
                             , getChildren
                             , showNode
                             , showType
                             , showDim
                             ) where

import Data.List(nubBy)
import Data.Maybe(isNothing, fromJust)

import Numeric.Dvda.Expr.Binary
import Numeric.Dvda.Expr.Unary

data Scalar a = SNum a
              | SSym String
              | SUnary (Unary (Scalar a))
              | SBinary (Binary (Scalar a))
              | SInt Int deriving Eq

data Vector a = VNum Int [a]
              | VSym Int String
              | VUnary (Unary (Vector a))
              | VBinary (Binary (Vector a))
              | VBroadcast Int (Scalar a) deriving Eq

data Matrix a = MNum (Int,Int) [a]
              | MSym (Int,Int) String
              | MUnary (Unary (Matrix a))
              | MBinary (Binary (Matrix a))
              | MBroadcast (Int,Int) (Scalar a) deriving Eq


instance Show a => Show (Scalar a) where
  show (SNum x) = show x
  show (SInt x) = show x
  show (SSym x) = x
  show (SUnary x) = show x
  show (SBinary x) = show x

instance Show a => Show (Vector a) where
  show (VNum _ x) = show x
  show (VSym _ x) = "{" ++ x ++ "}"
  show (VUnary x) = show x
  show (VBinary x) = show x
  show (VBroadcast d x) = "BC( " ++ show x ++ " -> " ++ show d ++ " )"

instance Show a => Show (Matrix a) where
  show (MNum _ x) = show x
  show (MSym _ x) = "{{" ++ x ++ "}}"
  show (MUnary x) = show x
  show (MBinary x) = show x
  show (MBroadcast d x) = "BC( " ++ show x ++ " -> " ++ show d ++ " )"

-- what will be displayed in the graphviz
sShowNode :: Show a => Scalar a -> String
--sShowNode (SNum x) = "{N}:" ++ show x
--sShowNode (SInt x) = "{I}:" ++ show x
sShowNode (SNum x) = show x
sShowNode (SInt x) = show x
sShowNode (SSym x) = x
sShowNode (SUnary (Unary unOp _)) = show unOp
sShowNode (SBinary (Binary binOp _ _)) = show binOp

vShowNode :: Show a => Vector a -> String
vShowNode x@(VNum _ _) = show x
vShowNode x@(VSym _ _) = show x
vShowNode (VBroadcast d _) = "BC["++ show d ++ "]"
vShowNode (VUnary (Unary unOp _)) = show unOp
vShowNode (VBinary (Binary binOp _ _)) = show binOp

mShowNode :: Show a => Matrix a -> String
mShowNode x@(MNum _ _) = show x
mShowNode x@(MSym _ _) = show x
mShowNode (MBroadcast d _) = "BC["++ show d ++ "]"
mShowNode (MUnary (Unary unOp _)) = show unOp
mShowNode (MBinary (Binary binOp _ _)) = show binOp

showNode :: Show a => Expr a -> String
showNode (EScalar x) = sShowNode x
showNode (EVector x) = vShowNode x
showNode (EMatrix x) = mShowNode x


showType :: Show a => Expr a -> String
showType (EScalar (SNum _)) = "N"
showType (EScalar (SInt _)) = "I"
showType _ = ""

showDim :: Show a => Expr a -> String
showDim (EScalar _) = ""
showDim (EVector x) = show $ [vecDim x]
showDim (EMatrix x) = show $ matDim x

------------------------------------------------------------
----------------- get symbolic variables -------------------
------------------------------------------------------------
sGetSyms :: Scalar a -> [Expr a]
sGetSyms (SNum _) = []
sGetSyms (SInt _) = []
sGetSyms x@(SSym _) = [EScalar x]
sGetSyms (SUnary (Unary _ x)) = sGetSyms x
sGetSyms (SBinary (Binary _ x y)) = sGetSyms x ++ sGetSyms y

vGetSyms :: Vector a -> [Expr a]
vGetSyms (VNum _ _) = []
vGetSyms x@(VSym _ _) = [EVector x]
vGetSyms (VUnary (Unary _ x)) = vGetSyms x
vGetSyms (VBinary (Binary _ x y)) = vGetSyms x ++ vGetSyms y
vGetSyms (VBroadcast _ x) = sGetSyms x

mGetSyms :: Matrix a -> [Expr a]
mGetSyms (MNum _ _) = []
mGetSyms x@(MSym _ _) = [EMatrix x]
mGetSyms (MUnary (Unary _ x)) = mGetSyms x
mGetSyms (MBinary (Binary _ x y)) = mGetSyms x ++ mGetSyms y
mGetSyms (MBroadcast _ x) = sGetSyms x

-- | get all the symbolic variables in an expression
getSyms :: Expr a -> [Expr a]
getSyms (EScalar x) = sGetSyms x
getSyms (EVector x) = vGetSyms x
getSyms (EMatrix x) = mGetSyms x


-- get dimensions of vector
vecDim :: Vector a -> Int
vecDim (VNum d _) = d
vecDim (VSym d _) = d
vecDim (VBroadcast d _) = d
vecDim (VUnary (Unary _ v)) = vecDim v
vecDim (VBinary (Binary _ vx vy)) 
  | vecDim vx == vecDim vy = vecDim vx
  | otherwise              = error "vecDim found mismatched dimensions in VBinary - this indicates the absence of proper checking during construction"

-- get dimensions of matrix (rows, cols)
matDim :: Matrix a -> (Int, Int)
matDim (MNum d _) = d
matDim (MSym d _) = d
matDim (MBroadcast d _) = d
matDim (MUnary (Unary _ m)) = matDim m
matDim (MBinary (Binary _ mx my)) 
  | matDim mx == matDim my = matDim mx
  | otherwise              = error "matDim found mismatched dimensions in MBinary - this indicates the absence of proper checking during construction"

-- test if scalar == SInt x
sIsI :: Int -> Scalar a -> Bool
sIsI i (SInt si) = i == si
sIsI _ _ = False

-- test if vector is broadcast from (SInt x)
vIsI :: Int -> Vector a -> Bool
vIsI i (VBroadcast _ s) = sIsI i s
vIsI _ _ = False

-- test if matrix is broadcast from (SInt x)
mIsI :: Int -> Matrix a -> Bool
mIsI i (MBroadcast _ s) = sIsI i s
mIsI _ _ = False


------------------------------------------------------------------------------
---------------------Scalar/Vector/Matrix Num instances-----------------------
------------------------------------------------------------------------------
instance Num a => Num (Scalar a) where
  fromInteger = error "API error: fromInteger (in Num a => Num (Vector a)) should not be accessible by the user"
  abs x 
    | sIsI 0 x = SInt 0
    | otherwise = SUnary (Unary Abs x)
  signum x 
    | sIsI 0 x = SInt 0
    | otherwise = SUnary (Unary Signum x)
  negate x 
    | sIsI 0 x = SInt 0
    | otherwise = SUnary (Unary Neg x)

  (SInt x) + (SInt y) = SInt $ x + y
  (SNum x) + (SNum y) = SNum $ x + y
  (SNum x) + (SInt y) = SNum $ x + (fromIntegral y)
  (SInt x) + (SNum y) = SNum $ (fromIntegral x) + y
  x + y
    | sIsI 0 x = y
    | sIsI 0 y = x
    | otherwise = SBinary (Binary Add x y)

  (SInt x) - (SInt y) = SInt $ x - y
  (SNum x) - (SNum y) = SNum $ x - y
  (SNum x) - (SInt y) = SNum $ x - (fromIntegral y)
  (SInt x) - (SNum y) = SNum $ (fromIntegral x) - y
  x - y
    | sIsI 0 x = negate y
    | sIsI 0 y = x
    | otherwise = SBinary (Binary Sub x y)

  (SInt x) * (SInt y) = SInt $ x * y
  (SNum x) * (SNum y) = SNum $ x * y
  (SNum x) * (SInt y) = SNum $ x * (fromIntegral y)
  (SInt x) * (SNum y) = SNum $ (fromIntegral x) * y
  x * y
    | sIsI 1 x = y
    | sIsI 1 y = x
    | sIsI 0 x || sIsI 0 y = SInt 0
    | otherwise = SBinary (Binary Add x y)


instance Num a => Num (Vector a) where
  fromInteger = error "API error: fromInteger (in Num a => Num (Vector a)) should not be accessible by the user"
  abs x 
    | vIsI 0 x = VBroadcast (vecDim x) (SInt 0)
    | otherwise = VUnary (Unary Abs x)
  signum x 
    | vIsI 0 x = VBroadcast (vecDim x) (SInt 0)
    | otherwise = VUnary (Unary Signum x)
  negate x 
    | vIsI 0 x = VBroadcast (vecDim x) (SInt 0)
    | otherwise = VUnary (Unary Neg x)

  (VNum dx xs) + (VNum _ ys) = VNum dx $ zipWith (+) xs ys
  x + y
    | vIsI 0 x = y
    | vIsI 0 y = x
    | otherwise = VBinary (Binary Add x y)
  x - y
    | vIsI 0 x = negate y
    | vIsI 0 y = x
    | otherwise = VBinary (Binary Sub x y)
  x * y
    | vIsI 1 x = y
    | vIsI 1 y = x
    | vIsI 0 x || vIsI 0 y = VBroadcast (vecDim x) (SInt 0)
    | otherwise = VBinary (Binary Mul x y)


instance Num a => Num (Matrix a) where
  fromInteger = error "API error: fromInteger (in Num a => Num (Matrix a)) should not be accessible by the user"
  abs x 
    | mIsI 0 x = MBroadcast (matDim x) (SInt 0)
    | otherwise = MUnary (Unary Abs x)
  signum x 
    | mIsI 0 x = MBroadcast (matDim x) (SInt 0)
    | otherwise = MUnary (Unary Signum x)
  negate x 
    | mIsI 0 x = MBroadcast (matDim x) (SInt 0)
    | otherwise = MUnary (Unary Neg x)
  (MNum dx xs) + (MNum _ ys) = MNum dx $ zipWith (+) xs ys
  x + y
    | mIsI 0 x = y
    | mIsI 0 y = x
    | otherwise = MBinary (Binary Add x y)
  x - y
    | mIsI 0 x = negate y
    | mIsI 0 y = x
    | otherwise = MBinary (Binary Sub x y)
  x * y
    | mIsI 1 x = y
    | mIsI 1 y = x
    | mIsI 0 x || mIsI 0 y = MBroadcast (matDim x) (SInt 0)
    | otherwise = MBinary (Binary Mul x y)


------------------------------------------------------------------------------
-----------------Scalar/Vector/Matrix Fractional instances--------------------
------------------------------------------------------------------------------
instance Fractional a => Fractional (Scalar a) where
  -- (SInt x) / (SInt y) = SNum $ fromRational (toInteger x % toInteger y)
  (SNum x) / (SNum y) = SNum $ x / y
  (SNum x) / (SInt y) = SNum $ x / (fromIntegral y)
  (SInt x) / (SNum y) = SNum $ (fromIntegral x) / y
  x / y = SBinary $ Binary Div x y
  fromRational = error "API error: fromRational (in Fractional a => Fractional (Vector a)) should not be accessible by the user"

instance Fractional a => Fractional (Vector a) where
  (VNum d x) / (VNum _ y) = VNum d $ zipWith (/) x y
  x / y = VBinary $ Binary Div x y
  fromRational = error "API error: fromRational (in Fractional a => Fractional (Vector a)) should not be accessible by the user"

instance Fractional a => Fractional (Matrix a) where
  (MNum d x) / (MNum _ y) = MNum d $ zipWith (/) x y
  x / y = MBinary $ Binary Div x y
  fromRational = error "API error: fromRational (in Fractional a => Fractional (Matrix a)) should not be accessible by the user"


------------------------------------------------------------------------------
-------------------Scalar/Vector/Matrix Floating instances--------------------
------------------------------------------------------------------------------
instance (Floating a) => Floating (Scalar a) where
  pi = error "API error: pi (in Floating a => Floating (Scalar a)) should not be accessible by the user"  
  
  exp x  = SUnary (Unary Exp x)
  sqrt x = SUnary (Unary Sqrt x)
  log x  = SUnary (Unary Log x)
  
  _ ** (SInt 0) = SInt 1
  x ** (SInt 1) = x
  x ** y = SBinary (Binary Pow x y)
  logBase x y = SBinary (Binary LogBase x y)
  
  sin x = SUnary (Unary Sin x)
  cos x = SUnary (Unary Cos x)
  tan x = SUnary (Unary Tan x)
                   
  asin x = SUnary (Unary ASin x)
  acos x = SUnary (Unary ACos x)
  atan x = SUnary (Unary ATan x)

  sinh x = SUnary (Unary Sinh x)
  cosh x = SUnary (Unary Cosh x)
  tanh x = SUnary (Unary Tanh x)

  asinh x = SUnary (Unary ASinh x)
  acosh x = SUnary (Unary ACosh x)
  atanh x = SUnary (Unary ATanh x)


instance (Floating a) => Floating (Vector a) where
  pi = error "API error: pi (in Floating a => Floating (Vector a)) should not be accessible by the user"  
  
  exp x  = VUnary (Unary Exp x)
  sqrt x = VUnary (Unary Sqrt x)
  log x  = VUnary (Unary Log x)
  
  _ ** (VBroadcast d (SInt 0)) = VBroadcast d (SInt 1)
  x ** (VBroadcast _ (SInt 1)) = x
  x ** y = VBinary (Binary Pow x y)
  logBase x y = VBinary (Binary LogBase x y)
  
  sin x = VUnary (Unary Sin x)
  cos x = VUnary (Unary Cos x)
  tan x = VUnary (Unary Tan x)
                   
  asin x = VUnary (Unary ASin x)
  acos x = VUnary (Unary ACos x)
  atan x = VUnary (Unary ATan x)

  sinh x = VUnary (Unary Sinh x)
  cosh x = VUnary (Unary Cosh x)
  tanh x = VUnary (Unary Tanh x)

  asinh x = VUnary (Unary ASinh x)
  acosh x = VUnary (Unary ACosh x)
  atanh x = VUnary (Unary ATanh x)


instance (Floating a) => Floating (Matrix a) where
  pi = error "API error: pi (in Floating a => Floating (Matrix a)) should not be accessible by the user"  
  
  exp x  = MUnary (Unary Exp x)
  sqrt x = MUnary (Unary Sqrt x)
  log x  = MUnary (Unary Log x)
  
  _ ** (MBroadcast d (SInt 0)) = MBroadcast d (SInt 1)
  x ** (MBroadcast _ (SInt 1)) = x
  x ** y = MBinary (Binary Pow x y)
  logBase x y = MBinary (Binary LogBase x y)
  
  sin x = MUnary (Unary Sin x)
  cos x = MUnary (Unary Cos x)
  tan x = MUnary (Unary Tan x)
                   
  asin x = MUnary (Unary ASin x)
  acos x = MUnary (Unary ACos x)
  atan x = MUnary (Unary ATan x)

  sinh x = MUnary (Unary Sinh x)
  cosh x = MUnary (Unary Cosh x)
  tanh x = MUnary (Unary Tanh x)

  asinh x = MUnary (Unary ASinh x)
  acosh x = MUnary (Unary ACosh x)
  atanh x = MUnary (Unary ATanh x)




--------------------------------------------------------------------
--------- please eliminate the following code duplication: ---------
--------------------------------------------------------------------
-- | all vector/vector and matrix/matrix dimension checking is done here, not in Num instanecs of Vector/Matrix
-- this is because those Num instances aren't exported and are only called through safeBinaryConstructNum
safeBinaryConstructNum :: Num a => (forall b . Num b => b -> b -> b) -> Expr a -> Expr a -> Expr a
-- normal combination:
safeBinaryConstructNum f (EScalar x) (EScalar y) = EScalar $ f x y
safeBinaryConstructNum f (EVector x) (EVector y) 
  | vecDim x == vecDim y = EVector $ f x y
  | otherwise            = error $ unlines [ "Dimension mismatch in EVector + EVector"
                                           , "v1: " ++ show x
                                           , ""
                                           , "v2: " ++ show y
                                           ]
safeBinaryConstructNum f (EMatrix x) (EMatrix y)
  | matDim x == matDim y = EMatrix $ f x y
  | otherwise            = error $ unlines [ "Dimension mismatch in EMatrix + EMatrix"
                                           , "m1: " ++ show x
                                           , ""
                                           , "m2: " ++ show y
                                           ]
-- broadcast scalar to vector:
safeBinaryConstructNum f (EScalar x) (EVector y) = EVector $ f (VBroadcast (vecDim y) x) y
safeBinaryConstructNum f (EVector x) (EScalar y) = EVector $ f x (VBroadcast (vecDim x) y)
-- broadcast scalar to matrix:
safeBinaryConstructNum f (EScalar x) (EMatrix y) = EMatrix $ f (MBroadcast (matDim y) x) y
safeBinaryConstructNum f (EMatrix x) (EScalar y) = EMatrix $ f x (MBroadcast (matDim x) y)
-- illegal combination:
safeBinaryConstructNum _ (EVector _) (EMatrix _) = error "safeBinaryConstructNum: Can't combine vector with matrix"
safeBinaryConstructNum _ (EMatrix _) (EVector _) = error "safeBinaryConstructNum: Can't combine vector with matrix"


safeUnaryConstructNum :: Num a => (forall b . Num b => b -> b) -> Expr a -> Expr a
safeUnaryConstructNum f (EScalar x) = EScalar $ f x
safeUnaryConstructNum f (EVector x) = EVector $ f x
safeUnaryConstructNum f (EMatrix x) = EMatrix $ f x



-- | all vector/vector and matrix/matrix dimension checking is done here, not in Num instanecs of Vector/Matrix
-- this is because those Num instances aren't exported and are only called through safeBinaryConstructFrac
safeBinaryConstructFrac :: Fractional a => (forall b . Fractional b => b -> b -> b) -> Expr a -> Expr a -> Expr a
-- normal combination:
safeBinaryConstructFrac f (EScalar x) (EScalar y) = EScalar $ f x y
safeBinaryConstructFrac f (EVector x) (EVector y) 
  | vecDim x == vecDim y = EVector $ f x y
  | otherwise            = error $ unlines [ "Dimension mismatch in EVector + EVector"
                                           , "v1: " ++ show x
                                           , ""
                                           , "v2: " ++ show y
                                           ]
safeBinaryConstructFrac f (EMatrix x) (EMatrix y)
  | matDim x == matDim y = EMatrix $ f x y
  | otherwise            = error $ unlines [ "Dimension mismatch in EMatrix + EMatrix"
                                           , "m1: " ++ show x
                                           , ""
                                           , "m2: " ++ show y
                                           ]
-- broadcast scalar to vector:
safeBinaryConstructFrac f (EScalar x) (EVector y) = EVector $ f (VBroadcast (vecDim y) x) y
safeBinaryConstructFrac f (EVector x) (EScalar y) = EVector $ f x (VBroadcast (vecDim x) y)
-- broadcast scalar to matrix:
safeBinaryConstructFrac f (EScalar x) (EMatrix y) = EMatrix $ f (MBroadcast (matDim y) x) y
safeBinaryConstructFrac f (EMatrix x) (EScalar y) = EMatrix $ f x (MBroadcast (matDim x) y)
-- illegal combination:
safeBinaryConstructFrac _ (EVector _) (EMatrix _) = error "safeBinaryConstructFrac: Can't combine vector with matrix"
safeBinaryConstructFrac _ (EMatrix _) (EVector _) = error "safeBinaryConstructFrac: Can't combine vector with matrix"


-- | all vector/vector and matrix/matrix dimension checking is done here, not in Num instanecs of Vector/Matrix
-- this is because those Num instances aren't exported and are only called through safeBinaryConstructFloating
safeBinaryConstructFloating :: Floating a => (forall b . Floating b => b -> b -> b) -> Expr a -> Expr a -> Expr a
-- normal combination:
safeBinaryConstructFloating f (EScalar x) (EScalar y) = EScalar $ f x y
safeBinaryConstructFloating f (EVector x) (EVector y) 
  | vecDim x == vecDim y = EVector $ f x y
  | otherwise            = error $ unlines [ "Dimension mismatch in EVector + EVector"
                                           , "v1: " ++ show x
                                           , ""
                                           , "v2: " ++ show y
                                           ]
safeBinaryConstructFloating f (EMatrix x) (EMatrix y)
  | matDim x == matDim y = EMatrix $ f x y
  | otherwise            = error $ unlines [ "Dimension mismatch in EMatrix + EMatrix"
                                           , "m1: " ++ show x
                                           , ""
                                           , "m2: " ++ show y
                                           ]
-- broadcast scalar to vector:
safeBinaryConstructFloating f (EScalar x) (EVector y) = EVector $ f (VBroadcast (vecDim y) x) y
safeBinaryConstructFloating f (EVector x) (EScalar y) = EVector $ f x (VBroadcast (vecDim x) y)
-- broadcast scalar to matrix:
safeBinaryConstructFloating f (EScalar x) (EMatrix y) = EMatrix $ f (MBroadcast (matDim y) x) y
safeBinaryConstructFloating f (EMatrix x) (EScalar y) = EMatrix $ f x (MBroadcast (matDim x) y)
-- illegal combination:
safeBinaryConstructFloating _ (EVector _) (EMatrix _) = error "safeBinaryConstructFloating: Can't combine vector with matrix"
safeBinaryConstructFloating _ (EMatrix _) (EVector _) = error "safeBinaryConstructFloating: Can't combine vector with matrix"


safeUnaryConstructFloating :: Floating a => (forall b . Floating b => b -> b) -> Expr a -> Expr a
safeUnaryConstructFloating f (EScalar x) = EScalar $ f x
safeUnaryConstructFloating f (EVector x) = EVector $ f x
safeUnaryConstructFloating f (EMatrix x) = EMatrix $ f x

--------------------------------------------------------------------------
--------------- (end of horrible hacky code duplication) -----------------
--------------------------------------------------------------------------


------------------------------------------------------------------------------
---------------------------------- Expr --------------------------------------
------------------------------------------------------------------------------
data Expr a = EScalar (Scalar a)
            | EVector (Vector a)
            | EMatrix (Matrix a) deriving Eq

instance Show a => Show (Expr a) where
  show (EScalar x) = show x
  show (EVector x) = show x
  show (EMatrix x) = show x


instance Num a => Num (Expr a) where
  x + y = safeBinaryConstructNum (+) x y
  x * y = safeBinaryConstructNum (*) x y
  x - y = safeBinaryConstructNum (-) x y
  abs = safeUnaryConstructNum abs
  signum = safeUnaryConstructNum abs
  fromInteger i = EScalar (SInt (fromInteger i))


instance Fractional a => Fractional (Expr a) where
  x / y = safeBinaryConstructFrac (/) x y
  fromRational r = EScalar $ SNum (fromRational r)


instance (Floating a) => Floating (Expr a) where
  pi = EScalar $ SNum pi
  
  exp x  = safeUnaryConstructFloating exp x
  sqrt x = safeUnaryConstructFloating sqrt x
  log x  = safeUnaryConstructFloating log x
  
  x**y = safeBinaryConstructFloating (**) x y
  logBase x y = safeBinaryConstructFloating logBase x y
  
  sin x = safeUnaryConstructFloating sin x
  cos x = safeUnaryConstructFloating cos x
  tan x = safeUnaryConstructFloating tan x
                   
  asin x = safeUnaryConstructFloating asin x
  acos x = safeUnaryConstructFloating acos x
  atan x = safeUnaryConstructFloating atan x

  sinh x = safeUnaryConstructFloating sinh x
  cosh x = safeUnaryConstructFloating cosh x
  tanh x = safeUnaryConstructFloating tanh x

  asinh x = safeUnaryConstructFloating asinh x
  acosh x = safeUnaryConstructFloating acosh x
  atanh x = safeUnaryConstructFloating atanh x


----------------------------------------------------------------------
------------------------ api constructors ----------------------------
----------------------------------------------------------------------
-- | create symbolic scalar
sym :: String -> Expr a
sym name = EScalar $ SSym name

-- | create symbolic vector with length
symVec :: Int -> String -> Expr a
symVec d name 
  | d > 0     = EVector $ VSym d name
  | otherwise = error $ "symVec can't make vector with length: " ++ show d

-- | create symbolic matrix with specified (rows, columns)
symMat :: (Int,Int) -> String -> Expr a
symMat (r,c) name
  | r > 0 && c > 0 = EMatrix $ MSym (r,c) name
  | otherwise      = error $ "symMat can't make matrix with dimensions: " ++ show (r,c)

-- | create numeric vector
vec :: [a] -> Expr a
vec xs 
  | length xs > 0 = EVector $ VNum (length xs) xs
  | otherwise     = error "Improper dimensions in vec :: [a] -> Expr a"

-- | Create numeric matrix with specified (rows, cols). List is taken rowwise.
mat :: (Int,Int) -> [a] -> Expr a
mat (r,c) xs 
  | and [length xs == r*c, r > 0, c > 0] = EMatrix $ MNum (r,c) xs
  | otherwise        = error "Improper dimensions in mat :: (Int,Int) -> [a] -> Expr a"


----------------------------------------------------------------------
---------------------------- substitute ------------------------------
----------------------------------------------------------------------
-- | substitute a list of pairs [(matchThis, replaceWithThis)] in an expression
subs :: Floating a => [(Expr a, Expr a)] -> Expr a -> Expr a
subs subslist expr
  | length subslist == length (nubBy contradictingSub subslist) = callCorrectSub expr
  | otherwise = error "Error in subs: same input in substitute pairs list twice with different outputs"
  where
    contradictingSub (x0,x1) (y0,y1) = (x0 == y0) && (x1 /= y1)
    
    callCorrectSub (EScalar s) = sSubs s
    callCorrectSub (EVector v) = vSubs v
    callCorrectSub (EMatrix m) = mSubs m
    
    (scalarSubs, vectorSubs, matrixSubs) = foldr sortSubs ([],[],[]) subslist
    
    sortSubs (EScalar x, EScalar y) (ss,vs,ms) = ( ss ++ [(x,y)], vs           , ms            )
    sortSubs (EVector x, EVector y) (ss,vs,ms) = ( ss           , vs ++ [(x,y)], ms            )
    sortSubs (EMatrix x, EMatrix y) (ss,vs,ms) = ( ss           , vs           , ms ++ [(x,y)] )
    sortSubs xy _ = error $ "Can't substitute two unlike quantities, must be scalar -> scalar, vector -> vector, or matrix -> matrix)\noffending pair: " ++ show xy

    sSubs x@(SNum _) = EScalar x
    sSubs x@(SInt _) = EScalar x
    sSubs x@(SSym _) = EScalar $ sub scalarSubs x
    sSubs (SUnary (Unary unOp x)) = applyUnary unOp $ sSubs x
    sSubs (SBinary (Binary binOp x y)) = applyBinary binOp (sSubs x) (sSubs y)
    
    vSubs x@(VNum _ _) = EVector x
    vSubs x@(VSym _ _) = EVector $ sub vectorSubs x
    vSubs (VUnary (Unary unOp x)) = (applyUnary unOp) (vSubs x)
    vSubs (VBinary (Binary binOp x y)) = applyBinary binOp (vSubs x) (vSubs y)
    vSubs (VBroadcast _ x) = sSubs x
    
    mSubs x@(MNum _ _) = EMatrix x
    mSubs x@(MSym _ _) = EMatrix $ sub matrixSubs x
    mSubs (MUnary (Unary unOp x)) = applyUnary unOp $ mSubs x
    mSubs (MBinary (Binary binOp x y)) = applyBinary binOp (mSubs x) (mSubs y)
    mSubs (MBroadcast _ x) = sSubs x

    sub :: Eq a => [(a,a)] -> a -> a
    sub sublist arg 
      | isNothing newSym = arg
      | otherwise        = fromJust newSym
      where
        newSym = lookup arg sublist


----------------------------------------------------------------------
-------------------------- get children  -----------------------------
----------------------------------------------------------------------
data Children a = CSource
                | CUnary a
                | CBinary a a

-- | get all the children of an Expr
getChildren :: Expr a -> Children (Expr a)
getChildren (EScalar (SUnary (Unary _ x))) = CUnary (EScalar x)
getChildren (EScalar (SBinary (Binary _ x y))) = CBinary (EScalar x) (EScalar y)
getChildren (EScalar _) = CSource

getChildren (EVector (VUnary (Unary _ x))) = CUnary (EVector x)
getChildren (EVector (VBinary (Binary _ x y))) = CBinary (EVector x) (EVector y)
getChildren (EVector (VBroadcast _ x)) = CUnary (EScalar x)
getChildren (EVector _) = CSource

getChildren (EMatrix (MUnary (Unary _ x))) = CUnary (EMatrix x)
getChildren (EMatrix (MBinary (Binary _ x y))) = CBinary (EMatrix x) (EMatrix y)
getChildren (EMatrix (MBroadcast _ x)) = CUnary (EScalar x)
getChildren (EMatrix _) = CSource
