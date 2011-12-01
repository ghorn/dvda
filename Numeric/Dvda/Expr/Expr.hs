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
                             , Children(..)
                             , getChildren
                             , showNode
                             , showType
                             , showDim
                             , symName
                             ) where

import Numeric.Dvda.Expr.Binary
import Numeric.Dvda.Expr.Unary
import Numeric.Dvda.Expr.Scalar
import Numeric.Dvda.Expr.Vector
import Numeric.Dvda.Expr.Matrix


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


-- | get name if variable is symbolic
symName :: Expr a -> Maybe String
symName (EScalar (SSym n)) = Just n
symName (EVector (VSym _ n)) = Just n
symName (EMatrix (MSym _ n)) = Just n
symName _ = Nothing


-- | get all the symbolic variables in an expression
getSyms :: Expr a -> [Expr a]
getSyms (EScalar x) = map EScalar $ sGetSyms x
getSyms (EVector x) = (map EVector vs) ++ (map EScalar ss)
  where
    (vs, ss) = vGetSyms x
getSyms (EMatrix x) = (map EMatrix ms) ++ (map EScalar ss)
  where
    (ms, ss) = mGetSyms x

-- | data type containing children of an expression
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
