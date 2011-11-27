-- Expr.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Expr.Expr( Expr(..)
                             , Dim(..)
--                             , dot
--                             , applyDot
                             , isNumeric
                             , symbolic
                             , symVec
                             , symMat
                             , vec
                             , mat
                             , zipBinaryF
                             ) where

--import Data.GraphViz(Labellable(..))
--import Data.Text.Lazy(pack)
--import qualified Data.Array.Repa as R

import Numeric.Dvda.Expr.BinaryType
import Numeric.Dvda.Expr.UnaryType
import Numeric.Dvda.Expr.Scalar

type VecDim = Int
type MatDim = (Int,Int)

data Dim = Sca
         | Vec VecDim
         | Mat MatDim deriving (Show, Eq)

data Expr a = Sym Dim String
            | EScalar (Scalar a)
            | Vector VecDim [a]
            | Matrix MatDim [a]
            | Unary UnaryType (Expr a)
            | Binary BinaryType (Expr a) (Expr a)
            | Broadcast Dim (Expr a) deriving Eq

--            | forall sh1 sh2 . (R.Shape sh1, R.Shape sh2, Dottable sh1 sh2 sh) =>
--              Dot { arg1' :: Expr sh1 a
--                  , arg2' :: Expr sh2 a
--                  , dim :: sh
--                  }

isNumeric :: Expr a -> Bool
isNumeric (EScalar (N _)) = True
isNumeric (Vector _ _) = True
isNumeric (Matrix _ _) = True
isNumeric (Broadcast _ x) = isNumeric x
isNumeric _ = False

isI  :: Int -> Expr a -> Bool
isI x (EScalar (I y)) = x == y
isI x (Broadcast _ y) = isI x y
isI _ _ = False

mapUnary :: Floating a => UnaryType -> Expr a -> Expr a
mapUnary ut (EScalar (N a)) = EScalar $ N (applyUnary ut a)
mapUnary ut (Vector d xs) = Vector d $ map (applyUnary ut) xs
mapUnary ut (Matrix d xs) = Matrix d $ map (applyUnary ut) xs
mapUnary ut (Broadcast d x) = Broadcast d $ (applyUnary ut x)
mapUnary ut expr = Unary ut expr

zipBinaryF :: Floating a => BinaryType -> Expr a -> Expr a -> Expr a 
zipBinaryF binType x' y'
  | getDim x' == getDim y' = zipBinaryF' x'' y''
  | otherwise              = error $ unlines [ "type mismatch in zipBinaryF (" ++ show binType ++ ")"
                                             , "x: " ++ show x'
                                             , "y: " ++ show y'
                                             ]
  where
    (Binary _ x'' y'') = zipBinary binType x' y' -- call this to prune Add (I 0)/Mul (I 1):
    
    zipBinaryF' (EScalar (N x)) (EScalar (N y)) = EScalar (N $ (applyBinary binType) x y)
    zipBinaryF' (Vector d x) (Vector _ y) = Vector d $ zipWith (applyBinary binType) x y
    zipBinaryF' (Matrix d x) (Matrix _ y) = Matrix d $ zipWith (applyBinary binType) x y
    zipBinaryF' x y = Binary binType x y


zipBinary :: Num a => BinaryType -> Expr a -> Expr a -> Expr a 
zipBinary binType' x' y'
  | getDim x' == getDim y' = zipBinary' binType' x' y'
  | otherwise              = error $ unlines [ "type mismatch in zipBinary (" ++ show binType' ++ ")"
                                             , "x: " ++ show x'
                                             , "y: " ++ show y'
                                             ]
  where
    zipBinary' Add x y
      | isI 0 x  = y
      | isI 0 y  = x
      | otherwise = Binary Add x y
    zipBinary' Sub x y
      | isI 0 x  = negate y
      | isI 0 y  = x
      | otherwise = Binary Add x y
    zipBinary' Mul x y
      | isI 1 x  = y
      | isI 1 y  = x
      | otherwise = Binary Mul x y
    zipBinary' binType x y = Binary binType x y

getDim :: Expr a -> Dim
getDim (Sym d _) = d
getDim (EScalar _) = Sca
getDim (Vector d _) = Vec d
getDim (Matrix d _) = Mat d
getDim (Unary _ arg) = getDim arg
getDim (Binary _ arg1 _) = getDim arg1
getDim (Broadcast d _) = d
--getDim (Tensor a) = R.extent a

--getDim (Dot { dim = d }) = d

vec :: Num a => [a] -> Expr a
vec xs = Vector (length xs) xs

mat :: Num a => (Int,Int) -> [a] -> Expr a
mat (r,c) xs 
  | length xs == r*c = Matrix (r,c) xs
  | otherwise        = error $ unlines [ "Improper dimensions in mat :: (Int,Int) -> [a] -> Expr a"
                                       , "dim: " ++ show (r,c)
                                       , "[a]: " ++ show xs
                                       ]


instance Show a => Show (Expr a) where
  show (Sym _ name) = name
  show (EScalar x) = show x
  show (Vector _ v) = show v
  show (Matrix _ m) = show m
  show (Unary unaryType arg) = show unaryType ++ "(" ++ show arg ++ ")"
  show (Binary binaryType arg1 arg2) = "( " ++ show arg1 ++" "++ show binaryType ++" "++ show arg2 ++ " )"
  show (Broadcast d x) = "broadcast(" ++ show x ++ ", " ++ show d ++ ")"
--  show (Tensor t) = show t
--  show (Dot {arg1' = a1, arg2' = a2}) = "dot( " ++ show a1 ++ ", " ++ show a2 ++ " )"


broadcast :: Scalar a -> Dim -> Expr a
broadcast x d = Broadcast d (EScalar x)

instance Num a => Num (Expr a) where
  -- (+)
  (EScalar x) + (EScalar y) = EScalar (x + y)
  (EScalar x) + y = (broadcast x (getDim y)) + y
  x + (EScalar y) = x + (broadcast y (getDim x))
  x + y = zipBinary Add x y
  
  -- (*)
  (EScalar x) * (EScalar y) = EScalar (x * y)
  (EScalar x) * y = (broadcast x (getDim y)) * y
  x * (EScalar y) = x * (broadcast y (getDim x))
  x * y = zipBinary Mul x y

  -- (-)
  (EScalar x) - (EScalar y) = EScalar (x - y)
  (EScalar x) - y = (broadcast x (getDim y)) - y
  x - (EScalar y) = x - (broadcast y (getDim x))
  x - y = zipBinary Sub x y

  negate x = Unary Neg x
  abs x = Unary Abs x
  signum x = Unary Signum x
  fromInteger x = EScalar $ I (fromInteger x)



instance Fractional a => Fractional (Expr a) where
  (EScalar x) / (EScalar y) = EScalar (x / y)
  (EScalar x) / y = (broadcast x (getDim y)) / y
  x / (EScalar y) = x / (broadcast y (getDim x))
  x / y = zipBinary Div x y

  fromRational x = EScalar $ fromRational x
--  fromRational x = num / den
--    where
--      num = fromIntegral $ numerator x
--      den = fromIntegral $ denominator x


instance (Floating a) => Floating (Expr a) where
  pi = EScalar $ N pi
  
  exp x  = mapUnary Exp x
  sqrt x = mapUnary Sqrt x
  log x  = mapUnary Log x
  
  x**y = zipBinary Pow x y
  logBase x y = zipBinary Pow x y
  
  sin x = mapUnary Sin x
  cos x = mapUnary Cos x
  tan x = mapUnary Tan x
                   
  asin x = mapUnary ASin x
  acos x = mapUnary ACos x
  atan x = mapUnary ATan x

  sinh x = mapUnary Sinh x
  cosh x = mapUnary Cosh x
  tanh x = mapUnary Tanh x

  asinh x = mapUnary ASinh x
  acosh x = mapUnary ACosh x
  atanh x = mapUnary ATanh x


--instance (Show a, Num a, R.Elt a, Show sh, R.Shape sh) => Labellable (Expr sh a) where
--  toLabelValue go = toLabelValue $ pack $ show go ++ "["++show (getDim go)++"]"

symbolic :: String -> Expr a
symbolic name = Sym Sca name

symVec :: String -> Int -> Expr a
symVec name d = Sym (Vec d) name

symMat :: String -> (Int,Int) -> Expr a
symMat name d = Sym (Mat d) name
