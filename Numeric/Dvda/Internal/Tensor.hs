-- Tensor.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Internal.Tensor( Tensor(..)
                                   , tShowNode
                                   , tGetSyms
                                   , tDim
                                   , tIsI
                                   , tToCCode
                                   , tEval
                                   ) where

import Numeric.Dvda.Internal.GNode
import Numeric.Dvda.Internal.BinaryType
import Numeric.Dvda.Internal.UnaryType
import Numeric.Dvda.Config(cType, cName)

data Tensor a = TNum [Int] [a]
              | TInt [Int] [Int]
              | TSym [Int] String
              | TUnary UnaryType (Tensor a)
              | TBinary BinaryType (Tensor a) (Tensor a)
              | TBroadcast [Int] (Tensor a) deriving Eq

instance Show a => Show (Tensor a) where
  show (TNum [] [x]) = show x
  show (TNum _ x) = show x
  show (TInt [] [x]) = show x
  show (TInt _ x) = show x
  show (TSym d x) = replicate n '{' ++ x ++ (replicate n '}')
    where
      n = length d
  show (TUnary Neg x) = "(" ++ show Neg ++ "(" ++ show x ++ "))"
  show (TUnary unaryType x) = show unaryType ++ "(" ++ show x ++ ")"
  show (TBinary binaryType x y) = "(" ++ show x ++ " " ++ show binaryType ++ " " ++ show y ++ ")"
  show (TBroadcast d x) = "BC(" ++ show d ++ " <- " ++ show x ++ ")"


-- what will be displayed in the graphviz
tShowNode :: Show a => Tensor a -> String
tShowNode x@(TNum _ _) = show x
tShowNode x@(TInt _ _) = show x
tShowNode x@(TSym _ _) = show x
tShowNode (TBroadcast d _) = "BC"++ show d
tShowNode (TUnary unOp _) = show unOp
tShowNode (TBinary binOp _ _) = show binOp

tGetSyms :: Tensor a -> [Tensor a]
tGetSyms (TNum _ _) = []
tGetSyms (TInt _ _) = []
tGetSyms x@(TSym _ _) = [x]
tGetSyms (TUnary _ x) = tGetSyms x
tGetSyms (TBinary _ x y) = tGetSyms x ++ (tGetSyms y)
tGetSyms (TBroadcast _ x) = tGetSyms x

-- | get dimensions of tensor
tDim :: Tensor a -> [Int]
tDim (TNum d _) = d
tDim (TInt d _) = d
tDim (TSym d _) = d
tDim (TBroadcast d _) = d
tDim (TUnary _ m) = tDim m
tDim (TBinary _ tx ty)
  | tDim tx == tDim ty = tDim tx
  | otherwise          = error "api error - tDim found mismatched dimensions in TBinary"


-- | test if tensor is broadcast from (SInt x)
tIsI :: Int -> Tensor a -> Bool
tIsI i (TInt [] [s]) = s == i
tIsI i (TBroadcast _ s) = tIsI i s
tIsI _ _ = False

broadcast :: [Int] -> Tensor a -> Tensor a
broadcast [] x@(TNum [] [_]) = x
broadcast [] x@(TInt [] [_]) = x
broadcast [] x@(TSym [] _) = x
broadcast [] _ = error "api error in broadcast"
broadcast dim x = TBroadcast dim x

-- Num instance
instance Num a => Num (Tensor a) where
  fromInteger = error "API error: fromInteger (in Num a => Num (Tensor a)) should not be accessible by the user"
  abs x 
    | tIsI 0 x = broadcast (tDim x) (TInt [] [0])
    | otherwise = TUnary Abs x
  signum x 
    | tIsI 0 x = broadcast (tDim x) (TInt [] [0])
    | otherwise = TUnary Signum x
  negate x 
    | tIsI 0 x = broadcast (tDim x) (TInt [] [0])
    | otherwise = TUnary Neg x
  (TNum dx xs) + (TNum _ ys) = TNum dx $ zipWith (+) xs ys
  (TInt dx xs) + (TInt _ ys) = TInt dx $ zipWith (+) xs ys
  x + y
    | tIsI 0 x = y
    | tIsI 0 y = x
    | otherwise = TBinary Add x y
  (TNum dx xs) - (TNum _ ys) = TNum dx $ zipWith (-) xs ys
  (TInt dx xs) - (TInt _ ys) = TInt dx $ zipWith (-) xs ys
  x - y
    | tIsI 0 x = negate y
    | tIsI 0 y = x
    | otherwise = TBinary Sub x y
  (TNum dx xs) * (TNum _ ys) = TNum dx $ zipWith (*) xs ys
  (TInt dx xs) * (TInt _ ys) = TInt dx $ zipWith (*) xs ys
  x * y
    | tIsI 1 x = y
    | tIsI 1 y = x
    | tIsI 0 x || tIsI 0 y = broadcast (tDim x) (TInt [] [0])
    | otherwise = TBinary Mul x y

-- Fractional instance
instance Fractional a => Fractional (Tensor a) where
  (TNum d x) / (TNum _ y) = TNum d $ zipWith (/) x y
  x / y 
    | tIsI 0 y  = error "Tensor divide by zero"
    | tIsI 0 x  = broadcast (tDim x) (TInt [] [0])
    | otherwise = TBinary Div x y
  fromRational = error "API error: fromRational (in Fractional a => Fractional (Tensor a)) should not be accessible by the user"

-- Floating instance
instance (Floating a) => Floating (Tensor a) where
  pi = error "API error: pi (in Floating a => Floating (Tensor a)) should not be accessible by the user"  
  
  exp x  = TUnary Exp x
  sqrt x = TUnary Sqrt x
  log x  = TUnary Log x
  
  x ** y
    | tIsI 0 x && tIsI 0 y = error "indeterminate expression 0**0 encountered"
    | tIsI 0 y             = broadcast (tDim x) (TInt [] [1])
    | tIsI 1 y             = x
    | otherwise = TBinary Pow x y
  logBase x y = TBinary LogBase x y
  
  sin x = TUnary Sin x
  cos x = TUnary Cos x
  tan x = TUnary Tan x
                   
  asin x = TUnary ASin x
  acos x = TUnary ACos x
  atan x = TUnary ATan x

  sinh x = TUnary Sinh x
  cosh x = TUnary Cosh x
  tanh x = TUnary Tanh x

  asinh x = TUnary ASinh x
  acosh x = TUnary ACosh x
  atanh x = TUnary ATanh x


-- | evaluate (Tensor a) to a numeric list
tEval :: Floating a => Tensor a -> [a]
tEval (TNum _ x) = x
tEval (TInt _ x) = map fromIntegral x
tEval (TBroadcast d x) = replicate (product d) (head $ tEval x)
tEval (TSym _ _) = error "api error: tEval can't evaluate symbolic expression"
tEval (TUnary unType x) = map (applyUnary unType) (tEval x)
tEval (TBinary binType x y) = zipWith (applyBinary binType) (tEval x) (tEval y)


-- | convert GNode (Tensor a) into proper c code
tToCCode :: Show a => GNode (Tensor a) -> String
tToCCode x = case length (tDim (exprOfGNode x)) of
  0 -> sToCCode x
  _ -> arrayToCCode x


-- scalar code
sAssign :: Int -> String
sAssign idx = cType ++ " " ++ cName idx ++ " = "
-- input
sToCCode :: Show a => GNode (Tensor a) -> String
sToCCode (GOutput idx _ cx k name) = "out["++ show k ++ "][0] = "++cName cx ++ "; // "++name++", output node: " ++ show idx
sToCCode (GSource idx (TNum [] [x])) = "const " ++ sAssign idx ++ show x ++ ";"
sToCCode (GSource idx (TInt [] [x])) = "const " ++ sAssign idx ++ show x ++ ";"
sToCCode (GSource idx (TSym [] n)) = "const " ++ sAssign idx ++ n ++ ";"
sToCCode (GUnary idx (TUnary unType _) ic) = "const " ++ sAssign idx ++ cshow unType ++ "(" ++ cName ic ++ ");"
sToCCode (GBinary idx (TBinary binType _ _) (icx, icy)) = "const " ++ sAssign idx ++ 
                                                          cName icx ++ 
                                                          " " ++ show binType ++ " " ++
                                                          cName icy ++";"
sToCCode x@(GSource _ _) = error $ "sToCCode api fail in GSource _ _)" ++ show x
sToCCode x@(GUnary _ _ _) = error $ "sToCCode api fail in GUnary _ _)" ++ show x
sToCCode x@(GBinary _ _ _) = error $ "sToCCode api fail in GBinary _ _)" ++ show x
sToCCode x@(GBroadcast _ _ _) = error $ "sToCCode api fail in GBroadcast _ _)" ++ show x


-- vector code
vAssign :: [Int] -> Int -> String
vAssign d idx = cType ++ " " ++ cName idx ++ "[" ++ show (product d) ++ "] = "

l2a :: Show a => [a] -> String
l2a xs = "{" ++ drop 1 (init (show xs)) ++ "}"

cMap :: [Int] -> String -> Int -> Int -> String
cMap d f self child = cType ++ " " ++ cName self ++ "[" ++ show (product d) ++ "]; "++
                      "    for (int k=0; k<"++show (product d)++"; k++){ "++
                      cName self ++ "[k] = " ++ f ++ "( " ++ cName child ++ "[k] ); }"

cBroadcast :: [Int] -> Int -> Int -> String
cBroadcast d self child = cType ++ " " ++ cName self ++ "[" ++ show (product d) ++ "]; "++
                          "    for (int k=0; k<"++show (product d)++"; k++){ "++
                          cName self ++ "[k] = " ++ cName child ++ "; }"

cZip :: [Int] -> String -> Int -> Int -> Int -> String
cZip d f self cx cy = cType ++ " " ++ cName self ++ "[" ++ show (product d) ++ "]; "++
                      "    for (int k=0; k<"++show (product d)++"; k++){ "++
                      cName self ++ "[k] = " ++ cName cx ++ "[k] " ++ f ++ " "++cName cy++"[k]; }"


arrayToCCode :: Show a => GNode (Tensor a) -> String
arrayToCCode (GOutput idx x cx k name) = "memcpy( out["++ show k ++ "], "++cName cx ++ ", "++show (product (tDim x))++"*sizeof(double) ); // "++name++", output node: " ++ show idx
arrayToCCode (GSource idx (TNum d xs)) = "const " ++ vAssign d idx ++ l2a xs ++ ";"
arrayToCCode (GSource idx (TInt d xs)) = "const " ++ vAssign d idx ++ l2a xs ++ ";"
arrayToCCode (GSource idx (TSym _ n)) = "const " ++ cType ++ " * const " ++ cName idx ++ " = " ++ n ++ ";"
arrayToCCode (GBroadcast idx (TBroadcast d _) ic) = cBroadcast d idx ic
arrayToCCode (GUnary idx x@(TUnary unType _) ic) = cMap (tDim x) (show unType) idx ic
arrayToCCode (GBinary idx x@(TBinary binType _ _) (icx, icy)) = cZip (tDim x) (show binType) idx icx icy

arrayToCCode x@(GSource _ _) = error $ "arrayToCCode api fail in GSource _ _)" ++ show x
arrayToCCode x@(GUnary _ _ _) = error $ "arrayToCCode api fail in GUnary _ _)" ++ show x
arrayToCCode x@(GBinary _ _ _) = error $ "arrayToCCode api fail in GBinary _ _)" ++ show x
arrayToCCode x@(GBroadcast _ _ _) = error $ "arrayToCCode api fail in GBroadcast _ _)" ++ show x
