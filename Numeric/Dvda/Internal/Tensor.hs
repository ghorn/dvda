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
import Numeric.Dvda.Internal.Binary
import Numeric.Dvda.Internal.Unary
import Numeric.Dvda.Config(cType, cName)

data Tensor a = TNum [Int] [a]
              | TInt [Int] [Int]
              | TSym [Int] String
              | TUnary (Unary (Tensor a))
              | TBinary (Binary (Tensor a))
              | TBroadcast [Int] (Tensor a) deriving Eq

instance Show a => Show (Tensor a) where
  show (TNum [] [x]) = show x
  show (TNum _ x) = show x
  show (TInt [] [x]) = show x
  show (TInt _ x) = show x
  show (TSym d x) = replicate n '{' ++ x ++ (replicate n '}')
    where
      n = length d
  show (TUnary x) = show x
  show (TBinary x) = show x
  show (TBroadcast d x) = "BC(" ++ show d ++ " <- " ++ show x ++ ")"


-- what will be displayed in the graphviz
tShowNode :: Show a => Tensor a -> String
tShowNode x@(TNum _ _) = show x
tShowNode x@(TInt _ _) = show x
tShowNode x@(TSym _ _) = show x
tShowNode (TBroadcast d _) = "BC"++ show d
tShowNode (TUnary (Unary unOp _)) = show unOp
tShowNode (TBinary (Binary binOp _ _)) = show binOp

tGetSyms :: Tensor a -> [Tensor a]
tGetSyms (TNum _ _) = []
tGetSyms (TInt _ _) = []
tGetSyms x@(TSym _ _) = [x]
tGetSyms (TUnary (Unary _ x)) = tGetSyms x
tGetSyms (TBinary (Binary _ x y)) = tGetSyms x ++ (tGetSyms y)
tGetSyms (TBroadcast _ x) = tGetSyms x

-- | get dimensions of tensor
tDim :: Tensor a -> [Int]
tDim (TNum d _) = d
tDim (TInt d _) = d
tDim (TSym d _) = d
tDim (TBroadcast d _) = d
tDim (TUnary (Unary _ m)) = tDim m
tDim (TBinary (Binary _ tx ty)) 
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
    | otherwise = TUnary (Unary Abs x)
  signum x 
    | tIsI 0 x = broadcast (tDim x) (TInt [] [0])
    | otherwise = TUnary (Unary Signum x)
  negate x 
    | tIsI 0 x = broadcast (tDim x) (TInt [] [0])
    | otherwise = TUnary (Unary Neg x)
  (TNum dx xs) + (TNum _ ys) = TNum dx $ zipWith (+) xs ys
  (TInt dx xs) + (TInt _ ys) = TInt dx $ zipWith (+) xs ys
  x + y
    | tIsI 0 x = y
    | tIsI 0 y = x
    | otherwise = TBinary (Binary Add x y)
  (TNum dx xs) - (TNum _ ys) = TNum dx $ zipWith (-) xs ys
  (TInt dx xs) - (TInt _ ys) = TInt dx $ zipWith (-) xs ys
  x - y
    | tIsI 0 x = negate y
    | tIsI 0 y = x
    | otherwise = TBinary (Binary Sub x y)
  (TNum dx xs) * (TNum _ ys) = TNum dx $ zipWith (*) xs ys
  (TInt dx xs) * (TInt _ ys) = TInt dx $ zipWith (*) xs ys
  x * y
    | tIsI 1 x = y
    | tIsI 1 y = x
    | tIsI 0 x || tIsI 0 y = broadcast (tDim x) (TInt [] [0])
    | otherwise = TBinary (Binary Mul x y)

-- Fractional instance
instance Fractional a => Fractional (Tensor a) where
  (TNum d x) / (TNum _ y) = TNum d $ zipWith (/) x y
  x / y 
    | tIsI 0 y  = error "Tensor divide by zero"
    | tIsI 0 x  = broadcast (tDim x) (TInt [] [0])
    | otherwise = TBinary $ Binary Div x y
  fromRational = error "API error: fromRational (in Fractional a => Fractional (Tensor a)) should not be accessible by the user"

-- Floating instance
instance (Floating a) => Floating (Tensor a) where
  pi = error "API error: pi (in Floating a => Floating (Tensor a)) should not be accessible by the user"  
  
  exp x  = TUnary (Unary Exp x)
  sqrt x = TUnary (Unary Sqrt x)
  log x  = TUnary (Unary Log x)
  
  x ** y
    | tIsI 0 x && tIsI 0 y = error "indeterminate expression 0**0 encountered"
    | tIsI 0 y             = broadcast (tDim x) (TInt [] [1])
    | tIsI 1 y             = x
    | otherwise = TBinary (Binary Pow x y)
  logBase x y = TBinary (Binary LogBase x y)
  
  sin x = TUnary (Unary Sin x)
  cos x = TUnary (Unary Cos x)
  tan x = TUnary (Unary Tan x)
                   
  asin x = TUnary (Unary ASin x)
  acos x = TUnary (Unary ACos x)
  atan x = TUnary (Unary ATan x)

  sinh x = TUnary (Unary Sinh x)
  cosh x = TUnary (Unary Cosh x)
  tanh x = TUnary (Unary Tanh x)

  asinh x = TUnary (Unary ASinh x)
  acosh x = TUnary (Unary ACosh x)
  atanh x = TUnary (Unary ATanh x)


-- | evaluate (Tensor a) to a numeric list
tEval :: Floating a => Tensor a -> [a]
tEval (TNum _ x) = x
tEval (TInt _ x) = map fromIntegral x
tEval (TBroadcast d x) = replicate (product d) (head $ tEval x)
tEval (TSym _ _) = error "api error: tEval can't evaluate symbolic expression"
tEval (TUnary (Unary unType x)) = map (applyUnary unType) (tEval x)
tEval (TBinary (Binary binType x y)) = zipWith (applyBinary binType) (tEval x) (tEval y)


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
sToCCode (GUnary idx (TUnary (Unary unType _)) ic) = "const " ++ sAssign idx ++ show unType ++ "(" ++ cName ic ++ ");"
sToCCode (GBinary idx (TBinary (Binary binType _ _)) (icx, icy)) = "const " ++ sAssign idx ++ 
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
arrayToCCode (GUnary idx x@(TUnary (Unary unType _)) ic) = cMap (tDim x) (show unType) idx ic
arrayToCCode (GBinary idx x@(TBinary (Binary binType _ _)) (icx, icy)) = cZip (tDim x) (show binType) idx icx icy

arrayToCCode x@(GSource _ _) = error $ "arrayToCCode api fail in GSource _ _)" ++ show x
arrayToCCode x@(GUnary _ _ _) = error $ "arrayToCCode api fail in GUnary _ _)" ++ show x
arrayToCCode x@(GBinary _ _ _) = error $ "arrayToCCode api fail in GBinary _ _)" ++ show x
arrayToCCode x@(GBroadcast _ _ _) = error $ "arrayToCCode api fail in GBroadcast _ _)" ++ show x
