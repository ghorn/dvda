{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}


module MutableDvda.Utils ( FullShow(..)
                         , backprop
                         , rad
                         ) where

import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HM
import Data.Hashable ( Hashable )

import Dvda.Dual hiding ( fad, fad' )

import MutableDvda.Expr

class FullShow a where
  fullShow :: a -> IO String

fs :: FullShow a => a -> a -> String -> IO String
fs x' y' name = do
  x <- fullShow x'
  y <- fullShow y'
  return $ "(" ++ name ++ " " ++ x ++ " " ++ y ++ ")"

fs' :: FullShow a => a -> String -> IO String
fs' x' name = do
  x <- fullShow x'
  return $ "(" ++ name ++ " " ++ x ++ ")"

instance FullShow a => FullShow (Nums a) where
  fullShow (Mul x y) = fs x y "Mul"
  fullShow (Add x y) = fs x y "Add"
  fullShow (Sub x y) = fs x y "Sub"
  fullShow (Negate x) = fmap ("Negate " ++) (fullShow x)
  fullShow (Abs x) = fmap ("Abs " ++) (fullShow x)
  fullShow (Signum x) = fmap ("Signum " ++) (fullShow x)
  fullShow (FromInteger x) = return (show x)

instance FullShow a => FullShow (Fractionals a) where
  fullShow (Div x y) = fs x y "Div"
  fullShow (FromRational x) = return (show x)

instance FullShow a => FullShow (Floatings a) where
  fullShow  (Pow x y) = fs x y "Pow"
  fullShow  (LogBase x y) = fs x y "LogBase"
  fullShow  (Exp x)   = fs' x "Exp"
  fullShow  (Log x)   = fs' x "Log"
  fullShow  (Sin x)   = fs' x "Sin"
  fullShow  (Cos x)   = fs' x "Cos"
  fullShow  (ASin x)  = fs' x "ASin"
  fullShow  (ATan x)  = fs' x "ATan"
  fullShow  (ACos x)  = fs' x "ACos"
  fullShow  (Sinh x)  = fs' x "Sinh"
  fullShow  (Cosh x)  = fs' x "Cosh"
  fullShow  (Tanh x)  = fs' x "Tanh"
  fullShow  (ASinh x) = fs' x "ASinh"
  fullShow  (ATanh x) = fs' x "ATanh"
  fullShow  (ACosh x) = fs' x "ACosh"

instance Show a => FullShow (Expr a) where
  fullShow (ESym name) = return name
  fullShow (EConst a) = return (show a)
  fullShow (ENum x) = fullShow x
  fullShow (EFractional x) = fullShow x
  fullShow (EFloating x) = fullShow x
  fullShow (EGraphRef e k) = do
    se <-  fullShow e
    return $ "EGraphRef(" ++ show k ++ "): (" ++ se ++ ")"
  fullShow e'@(ERef _) = do
    e <- readExpr e'
    se <-  fullShow e
    return $ "ERef (" ++ se ++ ")"

-- fad :: Num a => (Dual a -> [Dual a]) -> a -> [a]
-- fad f x = map dualPerturbation $ f (Dual x 1)

bpBinary :: (Eq a, Num a)
            => Expr a -> Expr a -> Expr a
            -> (Dual (Expr a) -> Dual (Expr a) -> Dual (Expr a))
            -> IO [(Expr a, Expr a)]
bpBinary sens g h binop = do
   let dfdg = dualPerturbation $ binop (Dual g 1) (Dual h 0)
       dfdh = dualPerturbation $ binop (Dual g 0) (Dual h 1)
   gsens <- backprop (sens*dfdg) g
   hsens <- backprop (sens*dfdh) h
   return $ gsens ++ hsens

bpUnary :: (Eq a, Num a)
           => Expr a -> Expr a
           -> (Dual (Expr a) -> Dual (Expr a))
           -> IO [(Expr a, Expr a)]
bpUnary sens g unop = do
   let dfdg = dualPerturbation $ unop (Dual g 1)
   backprop (sens*dfdg) g

backprop :: Eq a => Expr a -> Expr a -> IO [(Expr a, Expr a)]
backprop sens e@(ERef _) = readExpr e >>= backprop sens
backprop sens e@(ESym _) = return [(e,sens)]
backprop _ (EConst _) = return []
backprop _ (ENum (FromInteger _)) = return []
backprop _ (EFractional (FromRational _)) = return []
backprop sens (ENum (Mul x y)) = bpBinary sens x y (*)
backprop sens (ENum (Add x y)) = bpBinary sens x y (+)
backprop sens (ENum (Sub x y)) = bpBinary sens x y (-)
backprop sens (ENum (Abs x))    = bpUnary sens x abs
backprop sens (ENum (Negate x)) = bpUnary sens x negate
backprop sens (ENum (Signum x)) = bpUnary sens x signum
backprop sens (EFractional (Div x y)) = bpBinary sens x y (/)
backprop sens (EFloating (Pow x y)) = bpBinary sens x y (**)
backprop sens (EFloating (LogBase x y)) = bpBinary sens x y logBase
backprop sens (EFloating (Exp x))   = bpUnary sens x exp
backprop sens (EFloating (Log x))   = bpUnary sens x log
backprop sens (EFloating (Sin x))   = bpUnary sens x sin
backprop sens (EFloating (Cos x))   = bpUnary sens x cos
backprop sens (EFloating (ASin x))  = bpUnary sens x asin
backprop sens (EFloating (ATan x))  = bpUnary sens x atan
backprop sens (EFloating (ACos x))  = bpUnary sens x acos
backprop sens (EFloating (Sinh x))  = bpUnary sens x sinh
backprop sens (EFloating (Cosh x))  = bpUnary sens x cosh
backprop sens (EFloating (Tanh x))  = bpUnary sens x tanh
backprop sens (EFloating (ASinh x)) = bpUnary sens x asinh
backprop sens (EFloating (ATanh x)) = bpUnary sens x atanh
backprop sens (EFloating (ACosh x)) = bpUnary sens x acosh
backprop sens (EGraphRef x _)       = backprop sens x

rad :: (Num a, Eq a, Hashable a) => Expr a -> IO (HashMap (Expr a) (Expr a))
rad x = do
  sensitivities <- backprop 1 x
  return $ HM.fromListWith (+) sensitivities
