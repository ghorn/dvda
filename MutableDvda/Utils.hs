{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}


module MutableDvda.Utils ( FullShow(..)
                         , countNodes
                         , backprop
                         , rad
                         ) where

import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HM
import Data.Hashable ( Hashable )

import Dvda.Dual hiding ( fad, fad' )

import MutableDvda.Expr
import qualified MutableDvda.SharedVar as SV

class FullShow a where
  fullShow :: a -> SV.SVMonad String

fs :: FullShow a => a -> a -> String -> SV.SVMonad String
fs x' y' name = do
  x <- fullShow x'
  y <- fullShow y'
  return $ "(" ++ name ++ " " ++ x ++ " " ++ y ++ ")"

fs' :: FullShow a => a -> String -> SV.SVMonad String
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
  fullShow e@(ERef _) = readExpr e >>= fullShow

-- fad :: Num a => (Dual a -> [Dual a]) -> a -> [a]
-- fad f x = map dualPerturbation $ f (Dual x 1)

bpBinary :: (Eq a, Num a)
            => Expr a -> Expr a -> Expr a
            -> (Dual (Expr a) -> Dual (Expr a) -> Dual (Expr a))
            -> SV.SVMonad [(Expr a, Expr a)]
bpBinary sens g h binop = do
   let dfdg = dualPerturbation $ binop (Dual g 1) (Dual h 0)
       dfdh = dualPerturbation $ binop (Dual g 0) (Dual h 1)
   gsens <- backprop (sens*dfdg) g
   hsens <- backprop (sens*dfdh) h
   return $ gsens ++ hsens

bpUnary :: (Eq a, Num a)
           => Expr a -> Expr a
           -> (Dual (Expr a) -> Dual (Expr a))
           -> SV.SVMonad [(Expr a, Expr a)]
bpUnary sens g unop = do
   let dfdg = dualPerturbation $ unop (Dual g 1)
   backprop (sens*dfdg) g

backprop :: Eq a => Expr a -> Expr a -> SV.SVMonad [(Expr a, Expr a)]
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

rad :: (Num a, Eq a, Hashable (Expr a)) => Expr a -> SV.SVMonad (HashMap (Expr a) (Expr a))
rad x = do
  sensitivities <- backprop 1 x
  return $ HM.fromListWith (+) sensitivities


countNodes :: Expr a -> SV.SVMonad Int
countNodes e@(ERef _) = readExpr e >>= countNodes
countNodes (ESym _) = return 1
countNodes (EConst _) = return 1
countNodes (ENum (FromInteger _)) = return 1
countNodes (EFractional (FromRational _)) = return 1
countNodes (ENum (Mul x y)) = do
  nx <- countNodes x
  ny <- countNodes y
  return $ nx + ny + 1
countNodes (ENum (Add x y)) = do
  nx <- countNodes x
  ny <- countNodes y
  return $ nx + ny + 1
countNodes (ENum (Sub x y)) = do
  nx <- countNodes x
  ny <- countNodes y
  return $ nx + ny + 1
countNodes (ENum (Abs x)) = do
  n <- countNodes x
  return $ 1 + n
countNodes (ENum (Negate x)) = do
  n <- countNodes x
  return $ 1 + n
countNodes (ENum (Signum x)) = do
  n <- countNodes x
  return $ 1 + n
countNodes (EFractional (Div x y)) = do
  nx <- countNodes x
  ny <- countNodes y
  return $ nx + ny + 1
countNodes (EFloating (Pow x y)) = do
  nx <- countNodes x
  ny <- countNodes y
  return $ nx + ny + 1
countNodes (EFloating (LogBase x y)) = do
  nx <- countNodes x
  ny <- countNodes y
  return $ nx + ny + 1
countNodes (EFloating (Exp x))   = fmap (1 +) (countNodes x)
countNodes (EFloating (Log x))   = fmap (1 +) (countNodes x)
countNodes (EFloating (Sin x))   = fmap (1 +) (countNodes x)
countNodes (EFloating (Cos x))   = fmap (1 +) (countNodes x)
countNodes (EFloating (ASin x))  = fmap (1 +) (countNodes x)
countNodes (EFloating (ATan x))  = fmap (1 +) (countNodes x)
countNodes (EFloating (ACos x))  = fmap (1 +) (countNodes x)
countNodes (EFloating (Sinh x))  = fmap (1 +) (countNodes x)
countNodes (EFloating (Cosh x))  = fmap (1 +) (countNodes x)
countNodes (EFloating (Tanh x))  = fmap (1 +) (countNodes x)
countNodes (EFloating (ASinh x)) = fmap (1 +) (countNodes x)
countNodes (EFloating (ATanh x)) = fmap (1 +) (countNodes x)
countNodes (EFloating (ACosh x)) = fmap (1 +) (countNodes x)
