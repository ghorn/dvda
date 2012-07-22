{-# OPTIONS_GHC -Wall #-}

module MutableDvda.FullShow ( FullShow(..)
                            ) where

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
