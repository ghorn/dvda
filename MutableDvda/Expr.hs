{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
-- {-# Language StandaloneDeriving #-}

module MutableDvda.Expr ( Expr(..)
                        , isVal
                        , sym
                        , const'
                        ) where

import Control.Concurrent.MVar
import System.IO.Unsafe ( unsafePerformIO )

data Expr a where
  ERef :: IO (MVar (Expr a)) -> Expr a
  ESym :: String -> Expr a
  EConst :: a -> Expr a
  ENum :: Num a => Nums (Expr a) -> Expr a
  EFractional :: Fractional a => Fractionals (Expr a) -> Expr a
  EFloating :: Floating a => Floatings (Expr a) -> Expr a

instance Eq a => Eq (Expr a) where
  (==) x@(ERef mx_) y@(ERef my_) = unsafePerformIO $ do
    mx <- mx_
    my <- my_
    return $ mx == my || unsafePerformIO (readExpr x) == unsafePerformIO (readExpr y)
  (==) x@(ERef _) y = (==) (unsafePerformIO (readExpr x)) y
  (==) x y@(ERef _) = (==) x (unsafePerformIO (readExpr y))
  (==) (ESym x) (ESym y) = x == y
  (==) (EConst x) (EConst y) = x == y
  (==) (ENum x) (ENum y) = x == y
  (==) (EFractional x) (EFractional y) = x == y
  (==) (EFloating x) (EFloating y) = x == y
  (==) _ _ = False

data Nums a = Mul a a
            | Add a a
            | Sub a a
            | Negate a
            | Abs a
            | Signum a
            | FromInteger Integer deriving Show

commutativeMul :: Bool
commutativeMul = True

commutativeAdd :: Bool
commutativeAdd = True

instance Eq a => Eq (Nums a) where
  (Mul x0 y0) == (Mul x1 y1) = if commutativeMul
                               then (x0 == x1 && y0 == y1) || (x0 == y1 && x1 == y0)
                               else x0 == x1 && y0 == y1
  (Add x0 y0) == (Add x1 y1) = if commutativeAdd
                               then (x0 == x1 && y0 == y1) || (x0 == y1 && x1 == y0)
                               else x0 == x1 && y0 == y1
  (Sub x0 y0) == (Sub x1 y1) = x0 == x1 && y0 == y1
  (Negate x) == (Negate y) = x == y
  (Abs x) == (Abs y) = x == y
  (Signum x) == (Signum y) = x == y
  (FromInteger x) == (FromInteger y) = x == y
  _ == _ = False
  
data Fractionals a = Div a a
                   | FromRational Rational deriving (Eq, Show)

data Floatings a = Pow a a
                 | LogBase a a
                 | Exp a
                 | Log a
                 | Sin a
                 | Cos a
                 | ASin a
                 | ATan a
                 | ACos a
                 | Sinh a
                 | Cosh a
                 | Tanh a
                 | ASinh a
                 | ATanh a
                 | ACosh a deriving (Eq, Show)

--deriving instance Enum a => Enum (Nums a)
--deriving instance Bounded a => Bounded (Nums a)

--deriving instance Enum a => Enum (Fractionals a)
--deriving instance Bounded a => Bounded (Fractionals a)

--deriving instance Enum a => Enum (Floatings a)
--deriving instance Bounded a => Bounded (Floatings a)

--newExpr :: IO (Expr a) -> Expr a
--newExpr x_ = do
--  x <- x_
--  case 
--newExpr x@(ERef _) = x
--newExpr x = 

eref :: Expr a -> Expr a
eref x@(ERef _) = x
eref x = ERef (newMVar x)

instance (Num a, Eq a) => Num (Expr a) where
  (*) (EConst x) (EConst y) = EConst (x*y)
  (*) (ENum (FromInteger kx)) (ENum (FromInteger ky)) = ENum $ FromInteger (kx * ky)
  (*) (EConst x) (ENum (FromInteger ky)) = EConst $ x * fromInteger ky
  (*) (ENum (FromInteger kx)) (EConst y) = EConst $ fromInteger kx * y
  (*) x y = ERef $ do
    xIsZero <- isVal 0 x
    yIsZero <- isVal 0 y
    xIsOne <- isVal 1 x
    yIsOne <- isVal 1 y
    let ret
          | xIsZero || yIsZero = 0
          | xIsOne = y
          | yIsOne = x
          | otherwise = ENum $ Mul x y
    newMVar ret

  (+) (EConst x) (EConst y) = EConst (x+y)
  (+) (ENum (FromInteger kx)) (ENum (FromInteger ky)) = ENum $ FromInteger (kx + ky)
  (+) (EConst x) (ENum (FromInteger ky)) = EConst $ x + fromInteger ky
  (+) (ENum (FromInteger kx)) (EConst y) = EConst $ fromInteger kx + y
  (+) x y = ERef $ do
    xIsZero <- isVal 0 x
    yIsZero <- isVal 0 y
    let ret
          | xIsZero = y
          | yIsZero = x
          | otherwise = ENum $ Add x y
    newMVar ret

  (-) (EConst x) (EConst y) = EConst (x-y)
  (-) (ENum (FromInteger kx)) (ENum (FromInteger ky)) = ENum $ FromInteger (kx - ky)
  (-) (EConst x) (ENum (FromInteger ky)) = EConst $ x - fromInteger ky
  (-) (ENum (FromInteger kx)) (EConst y) = EConst $ fromInteger kx - y
  (-) x y = ERef $ do
    xIsZero <- isVal 0 x
    yIsZero <- isVal 0 y
    let ret
          | xIsZero = negate y
          | yIsZero = x
          | otherwise = ENum $ Sub x y
    newMVar ret

  abs (EConst x) = EConst (abs x)
  abs (ENum (FromInteger k)) = ENum (FromInteger (abs k))
  abs x = eref $ ENum $ Abs x

  negate (EConst x) = EConst (negate x)
  negate (ENum (FromInteger k)) = ENum (FromInteger (negate k))
  negate x = eref $ ENum $ Negate x

  signum (EConst x) = EConst (signum x)
  signum (ENum (FromInteger k)) = ENum (FromInteger (signum k))
  signum x = eref $ ENum $ Signum x

  fromInteger = eref . ENum . FromInteger

instance (Fractional a, Eq a) => Fractional (Expr a) where
  (/) (EConst x) (EConst y) = EConst (x/y)
  (/) (ENum (FromInteger kx)) (ENum (FromInteger ky)) = EConst $ fromInteger kx / fromInteger ky
  (/) (EConst x) (ENum (FromInteger ky)) = EConst $ x / fromInteger ky
  (/) (ENum (FromInteger kx)) (EConst y) = EConst $ fromInteger kx / y
  (/) x y = ERef $ do
    xIsZero <- isVal 0 x
    yIsZero <- isVal 0 y
    yIsOne <- isVal 1 y
    let ret
          | yIsZero = error "Fractional (Expr a) divide by zero"
          | xIsZero = 0
          | yIsOne = x
          | otherwise = EFractional $ Div x y
    newMVar ret

  fromRational = eref . EFractional . FromRational

instance (Floating a, Eq a) => Floating (Expr a) where
  pi          = eref $ EConst pi
  x ** y      = eref $ EFloating $ Pow x y
  logBase x y = eref $ EFloating $ LogBase x y
  exp   = eref . EFloating . Exp
  log   = eref . EFloating . Log
  sin   = eref . EFloating . Sin
  cos   = eref . EFloating . Cos
  asin  = eref . EFloating . ASin
  atan  = eref . EFloating . ATan
  acos  = eref . EFloating . ACos
  sinh  = eref . EFloating . Sinh
  cosh  = eref . EFloating . Cosh
  tanh  = eref . EFloating . Tanh
  asinh = eref . EFloating . ASinh
  atanh = eref . EFloating . ATanh
  acosh = eref . EFloating . ACosh

instance Show a => Show (Expr a) where
  {-# NOINLINE show #-}
  show (ERef ref) = show $ unsafePerformIO $ readMVar $ unsafePerformIO ref
  show (ESym name) = name
  show (EConst a) = show a
  show (ENum x) = show x
  show (EFractional x) = show x
  show (EFloating x) = show x

sym :: String -> Expr a
sym = ESym

const' :: a -> Expr a
const' = EConst

readExpr :: Expr a -> IO (Expr a)
readExpr (ERef mx) = mx >>= readMVar >>= readExpr
readExpr x = return x

-- | Checks to see if an Expr is equal to a value. Does not perform any simplification first, beware.
isVal :: Eq a => a -> Expr a -> IO Bool
isVal v e@(ERef _) = readExpr e >>= isVal v
isVal v (EConst c) = return $ v == c
isVal v (ENum (FromInteger k)) = return $ v == fromInteger k
isVal _ _ = return False
