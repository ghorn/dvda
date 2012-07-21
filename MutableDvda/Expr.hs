{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
-- {-# Language StandaloneDeriving #-}

module MutableDvda.Expr ( Expr(..)
                        , isVal
                        , sym
                        , const'
                        , rad
                        , backprop
                        ) where

import Control.Concurrent.MVar
import System.IO.Unsafe ( unsafePerformIO )
import Data.Hashable ( Hashable, hash, combine )

import Dvda.Dual hiding ( fad, fad' )
--import Dvda.HashMap ( HashMap )
--import qualified Dvda.HashMap as HM
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HM

commutativeMul :: Bool
commutativeMul = True

commutativeAdd :: Bool
commutativeAdd = True

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

----------------------------- hashable instances --------------------------
instance Hashable a => Hashable (Nums a) where
  hash (Mul x y)  = hash "Mul" `combine` hx `combine` hy
    where
      hx' = hash x
      hy' = hash y
      (hx, hy)
        | commutativeMul = (min hx' hy', max hx' hy')
        | otherwise = (hx', hy')
  hash (Add x y)  = hash "Add" `combine` hx `combine` hy
    where
      hx' = hash x
      hy' = hash y
      (hx, hy)
        | commutativeAdd = (min hx' hy', max hx' hy')
        | otherwise = (hx', hy')
  hash (Sub x y)  = hash "Sub" `combine` hash x `combine` hash y
  hash (Negate x)      = hash "Negate"      `combine` hash x
  hash (Abs x)         = hash "Abs"         `combine` hash x
  hash (Signum x)      = hash "Signum"      `combine` hash x
  hash (FromInteger x) = hash "FromInteger" `combine` hash x

instance Hashable a => Hashable (Fractionals a) where
  hash (Div x y)  = hash "Div" `combine` hash x `combine` hash y
  hash (FromRational x) = hash "FromRational" `combine` hash x

instance Hashable a => Hashable (Floatings a) where
  hash (Pow x y) = hash "Pow" `combine` hash x `combine` hash y
  hash (LogBase x y) = hash "LogBase" `combine` hash x `combine` hash y
  hash (Exp x)   = hash "Exp"   `combine` hash x
  hash (Log x)   = hash "Log"   `combine` hash x
  hash (Sin x)   = hash "Sin"   `combine` hash x
  hash (Cos x)   = hash "Cos"   `combine` hash x
  hash (ASin x)  = hash "ASin"  `combine` hash x
  hash (ATan x)  = hash "ATan"  `combine` hash x
  hash (ACos x)  = hash "ACos"  `combine` hash x
  hash (Sinh x)  = hash "Sinh"  `combine` hash x
  hash (Cosh x)  = hash "Cosh"  `combine` hash x
  hash (Tanh x)  = hash "Tanh"  `combine` hash x
  hash (ASinh x) = hash "ASinh" `combine` hash x
  hash (ATanh x) = hash "ATanh" `combine` hash x
  hash (ACosh x) = hash "ACosh" `combine` hash x

instance Hashable a => Hashable (Expr a) where
  hash (ERef _) = error "can't hash ERef"
  hash (ESym name)     = hash "ESym"        `combine` hash name
  hash (EConst x)      = hash "EConst"      `combine` hash x
  hash (ENum x)        = hash "ENum"        `combine` hash x
  hash (EFractional x) = hash "EFractional" `combine` hash x
  hash (EFloating x)   = hash "EFloating"   `combine` hash x

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
  exp         = eref . EFloating . Exp
  log         = eref . EFloating . Log
  sin         = eref . EFloating . Sin
  cos         = eref . EFloating . Cos
  asin        = eref . EFloating . ASin
  atan        = eref . EFloating . ATan
  acos        = eref . EFloating . ACos
  sinh        = eref . EFloating . Sinh
  cosh        = eref . EFloating . Cosh
  tanh        = eref . EFloating . Tanh
  asinh       = eref . EFloating . ASinh
  atanh       = eref . EFloating . ATanh
  acosh       = eref . EFloating . ACosh

instance Show a => Show (Expr a) where
  show (ERef ref) = show $ unsafePerformIO $ readMVar $ unsafePerformIO ref
--  show (ERef ref) = "ERef"
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

fad :: Num a => (Dual a -> [Dual a]) -> a -> [a]
fad f x = map dualPerturbation $ f (Dual x 1)

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

rad :: (Num a, Eq a, Hashable (Expr a)) => Expr a -> IO (HashMap (Expr a) (Expr a))
rad x = do
  sensitivities <- backprop 1 x
  return $ HM.fromListWith (+) sensitivities

