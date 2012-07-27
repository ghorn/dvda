{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}

module MutableDvda.Expr ( Expr(..)
                        , Nums(..)
                        , Fractionals(..)
                        , Floatings(..)
                        , GraphRef(..)
                        , readExpr
                        , isVal
                        , sym
                        , const'
                        ) where

import Control.Concurrent.MVar ( MVar, newMVar, readMVar )
import Data.Hashable ( Hashable, hash, combine )
import System.IO.Unsafe ( unsafePerformIO )

commutativeMul :: Bool
commutativeMul = True

commutativeAdd :: Bool
commutativeAdd = True

data GraphRef = GraphRef Int deriving (Eq, Show)
instance Hashable GraphRef where
  hash (GraphRef k) = hash "GraphRef" `combine` k


data Expr a where
  ERef :: MVar (Expr a) -> Expr a
  ESym :: String -> Expr a
  EConst :: a -> Expr a
  ENum :: Num a => Nums (Expr a) -> Expr a
  EFractional :: Fractional a => Fractionals (Expr a) -> Expr a
  EFloating :: Floating a => Floatings (Expr a) -> Expr a
  EGraphRef :: Expr a -> GraphRef -> Expr a

data Nums a = Mul a a
            | Add a a
            | Sub a a
            | Negate a
            | Abs a
            | Signum a
            | FromInteger Integer

data Fractionals a = Div a a
                   | FromRational Rational deriving Eq

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
                 | ACosh a deriving Eq

----------------------- Show instances -------------------------
showsInfixBinary :: (Show a, Show b) => Int -> Int -> String -> a -> b -> ShowS
showsInfixBinary d prec op u v = showParen (d > prec) $
                                 showsPrec prec u .
                                 showString op .
                                 showsPrec prec v

showsUnary :: Show a => Int -> Int -> String -> a -> ShowS
showsUnary d prec op u = showParen (d > prec) $
                         showString op .
                         showsPrec prec u

instance Show a => Show (Nums a) where
  showsPrec d (Mul x y) = showsInfixBinary d 7 " * " x y
  showsPrec d (Add x y) = showsInfixBinary d 7 " + " x y
  showsPrec d (Sub x y) = showsInfixBinary d 7 " + " x y
  showsPrec d (Negate x) = showsUnary d 7 "-" x
  showsPrec d (Abs x) = showsUnary d 10 "abs" x
  showsPrec d (Signum x) = showsUnary d 10 "signum" x
  showsPrec _ (FromInteger k) = showString (show k)

instance Show a => Show (Fractionals a) where
  showsPrec d (Div x y) = showsInfixBinary d 7 " / " x y
  showsPrec _ (FromRational r) = showString $ show (fromRational r :: Double)

instance Show a => Show (Floatings a) where
  showsPrec d (Pow x y) = showsInfixBinary d 8 " ** " x y
  showsPrec d (LogBase x y) = showParen (d > 10) $ showString $ "logBase(" ++ show x ++ ", " ++ show y ++ ")"
  showsPrec d (Exp x)   = showsUnary d 10 "exp" x
  showsPrec d (Log x)   = showsUnary d 10 "log" x
  showsPrec d (Sin x)   = showsUnary d 10 "sin" x
  showsPrec d (Cos x)   = showsUnary d 10 "cos" x
  showsPrec d (ASin x)  = showsUnary d 10 "asin" x
  showsPrec d (ATan x)  = showsUnary d 10 "atan" x
  showsPrec d (ACos x)  = showsUnary d 10 "acos" x
  showsPrec d (Sinh x)  = showsUnary d 10 "sinh" x
  showsPrec d (Cosh x)  = showsUnary d 10 "cosh" x
  showsPrec d (Tanh x)  = showsUnary d 10 "tanh" x
  showsPrec d (ASinh x) = showsUnary d 10 "asinh" x
  showsPrec d (ATanh x) = showsUnary d 10 "atanh" x
  showsPrec d (ACosh x) = showsUnary d 10 "acosh" x

instance Show a => Show (Expr a) where
--  showsPrec _ (ERef _) = showString "ERef"
  showsPrec d (ERef mv) = showsPrec d $ unsafePerformIO (readMVar mv)
  showsPrec _ (ESym name) = showString name
  showsPrec _ (EConst x) = showString (show x)
  showsPrec d (ENum x) = showsPrec d x
  showsPrec d (EFractional x) = showsPrec d x
  showsPrec d (EFloating x) = showsPrec d x
  showsPrec d (EGraphRef x k) = showString ("EGraphRef(" ++ show k ++ "): ") .
                                showsPrec d x


----------------------- Eq instances -------------------------
instance Eq a => Eq (Expr a) where
  (==) x@(ERef mx) y@(ERef my) = unsafePerformIO $ do
    return $ mx == my || unsafePerformIO (readExpr x) == unsafePerformIO (readExpr y)
  (==) x@(ERef _) y = (==) (unsafePerformIO (readExpr x)) y
  (==) x y@(ERef _) = (==) x (unsafePerformIO (readExpr y))
  (==) (ESym x) (ESym y) = x == y
  (==) (EConst x) (EConst y) = x == y
  (==) (ENum x) (ENum y) = x == y
  (==) (EFractional x) (EFractional y) = x == y
  (==) (EFloating x) (EFloating y) = x == y
  (==) (EGraphRef x _) y = x == y
  (==) x (EGraphRef y _) = x == y
  (==) _ _ = False

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
  hash e@(ERef _)      = hash (unsafePerformIO $ readExpr e)
  hash (ESym name)     = hash "ESym"        `combine` hash name
  hash (EConst x)      = hash "EConst"      `combine` hash x
  hash (ENum x)        = hash "ENum"        `combine` hash x
  hash (EFractional x) = hash "EFractional" `combine` hash x
  hash (EFloating x)   = hash "EFloating"   `combine` hash x
  hash (EGraphRef x _) = hash x

--deriving instance Enum a => Enum (Nums a)
--deriving instance Bounded a => Bounded (Nums a)

--deriving instance Enum a => Enum (Fractionals a)
--deriving instance Bounded a => Bounded (Fractionals a)

--deriving instance Enum a => Enum (Floatings a)
--deriving instance Bounded a => Bounded (Floatings a)

eref :: Expr a -> Expr a
eref x@(ERef _) = x
eref x = ERef (unsafePerformIO $ newMVar x)

instance (Num a, Eq a) => Num (Expr a) where
  (*) (EConst x) (EConst y) = EConst (x*y)
  (*) (ENum (FromInteger kx)) (ENum (FromInteger ky)) = ENum $ FromInteger (kx * ky)
  (*) (EFractional (FromRational rx)) (EFractional (FromRational ry)) = EFractional $ FromRational (rx * ry)
  (*) (EConst x) (ENum (FromInteger ky)) = EConst $ x * fromInteger ky
  (*) (ENum (FromInteger kx)) (EConst y) = EConst $ fromInteger kx * y
  (*) (EConst x) (EFractional (FromRational ry)) = EConst $ x * fromRational ry
  (*) (EFractional (FromRational rx)) (EConst y) = EConst $ fromRational rx * y
  (*) (ENum (FromInteger kx)) (EFractional (FromRational ry)) = EFractional $ FromRational (fromInteger kx * ry)
  (*) (EFractional (FromRational rx)) (ENum (FromInteger ky)) = EFractional $ FromRational (rx * fromInteger ky)
  (*) x y
    | isVal 0 x || isVal 0 y = 0
    | isVal 1 x = y
    | isVal 1 y = x
    | otherwise = eref $ ENum $ Mul x y

  (+) (EConst x) (EConst y) = EConst (x+y)
  (+) (ENum (FromInteger kx)) (ENum (FromInteger ky)) = ENum $ FromInteger (kx + ky)
  (+) (EFractional (FromRational rx)) (EFractional (FromRational ry)) = EFractional $ FromRational (rx + ry)
  (+) (EConst x) (ENum (FromInteger ky)) = EConst $ x + fromInteger ky
  (+) (ENum (FromInteger kx)) (EConst y) = EConst $ fromInteger kx + y
  (+) (EConst x) (EFractional (FromRational ry)) = EConst $ x + fromRational ry
  (+) (EFractional (FromRational rx)) (EConst y) = EConst $ fromRational rx + y
  (+) (ENum (FromInteger kx)) (EFractional (FromRational ry)) = EFractional $ FromRational (fromInteger kx + ry)
  (+) (EFractional (FromRational rx)) (ENum (FromInteger ky)) = EFractional $ FromRational (rx + fromInteger ky)
  (+) x y
    | isVal 0 x = y
    | isVal 0 y = x
    | otherwise = eref $ ENum $ Add x y

  (-) (EConst x) (EConst y) = EConst (x-y)
  (-) (ENum (FromInteger kx)) (ENum (FromInteger ky)) = ENum $ FromInteger (kx - ky)
  (-) (EFractional (FromRational rx)) (EFractional (FromRational ry)) = EFractional $ FromRational (rx - ry)
  (-) (EConst x) (ENum (FromInteger ky)) = EConst $ x - fromInteger ky
  (-) (ENum (FromInteger kx)) (EConst y) = EConst $ fromInteger kx - y
  (-) (EConst x) (EFractional (FromRational ry)) = EConst $ x - fromRational ry
  (-) (EFractional (FromRational rx)) (EConst y) = EConst $ fromRational rx - y
  (-) (ENum (FromInteger kx)) (EFractional (FromRational ry)) = EFractional $ FromRational (fromInteger kx - ry)
  (-) (EFractional (FromRational rx)) (ENum (FromInteger ky)) = EFractional $ FromRational (rx - fromInteger ky)
  (-) x y
    | isVal 0 x = negate y
    | isVal 0 y = x
    | otherwise = eref $ ENum $ Sub x y

  abs (EConst x) = EConst (abs x)
  abs (ENum (FromInteger k)) = ENum (FromInteger (abs k))
  abs (EFractional (FromRational r)) = EFractional (FromRational (abs r))
  abs x = eref $ ENum $ Abs x

  negate (EConst x) = EConst (negate x)
  negate (ENum (FromInteger k)) = ENum (FromInteger (negate k))
  negate (EFractional (FromRational r)) = EFractional (FromRational (negate r))
  negate x = eref $ ENum $ Negate x

  signum (EConst x) = EConst (signum x)
  signum (ENum (FromInteger k)) = ENum (FromInteger (signum k))
  signum (EFractional (FromRational r)) = EFractional (FromRational (signum r))
  signum x = eref $ ENum $ Signum x

  fromInteger = eref . ENum . FromInteger

instance (Fractional a, Eq a) => Fractional (Expr a) where
  (/) (EConst x) (EConst y) = EConst (x/y)
--  (/) (ENum (FromInteger kx)) (ENum (FromInteger ky)) = ENum $ FromInteger (kx / ky)
  (/) (EFractional (FromRational rx)) (EFractional (FromRational ry)) = EFractional $ FromRational (rx / ry)
  (/) (EConst x) (ENum (FromInteger ky)) = EConst $ x / fromInteger ky
  (/) (ENum (FromInteger kx)) (EConst y) = EConst $ fromInteger kx / y
  (/) (EConst x) (EFractional (FromRational ry)) = EConst $ x / fromRational ry
  (/) (EFractional (FromRational rx)) (EConst y) = EConst $ fromRational rx / y
  (/) (ENum (FromInteger kx)) (EFractional (FromRational ry)) = EFractional $ FromRational (fromInteger kx / ry)
  (/) (EFractional (FromRational rx)) (ENum (FromInteger ky)) = EFractional $ FromRational (rx / fromInteger ky)
  (/) x y
    | isVal 0 y = error "Fractional (Expr a) divide by zero"
    | isVal 0 x = 0
    | isVal 1 y = x
    | otherwise = eref $ EFractional $ Div x y

  fromRational = eref . EFractional . FromRational

instance (Floating a, Eq a) => Floating (Expr a) where
  pi          = eref $ EConst pi
  x ** y      = eref $ EFloating $ Pow x y
  logBase x y = eref $ EFloating $ LogBase x y
  exp         = applyFloatingUn (  exp,   Exp)
  log         = applyFloatingUn (  log,   Log)
  sin         = applyFloatingUn (  sin,   Sin)
  cos         = applyFloatingUn (  cos,   Cos)
  asin        = applyFloatingUn ( asin,  ASin)
  atan        = applyFloatingUn ( atan,  ATan)
  acos        = applyFloatingUn ( acos,  ACos)
  sinh        = applyFloatingUn ( sinh,  Sinh)
  cosh        = applyFloatingUn ( cosh,  Cosh)
  tanh        = applyFloatingUn ( tanh,  Tanh)
  asinh       = applyFloatingUn (asinh, ASinh)
  atanh       = applyFloatingUn (atanh, ATanh)
  acosh       = applyFloatingUn (acosh, ACosh)

applyFloatingUn :: Floating a => (t -> a, Expr t -> Floatings (Expr a)) -> Expr t -> Expr a
applyFloatingUn (f,_) (EConst x) = EConst (f x)
applyFloatingUn (f,_) (ENum (FromInteger x)) = EConst (f $ fromInteger x)
applyFloatingUn (f,_) (EFractional (FromRational x)) = EConst (f $ fromRational x)
applyFloatingUn (_,f) x = eref $ EFloating (f x)


sym :: String -> Expr a
sym name = eref (ESym name)

const' :: a -> Expr a
const' = EConst

readExpr :: Expr a -> IO (Expr a)
readExpr (ERef mx) = readMVar mx >>= readExpr
readExpr x = return x

-- | Checks to see if an Expr is equal to a value
isVal :: Eq a => a -> Expr a -> Bool
isVal v e@(ERef _) = isVal v (unsafePerformIO $ readExpr e)
isVal v (EConst c) = v == c
isVal v (ENum (FromInteger k)) = v == fromInteger k
isVal v (EFractional (FromRational r)) = v == fromRational r
isVal _ _ = False
