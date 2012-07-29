{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveDataTypeable #-}
{-# Language FlexibleInstances #-}

module MutableDvda.Expr ( Expr(..)
                        , GExpr(..)
                        , Nums(..)
                        , Fractionals(..)
                        , Floatings(..)
                        , Sym(..)
                        , isVal
                        , sym
                        , symDependent
                        , symDependentN
                        , const'
                        , getParents
                        ) where

import Control.Applicative
import Data.Data ( Data, Typeable, Typeable1, Typeable2 )
import Data.Hashable ( Hashable, hash, combine )

import MutableDvda.Reify ( MuRef(..) )

commutativeMul :: Bool
commutativeMul = True

commutativeAdd :: Bool
commutativeAdd = True

data Sym = Sym String                  -- doesn't depend on independent variable, or is an independent variable
         | SymDependent String Int Sym -- depends on independent variable, Int specifies the nth derivative
           deriving Eq

instance Show Sym where
  show (Sym name) = name
  show (SymDependent name k s) = name ++ replicate k '\'' ++ "(" ++ show s ++ ")"

data Expr a where
  ESym :: Sym -> Expr a
  EConst :: a -> Expr a
  ENum :: Num a => Nums (Expr a) -> Expr a
  EFractional :: Fractional a => Fractionals (Expr a) -> Expr a
  EFloating :: Floating a => Floatings (Expr a) -> Expr a

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

deriving instance Data Sym
deriving instance Data a => Data (Nums a)
deriving instance Data a => Data (Fractionals a)
deriving instance Data a => Data (Floatings a)
deriving instance (Data a, Floating a) => Data (Expr a)
deriving instance (Data a, Data b, Floating a) => Data (GExpr a b)

deriving instance Typeable Sym
deriving instance Typeable1 Nums
deriving instance Typeable1 Fractionals
deriving instance Typeable1 Floatings
deriving instance Typeable1 Expr
deriving instance Typeable2 GExpr

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
  showsPrec d (Add x y) = showsInfixBinary d 6 " + " x y
  showsPrec d (Sub x y) = showsInfixBinary d 6 " - " x y
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
  showsPrec _ (ESym s) = showString (show s)
  showsPrec _ (EConst x) = showString (show x)
  showsPrec d (ENum x) = showsPrec d x
  showsPrec d (EFractional x) = showsPrec d x
  showsPrec d (EFloating x) = showsPrec d x


----------------------- Eq instances -------------------------
instance Eq a => Eq (Expr a) where
  (==) (ESym x) (ESym y) = x == y
  (==) (EConst x) (EConst y) = x == y
  (==) (ENum x) (ENum y) = x == y
  (==) (EFractional x) (EFractional y) = x == y
  (==) (EFloating x) (EFloating y) = x == y
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
instance Hashable Sym where
  hash (Sym name) = hash "Sym" `combine` hash name
  hash (SymDependent name k s) = hash ("SymDependent", name, k, s)

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
    | otherwise = ENum $ Mul x y

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
    | otherwise = ENum $ Add x y

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
    | otherwise = ENum $ Sub x y

  abs (EConst x) = EConst (abs x)
  abs (ENum (FromInteger k)) = ENum (FromInteger (abs k))
  abs (EFractional (FromRational r)) = EFractional (FromRational (abs r))
  abs x = ENum $ Abs x

  negate (EConst x) = EConst (negate x)
  negate (ENum (FromInteger k)) = ENum (FromInteger (negate k))
  negate (EFractional (FromRational r)) = EFractional (FromRational (negate r))
  negate x = ENum $ Negate x

  signum (EConst x) = EConst (signum x)
  signum (ENum (FromInteger k)) = ENum (FromInteger (signum k))
  signum (EFractional (FromRational r)) = EFractional (FromRational (signum r))
  signum x = ENum $ Signum x

  fromInteger = ENum . FromInteger

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
    | otherwise = EFractional $ Div x y

  fromRational = EFractional . FromRational

instance (Floating a, Eq a) => Floating (Expr a) where
  pi          = EConst pi
  x ** y      = EFloating $ Pow x y
  logBase x y = EFloating $ LogBase x y
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
applyFloatingUn (_,f) x = EFloating (f x)

---------------------------------- GExprs --------------------------------
data GExpr a b where
  GSym :: Sym -> GExpr a b
  GConst :: a -> GExpr a b
  GNum :: Num a => Nums b -> GExpr a b
  GFractional :: Fractional a => Fractionals b -> GExpr a b
  GFloating :: Floating a => Floatings b -> GExpr a b

-- you might use this to use Expr's nice Show instance
gexprToExpr :: (b -> Expr a) -> GExpr a b -> Expr a
gexprToExpr _ (GSym s@(Sym _)) = ESym s
gexprToExpr _ (GSym sd@(SymDependent _ _ _)) = ESym sd
gexprToExpr _ (GConst c) = EConst c
gexprToExpr f (GNum (Mul x y))               = ENum (Mul (f x) (f y))
gexprToExpr f (GNum (Add x y))               = ENum (Add (f x) (f y))
gexprToExpr f (GNum (Sub x y))               = ENum (Sub (f x) (f y))
gexprToExpr f (GNum (Negate x))              = ENum (Negate (f x))
gexprToExpr f (GNum (Abs x))                 = ENum (Abs (f x))
gexprToExpr f (GNum (Signum x))              = ENum (Signum (f x))
gexprToExpr _ (GNum (FromInteger x))         = ENum (FromInteger x)
gexprToExpr f (GFractional (Div x y))        = EFractional (Div (f x) (f y))
gexprToExpr _ (GFractional (FromRational x)) = EFractional (FromRational x)
gexprToExpr f (GFloating (Pow x y))          = EFloating (Pow (f x) (f y))
gexprToExpr f (GFloating (LogBase x y))      = EFloating (LogBase (f x) (f y))
gexprToExpr f (GFloating (Exp x))            = EFloating (Exp   (f x))
gexprToExpr f (GFloating (Log x))            = EFloating (Log   (f x))
gexprToExpr f (GFloating (Sin x))            = EFloating (Sin   (f x))
gexprToExpr f (GFloating (Cos x))            = EFloating (Cos   (f x))
gexprToExpr f (GFloating (ASin x))           = EFloating (ASin  (f x))
gexprToExpr f (GFloating (ATan x))           = EFloating (ATan  (f x))
gexprToExpr f (GFloating (ACos x))           = EFloating (ACos  (f x))
gexprToExpr f (GFloating (Sinh x))           = EFloating (Sinh  (f x))
gexprToExpr f (GFloating (Cosh x))           = EFloating (Cosh  (f x))
gexprToExpr f (GFloating (Tanh x))           = EFloating (Tanh  (f x))
gexprToExpr f (GFloating (ASinh x))          = EFloating (ASinh (f x))
gexprToExpr f (GFloating (ATanh x))          = EFloating (ATanh (f x))
gexprToExpr f (GFloating (ACosh x))          = EFloating (ACosh (f x))

getParents :: GExpr a b -> [b]
getParents (GSym _)                       = []
getParents (GConst _)                     = []
getParents (GNum (Mul x y))               = [x,y]
getParents (GNum (Add x y))               = [x,y]
getParents (GNum (Sub x y))               = [x,y]
getParents (GNum (Negate x))              = [x]
getParents (GNum (Abs x))                 = [x]
getParents (GNum (Signum x))              = [x]
getParents (GNum (FromInteger _))         = []
getParents (GFractional (Div x y))        = [x,y]
getParents (GFractional (FromRational _)) = []
getParents (GFloating (Pow x y))          = [x,y]
getParents (GFloating (LogBase x y))      = [x,y]
getParents (GFloating (Exp x))            = [x]
getParents (GFloating (Log x))            = [x]
getParents (GFloating (Sin x))            = [x]
getParents (GFloating (Cos x))            = [x]
getParents (GFloating (ASin x))           = [x]
getParents (GFloating (ATan x))           = [x]
getParents (GFloating (ACos x))           = [x]
getParents (GFloating (Sinh x))           = [x]
getParents (GFloating (Cosh x))           = [x]
getParents (GFloating (Tanh x))           = [x]
getParents (GFloating (ASinh x))          = [x]
getParents (GFloating (ATanh x))          = [x]
getParents (GFloating (ACosh x))          = [x]

instance (Show a, Show b) => Show (GExpr a b) where
  show = show . (gexprToExpr (\x -> ESym (Sym ("{" ++ show x ++ "}"))))
  
deriving instance (Eq a, Eq b) => Eq (GExpr a b)

instance (Hashable a, Hashable b) => Hashable (GExpr a b) where
  hash (GSym name)     = hash "GSym"        `combine` hash name
  hash (GConst x)      = hash "GConst"      `combine` hash x
  hash (GNum x)        = hash "GNum"        `combine` hash x
  hash (GFractional x) = hash "GFractional" `combine` hash x
  hash (GFloating x)   = hash "GFloating"   `combine` hash x

instance MuRef (Expr a) where
  type DeRef (Expr a) = GExpr a
  mapDeRef _ (ESym name) = pure (GSym name)
  mapDeRef _ (EConst c)  = pure (GConst c)
  mapDeRef f (ENum (Mul x y)) = GNum <$> (Mul <$> (f x) <*> (f y))
  mapDeRef f (ENum (Add x y)) = GNum <$> (Add <$> (f x) <*> (f y))
  mapDeRef f (ENum (Sub x y)) = GNum <$> (Sub <$> (f x) <*> (f y))
  mapDeRef f (ENum (Negate x)) = GNum <$> (Negate <$> (f x))
  mapDeRef f (ENum (Abs x)) = GNum <$> (Negate <$> (f x))
  mapDeRef f (ENum (Signum x)) = GNum <$> (Signum <$> (f x))
  mapDeRef _ (ENum (FromInteger k)) = pure $ GNum (FromInteger k)
  
  mapDeRef f (EFractional (Div x y)) = GFractional <$> (Div <$> (f x) <*> (f y))
  mapDeRef _ (EFractional (FromRational x)) = pure $ GFractional (FromRational x)

  mapDeRef f (EFloating (Pow x y))     = GFloating <$> (Pow <$> (f x) <*> (f y))
  mapDeRef f (EFloating (LogBase x y)) = GFloating <$> (LogBase <$> (f x) <*> (f y))
  mapDeRef f (EFloating (Exp   x))     = GFloating <$> (Exp   <$> (f x))
  mapDeRef f (EFloating (Log   x))     = GFloating <$> (Log   <$> (f x))
  mapDeRef f (EFloating (Sin   x))     = GFloating <$> (Sin   <$> (f x))
  mapDeRef f (EFloating (Cos   x))     = GFloating <$> (Cos   <$> (f x))
  mapDeRef f (EFloating (ASin  x))     = GFloating <$> (ASin  <$> (f x))
  mapDeRef f (EFloating (ATan  x))     = GFloating <$> (ATan  <$> (f x))
  mapDeRef f (EFloating (ACos  x))     = GFloating <$> (ACos  <$> (f x))
  mapDeRef f (EFloating (Sinh  x))     = GFloating <$> (Sinh  <$> (f x))
  mapDeRef f (EFloating (Cosh  x))     = GFloating <$> (Cosh  <$> (f x))
  mapDeRef f (EFloating (Tanh  x))     = GFloating <$> (Tanh  <$> (f x))
  mapDeRef f (EFloating (ASinh x))     = GFloating <$> (ASinh <$> (f x))
  mapDeRef f (EFloating (ATanh x))     = GFloating <$> (ATanh <$> (f x))
  mapDeRef f (EFloating (ACosh x))     = GFloating <$> (ACosh <$> (f x))

---------------------------------- utility functions -------------------------------

-- | symbolic scalar
sym :: String -> Expr a
sym = ESym . Sym

-- | Symbolic scalar which is a function of some independent variable, like time.
-- .
-- This lets you do d(f(g(t)))/dt == f'(g(t))*g'(t)
symDependent :: String -> Expr a -> Expr a
symDependent name s = symDependentN name s 0

-- | same as symDependent but it can start as the Nth derivative
symDependentN :: String -> Expr a -> Int -> Expr a
symDependentN name (ESym s) n = ESym (SymDependent name n s)
symDependentN _ _ _ = error "symDependent got non ESym dependency"


const' :: a -> Expr a
const' = EConst

-- | Checks to see if an Expr is equal to a value
isVal :: Eq a => a -> Expr a -> Bool
isVal v (EConst c) = v == c
isVal v (ENum (FromInteger k)) = v == fromInteger k
isVal v (EFractional (FromRational r)) = v == fromRational r
isVal _ _ = False
