{-# OPTIONS_GHC -Wall #-}
{-# Language StandaloneDeriving #-}
{-# Language GADTs #-}
{-# Language TypeFamilies #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}

module Dvda.Expr ( Expr(..)
                 , GExpr(..)
                 , Nums(..)
                 , Fractionals(..)
                 , Floatings(..)
                 , Sym(..)
                 , sym
                 , symDependent
                 , symDependentN
                 , const'
                 , extractLinearPart
                 , getConst
                 , substitute
                 , sketchySubstitute
                 , foldExpr
                 , fromNeg
                 ) where

import Control.Applicative ( (<$>), pure )
import Data.Hashable ( Hashable(..), hash )
import Data.Ratio ( (%) )
import GHC.Generics ( Generic )
import Data.Monoid ( mempty )
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import qualified Dvda.HashMap as HM

commutativeMul :: Bool
commutativeMul = True

commutativeAdd :: Bool
commutativeAdd = True

data Sym =
    Sym String -- doesn't depend on independent variable, or is an independent variable
  | SymDependent String Int Sym -- depends on independent variable, Int specifies the nth derivative
  deriving (Eq, Ord, Generic)

instance Show Sym where
  showsPrec d (Sym name) = showParen (d >= 9) $ showString name
  showsPrec d (SymDependent name k s) =
    showParen (d >= 9) $
    showString $ name ++ replicate k '\'' ++ "(" ++ show s ++ ")"

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
            deriving (Ord, Generic, Functor, F.Foldable, T.Traversable)

data Fractionals a = Div a a
                   | FromRational Rational
                   deriving (Eq, Ord, Generic, Functor, F.Foldable, T.Traversable)

data Floatings a = Pow a a
                 | LogBase a a
                 | Exp a
                 | Log a
                 | Sin a
                 | Cos a
                 | Tan a
                 | ASin a
                 | ATan a
                 | ACos a
                 | Sinh a
                 | Cosh a
                 | Tanh a
                 | ASinh a
                 | ATanh a
                 | ACosh a
                 deriving (Eq, Ord, Generic, Functor, F.Foldable, T.Traversable)

----------------------- Show instances -------------------------
showsInfixBinary :: (Show a, Show b) => Int -> Int -> String -> a -> b -> ShowS
showsInfixBinary d prec op u v = showParen (d >= prec) $
                                 showsPrec prec u .
                                 showString op .
                                 showsPrec prec v

showsUnary :: Show a => Int -> Int -> String -> a -> ShowS
showsUnary d prec op u = showParen (d >= prec) $
                         showString op .
                         showsPrec prec u

instance Show a => Show (Nums a) where
  showsPrec d (Mul x y) = showsInfixBinary d 7 " * " x y
  showsPrec d (Add x y) = showsInfixBinary d 6 " + " x y
  showsPrec d (Sub x y) = showsInfixBinary d 6 " - " x y
  showsPrec d (Negate x) = showsUnary d 7 "-" x
  showsPrec d (Abs x) = showsUnary d 10 "abs" x
  showsPrec d (Signum x) = showsUnary d 10 "signum" x
  showsPrec d (FromInteger k) = showParen (d >= 9) $ showString (show k)

instance Show a => Show (Fractionals a) where
  showsPrec d (Div x y) = showsInfixBinary d 7 " / " x y
  showsPrec d (FromRational r) = showParen (d >= 9) $ showString $ show (fromRational r :: Double)

instance Show a => Show (Floatings a) where
  showsPrec d (Pow x y) = showsInfixBinary d 8 " ** " x y
  showsPrec d (LogBase x y) = showParen (d > 10) $ showString $ "logBase(" ++ show x ++ ", " ++ show y ++ ")"
  showsPrec d (Exp x)   = showsUnary d 10 "exp" x
  showsPrec d (Log x)   = showsUnary d 10 "log" x
  showsPrec d (Sin x)   = showsUnary d 10 "sin" x
  showsPrec d (Cos x)   = showsUnary d 10 "cos" x
  showsPrec d (Tan x)   = showsUnary d 10 "tan" x
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
  showsPrec d (ESym s) = showParen (d > 9) $
                         showString (show s)
  showsPrec d (EConst x) = showParen (d >= 9) $ showString (show x)
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
instance Hashable Sym

instance Hashable a => Hashable (Nums a) where
  hashWithSalt s (Mul x y)  = s `hashWithSalt` "Mul" `hashWithSalt` hx `hashWithSalt` hy
    where
      hx' = hash x
      hy' = hash y
      (hx, hy)
        | commutativeMul = (min hx' hy', max hx' hy')
        | otherwise = (hx', hy')
  hashWithSalt s (Add x y)  = s `hashWithSalt` "Add" `hashWithSalt` hx `hashWithSalt` hy
    where
      hx' = hash x
      hy' = hash y
      (hx, hy)
        | commutativeAdd = (min hx' hy', max hx' hy')
        | otherwise = (hx', hy')
  hashWithSalt s (Sub x y)  = s `hashWithSalt` "Sub" `hashWithSalt` x `hashWithSalt` y
  hashWithSalt s (Negate x)      = s `hashWithSalt` "Negate"      `hashWithSalt` x
  hashWithSalt s (Abs x)         = s `hashWithSalt` "Abs"         `hashWithSalt` x
  hashWithSalt s (Signum x)      = s `hashWithSalt` "Signum"      `hashWithSalt` x
  hashWithSalt s (FromInteger x) = s `hashWithSalt` "FromInteger" `hashWithSalt` x

instance Hashable a => Hashable (Fractionals a)
instance Hashable a => Hashable (Floatings a)

instance Hashable a => Hashable (Expr a) where
  hashWithSalt s (ESym name)     = s `hashWithSalt` "ESym"        `hashWithSalt` name
  hashWithSalt s (EConst x)      = s `hashWithSalt` "EConst"      `hashWithSalt` x
  hashWithSalt s (ENum x)        = s `hashWithSalt` "ENum"        `hashWithSalt` x
  hashWithSalt s (EFractional x) = s `hashWithSalt` "EFractional" `hashWithSalt` x
  hashWithSalt s (EFloating x)   = s `hashWithSalt` "EFloating"   `hashWithSalt` x

fromNeg :: Expr a -> Maybe (Expr a)
fromNeg (ENum (Negate x)) = Just x
fromNeg (ENum (FromInteger k))
  | k < 0 = Just (ENum (FromInteger (abs k)))
fromNeg (EFractional (FromRational r))
  | r < 0 = Just (EFractional (FromRational (abs r)))
--fromNeg (EConst c)
--  | c < 0 = Just (EConst (abs c))
fromNeg _ = Nothing

instance (Eq a, Num a) => Num (Expr a) where
  (*) x y
    | isVal 0 x || isVal 0 y = 0
    | isVal 1 x = y
    | isVal 1 y = x
  (*) (EConst x) (EConst y) = EConst (x*y)
  (*) (ENum (FromInteger kx)) (ENum (FromInteger ky)) = ENum $ FromInteger (kx * ky)
  (*) (EFractional (FromRational rx)) (EFractional (FromRational ry)) = EFractional $ FromRational (rx * ry)
  (*) (EConst x) (ENum (FromInteger ky)) = EConst $ x * fromInteger ky
  (*) (ENum (FromInteger kx)) (EConst y) = EConst $ fromInteger kx * y
  (*) (EConst x) (EFractional (FromRational ry)) = EConst $ x * fromRational ry
  (*) (EFractional (FromRational rx)) (EConst y) = EConst $ fromRational rx * y
  (*) (ENum (FromInteger kx)) (EFractional (FromRational ry)) = EFractional $ FromRational (fromInteger kx * ry)
  (*) (EFractional (FromRational rx)) (ENum (FromInteger ky)) = EFractional $ FromRational (rx * fromInteger ky)
  (*) x y = case (fromNeg x, fromNeg y) of
              (Just x', Just y') -> x' * y'
              (Nothing, Just y') -> negate (x  * y')
              (Just x', Nothing) -> negate (x' * y )
              _ -> ENum $ Mul x y

  (+) x y
    | isVal 0 x = y
    | isVal 0 y = x
    | x == negate y = 0
  (+) (EConst x) (EConst y) = EConst (x+y)
  (+) (ENum (FromInteger kx)) (ENum (FromInteger ky)) = ENum $ FromInteger (kx + ky)
  (+) (EFractional (FromRational rx)) (EFractional (FromRational ry)) = EFractional $ FromRational (rx + ry)
  (+) (EConst x) (ENum (FromInteger ky)) = EConst $ x + fromInteger ky
  (+) (ENum (FromInteger kx)) (EConst y) = EConst $ fromInteger kx + y
  (+) (EConst x) (EFractional (FromRational ry)) = EConst $ x + fromRational ry
  (+) (EFractional (FromRational rx)) (EConst y) = EConst $ fromRational rx + y
  (+) (ENum (FromInteger kx)) (EFractional (FromRational ry)) = EFractional $ FromRational (fromInteger kx + ry)
  (+) (EFractional (FromRational rx)) (ENum (FromInteger ky)) = EFractional $ FromRational (rx + fromInteger ky)
  (+) x y = case (fromNeg x, fromNeg y) of
              (Just x', Just y') -> negate (x' + y')
              (Nothing, Just y') -> x  - y'
              (Just x', Nothing) -> y  - x'
              _ -> ENum $ Add x y

  (-) x y
    | isVal 0 x = negate y
    | isVal 0 y = x
    | x == y = 0
  (-) (EConst x) (EConst y) = EConst (x-y)
  (-) (ENum (FromInteger kx)) (ENum (FromInteger ky)) = ENum $ FromInteger (kx - ky)
  (-) (EFractional (FromRational rx)) (EFractional (FromRational ry)) = EFractional $ FromRational (rx - ry)
  (-) (EConst x) (ENum (FromInteger ky)) = EConst $ x - fromInteger ky
  (-) (ENum (FromInteger kx)) (EConst y) = EConst $ fromInteger kx - y
  (-) (EConst x) (EFractional (FromRational ry)) = EConst $ x - fromRational ry
  (-) (EFractional (FromRational rx)) (EConst y) = EConst $ fromRational rx - y
  (-) (ENum (FromInteger kx)) (EFractional (FromRational ry)) = EFractional $ FromRational (fromInteger kx - ry)
  (-) (EFractional (FromRational rx)) (ENum (FromInteger ky)) = EFractional $ FromRational (rx - fromInteger ky)
  (-) x y = case (fromNeg x, fromNeg y) of
              (Just x', Just y') -> y' - x' -- (-x) - (-y) == y - x
              (Nothing, Just y') -> x + y' -- (x) - (-y) == x + y
              (Just x', Nothing) -> negate (x' + y) -- (-x) - (y) == -(x+y)
              _ -> ENum $ Sub x y

  abs (EConst x) = EConst (abs x)
  abs (ENum (FromInteger k)) = ENum (FromInteger (abs k))
  abs (EFractional (FromRational r)) = EFractional (FromRational (abs r))
  abs x = case fromNeg x of Nothing -> ENum (Abs x)
                            Just x' -> abs x'

  negate (EConst x) = EConst (negate x)
  negate (ENum (Sub x y)) = y - x
  negate (ENum (FromInteger k)) = ENum (FromInteger (negate k))
  negate (EFractional (FromRational r)) = EFractional (FromRational (negate r))
  negate (ENum (Negate x)) = x
  negate x = ENum $ Negate x

  signum (EConst x) = EConst (signum x)
  signum (ENum (FromInteger k)) = ENum (FromInteger (signum k))
  signum (EFractional (FromRational r)) = EFractional (FromRational (signum r))
  signum x = ENum $ Signum x

  fromInteger = ENum . FromInteger

instance (Eq a, Fractional a) => Fractional (Expr a) where
  (/) x y
    | isVal 0 y = error "Fractional (Expr a) divide by zero"
    | isVal 0 x = 0
    | isVal 1 y = x
    | x == y = 1
  (/) (EConst x) (EConst y) = EConst (x/y)
  (/) (ENum (FromInteger kx)) (ENum (FromInteger ky)) = EFractional $ FromRational (kx % ky)
  (/) (EFractional (FromRational rx)) (EFractional (FromRational ry)) = EFractional $ FromRational (rx / ry)
  (/) (EConst x) (ENum (FromInteger ky)) = EConst $ x / fromInteger ky
  (/) (ENum (FromInteger kx)) (EConst y) = EConst $ fromInteger kx / y
  (/) (EConst x) (EFractional (FromRational ry)) = EConst $ x / fromRational ry
  (/) (EFractional (FromRational rx)) (EConst y) = EConst $ fromRational rx / y
  (/) (ENum (FromInteger kx)) (EFractional (FromRational ry)) = EFractional $ FromRational (fromInteger kx / ry)
  (/) (EFractional (FromRational rx)) (ENum (FromInteger ky)) = EFractional $ FromRational (rx / fromInteger ky)
  (/) x y = case (fromNeg x, fromNeg y) of
              (Just x', Just y') -> x' / y'
              (Nothing, Just y') -> negate (x  / y')
              (Just x', Nothing) -> negate (x' / y )
              _ -> EFractional $ Div x y

  fromRational = EFractional . FromRational

instance (Eq a, Floating a) => Floating (Expr a) where
  pi          = EConst pi
  x ** 1      = x
  0 ** 0      = error "Expr: 0 ** 0 indeterminate"
  _ ** 0      = 1
  x ** y      = EFloating $ Pow x y
  logBase x y = EFloating $ LogBase x y
  exp         = applyFloatingUn (  exp,   Exp)
  log         = applyFloatingUn (  log,   Log)
  sin         = applyFloatingUn (  sin,   Sin)
  cos         = applyFloatingUn (  cos,   Cos)
  tan         = applyFloatingUn (  tan,   Tan)
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

deriving instance (Show a, Show b) => Show (GExpr a b)
instance Functor (GExpr a) where
  fmap _ (GSym s) = GSym s
  fmap _ (GConst c) = GConst c
  fmap f (GNum nums) = GNum (fmap f nums)
  fmap f (GFractional fracs) = GFractional (fmap f fracs)
  fmap f (GFloating floatings) = GFloating (fmap f floatings)

instance F.Foldable (GExpr a) where
  foldMap _ (GSym _) = mempty
  foldMap _ (GConst _) = mempty
  foldMap f (GNum nums) = F.foldMap f nums
  foldMap f (GFractional fracs) = F.foldMap f fracs
  foldMap f (GFloating floatings) = F.foldMap f floatings

  foldr _ z (GSym _) = z
  foldr _ z (GConst _) = z
  foldr f z (GNum nums) = F.foldr f z nums
  foldr f z (GFractional fracs) = F.foldr f z fracs
  foldr f z (GFloating floatings) = F.foldr f z floatings

instance T.Traversable (GExpr a) where
  traverse _ (GSym s) = pure (GSym s)
  traverse _ (GConst c) = pure (GConst c)
  traverse f (GNum nums) = GNum <$> T.traverse f nums
  traverse f (GFractional fracs) = GFractional <$> T.traverse f fracs
  traverse f (GFloating floatings) = GFloating <$> T.traverse f floatings

deriving instance (Eq a, Eq b) => Eq (GExpr a b)

instance (Hashable a, Hashable b) => Hashable (GExpr a b) where
  hashWithSalt s (GSym name)     = s `hashWithSalt` "GSym"        `hashWithSalt` name
  hashWithSalt s (GConst x)      = s `hashWithSalt` "GConst"      `hashWithSalt` x
  hashWithSalt s (GNum x)        = s `hashWithSalt` "GNum"        `hashWithSalt` x
  hashWithSalt s (GFractional x) = s `hashWithSalt` "GFractional" `hashWithSalt` x
  hashWithSalt s (GFloating x)   = s `hashWithSalt` "GFloating"   `hashWithSalt` x

substitute :: (Ord a, Hashable a, Show a) => Expr a -> [(Expr a, Expr a)] -> Expr a
substitute expr subList
  | nonSymInputs /= [] = error $ "substitute got non-ESym input: " ++ show nonSymInputs
  | otherwise = subs expr
  where
    isSym (ESym _) = True
    isSym _ = False
    nonSymInputs = filter (not . isSym . fst) subList
    lookup' e = let hm = HM.fromList subList in
      HM.lookupDefault e e hm

    subs e@(ESym _) = lookup' e
    subs e@(EConst _) = e
    subs e@(ENum (FromInteger _)) = e
    subs e@(EFractional (FromRational _)) = e
    subs (ENum (Mul x y)) = subs x * subs y
    subs (ENum (Add x y)) = subs x + subs y
    subs (ENum (Sub x y)) = subs x - subs y
    subs (ENum (Negate x)) = negate (subs x)
    subs (ENum (Abs x))    = abs (subs x)
    subs (ENum (Signum x)) = signum (subs x)

    subs (EFractional (Div x y)) = subs x / subs y

    subs (EFloating (Pow x y))     = subs x ** subs y
    subs (EFloating (LogBase x y)) = logBase (subs x) (subs y)
    subs (EFloating (Exp   x))     = exp   (subs x)
    subs (EFloating (Log   x))     = log   (subs x)
    subs (EFloating (Sin   x))     = sin   (subs x)
    subs (EFloating (Cos   x))     = cos   (subs x)
    subs (EFloating (Tan   x))     = tan   (subs x)
    subs (EFloating (ASin  x))     = asin  (subs x)
    subs (EFloating (ATan  x))     = atan  (subs x)
    subs (EFloating (ACos  x))     = acos  (subs x)
    subs (EFloating (Sinh  x))     = sinh  (subs x)
    subs (EFloating (Cosh  x))     = cosh  (subs x)
    subs (EFloating (Tanh  x))     = tanh  (subs x)
    subs (EFloating (ASinh x))     = asinh (subs x)
    subs (EFloating (ATanh x))     = atanh (subs x)
    subs (EFloating (ACosh x))     = acosh (subs x)

-- | this substitute is sketchy because it doesn't perform simplifications
--   that are often assumed to be done
sketchySubstitute :: (Eq a, Hashable a, Show a) => Expr a -> [(Expr a, Expr a)] -> Expr a
sketchySubstitute expr subList
  | nonSymInputs /= [] = error $ "substitute got non-ESym input: " ++ show nonSymInputs
  | otherwise = subs expr
  where
    isSym (ESym _) = True
    isSym _ = False
    nonSymInputs = filter (not . isSym . fst) subList
    lookup' e = let hm = HM.fromList subList in
      HM.lookupDefault e e hm

    subs e@(ESym _) = lookup' e
    subs e@(EConst _)  = e
    subs e@(ENum (FromInteger _)) = e
    subs e@(EFractional (FromRational _)) = e
    subs (ENum (Mul x y)) = ENum (Mul (subs x) (subs y))
    subs (ENum (Add x y)) = ENum (Add (subs x) (subs y))
    subs (ENum (Sub x y)) = ENum (Sub (subs x) (subs y))
    subs (ENum (Negate x)) = ENum (Negate (subs x))
    subs (ENum (Abs x)) = ENum (Negate (subs x))
    subs (ENum (Signum x)) = ENum (Signum (subs x))

    subs (EFractional (Div x y)) = EFractional (Div (subs x) (subs y))

    subs (EFloating (Pow x y))     = EFloating (Pow (subs x) (subs y))
    subs (EFloating (LogBase x y)) = EFloating (LogBase (subs x) (subs y))
    subs (EFloating (Exp   x))     = EFloating (Exp   (subs x))
    subs (EFloating (Log   x))     = EFloating (Log   (subs x))
    subs (EFloating (Sin   x))     = EFloating (Sin   (subs x))
    subs (EFloating (Cos   x))     = EFloating (Cos   (subs x))
    subs (EFloating (Tan   x))     = EFloating (Tan   (subs x))
    subs (EFloating (ASin  x))     = EFloating (ASin  (subs x))
    subs (EFloating (ATan  x))     = EFloating (ATan  (subs x))
    subs (EFloating (ACos  x))     = EFloating (ACos  (subs x))
    subs (EFloating (Sinh  x))     = EFloating (Sinh  (subs x))
    subs (EFloating (Cosh  x))     = EFloating (Cosh  (subs x))
    subs (EFloating (Tanh  x))     = EFloating (Tanh  (subs x))
    subs (EFloating (ASinh x))     = EFloating (ASinh (subs x))
    subs (EFloating (ATanh x))     = EFloating (ATanh (subs x))
    subs (EFloating (ACosh x))     = EFloating (ACosh (subs x))


---------------------------------- utility functions -------------------------------
-- | foldr over the constants and symbols
foldExpr :: (Expr a -> b -> b) -> b -> Expr a -> b
foldExpr f acc e@(ESym _)                       = f e acc
foldExpr f acc e@(EConst _)                     = f e acc
foldExpr f acc e@(EFractional (FromRational _)) = f e acc
foldExpr f acc e@(ENum (FromInteger _))         = f e acc
foldExpr f acc (ENum (Mul x y))          = foldExpr f (foldExpr f acc y) x
foldExpr f acc (ENum (Add x y))          = foldExpr f (foldExpr f acc y) x
foldExpr f acc (ENum (Sub x y))          = foldExpr f (foldExpr f acc y) x
foldExpr f acc (ENum (Negate x))         = foldExpr f acc x
foldExpr f acc (ENum (Abs x))            = foldExpr f acc x
foldExpr f acc (ENum (Signum x))         = foldExpr f acc x
foldExpr f acc (EFractional (Div x y))   = foldExpr f (foldExpr f acc y) x
foldExpr f acc (EFloating (Pow x y))     = foldExpr f (foldExpr f acc y) x
foldExpr f acc (EFloating (LogBase x y)) = foldExpr f (foldExpr f acc y) x
foldExpr f acc (EFloating (Exp x))       = foldExpr f acc x
foldExpr f acc (EFloating (Log x))       = foldExpr f acc x
foldExpr f acc (EFloating (Sin x))       = foldExpr f acc x
foldExpr f acc (EFloating (Cos x))       = foldExpr f acc x
foldExpr f acc (EFloating (Tan x))       = foldExpr f acc x
foldExpr f acc (EFloating (ASin x))      = foldExpr f acc x
foldExpr f acc (EFloating (ATan x))      = foldExpr f acc x
foldExpr f acc (EFloating (ACos x))      = foldExpr f acc x
foldExpr f acc (EFloating (Sinh x))      = foldExpr f acc x
foldExpr f acc (EFloating (Cosh x))      = foldExpr f acc x
foldExpr f acc (EFloating (Tanh x))      = foldExpr f acc x
foldExpr f acc (EFloating (ASinh x))     = foldExpr f acc x
foldExpr f acc (EFloating (ATanh x))     = foldExpr f acc x
foldExpr f acc (EFloating (ACosh x))     = foldExpr f acc x

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
{-# INLINE isVal #-}

-- | if the expression is a constant, a fromInteger, or a fromRational, return the constant part
--   otherwise return nothing
getConst :: Expr a -> Maybe a
getConst (EConst x) = Just x
getConst (ENum (FromInteger k)) = Just (fromInteger k)
getConst (EFractional (FromRational r)) = Just (fromRational r)
getConst _ = Nothing

-- | Separate nonlinear and linear parts of an expression
--   @extractLinearPart (fNonLin(x)+a*x) x == (fNonLin(x), a)
extractLinearPart :: (Num a, Eq a, Show a) => Expr a -> Expr a -> (Expr a, a)
extractLinearPart e@(EConst _) _ = (e,0)
extractLinearPart e@(ENum (FromInteger _)) _ = (e,0)
extractLinearPart e@(EFractional (FromRational _)) _ = (e,0)
extractLinearPart e@(ESym _) arg
  | e == arg = (0,1)
  | otherwise = (e,0)
extractLinearPart (ENum (Add x y)) arg = (xNonlin+yNonlin, xLin+yLin)
  where
    (xNonlin,xLin) = extractLinearPart x arg
    (yNonlin,yLin) = extractLinearPart y arg
extractLinearPart (ENum (Sub x y)) arg = (xNonlin-yNonlin, xLin-yLin)
  where
    (xNonlin,xLin) = extractLinearPart x arg
    (yNonlin,yLin) = extractLinearPart y arg
extractLinearPart (ENum (Negate x)) arg = (-xNonlin, -xLin)
  where
    (xNonlin,xLin) = extractLinearPart x arg
extractLinearPart e@(ENum (Mul x y)) arg = case (getConst x, getConst y) of
  (Nothing,Nothing) -> (e,0)
  (Just cx, Nothing) -> let (yNl,yL) = extractLinearPart y arg in (EConst cx * yNl,cx*yL)
  (Nothing, Just cy) -> let (xNl,xL) = extractLinearPart x arg in (xNl * EConst cy,xL*cy)
  _ -> error $ "extractLinearPart got ENum (Mul x y) where x and y are both constants\n"++
       "x: " ++ show x ++ "\ny: " ++ show y
extractLinearPart e@(EFractional (Div x y)) arg = case getConst y of
  Nothing -> (e,0)
  Just cy -> let (xNl,xL) = extractLinearPart x arg in (xNl/EConst cy,xL/cy)
extractLinearPart e@(ENum (Abs _))    _ = (e,0)
extractLinearPart e@(ENum (Signum _)) _ = (e,0)
extractLinearPart e@(EFloating (Pow _ _)) _ = (e,0)
extractLinearPart e@(EFloating (LogBase _ _)) _ = (e,0)
extractLinearPart e@(EFloating (Exp _))   _ = (e,0)
extractLinearPart e@(EFloating (Log _))   _ = (e,0)
extractLinearPart e@(EFloating (Sin _))   _ = (e,0)
extractLinearPart e@(EFloating (Cos _))   _ = (e,0)
extractLinearPart e@(EFloating (Tan _))   _ = (e,0)
extractLinearPart e@(EFloating (ASin _))  _ = (e,0)
extractLinearPart e@(EFloating (ATan _))  _ = (e,0)
extractLinearPart e@(EFloating (ACos _))  _ = (e,0)
extractLinearPart e@(EFloating (Sinh _))  _ = (e,0)
extractLinearPart e@(EFloating (Cosh _))  _ = (e,0)
extractLinearPart e@(EFloating (Tanh _))  _ = (e,0)
extractLinearPart e@(EFloating (ASinh _)) _ = (e,0)
extractLinearPart e@(EFloating (ATanh _)) _ = (e,0)
extractLinearPart e@(EFloating (ACosh _)) _ = (e,0)


------------------------------- arbitrary instances --------------------------
--instance Arbitrary a => Arbitrary (Nums a) where
--  arbitrary = liftM ENums arbitrary
--data Nums a = Mul a a
--            | Add a a
--            | Sub a a
--            | Negate a
--            | Abs a
--            | Signum a
--            | FromInteger Integer

--instance Arbitrary a => Arbitrary (Expr a) where
--   arbitrary = oneof [arbConst, arbUnary, arbBinary]
--
--arbConst :: Arbitrary a => Gen (Expr a)
--arbConst = liftM EConst arbitrary
--
--arbUnary :: Arbitrary (Expr a) => Gen (Expr a)
--arbUnary = liftM2 EUnary arbitrary arbitrary
--
--arbBinary :: Arbitrary (Expr a) => Gen (Expr a)
--arbBinary = liftM3 EBinary arbitrary arbitrary arbitrary

--arbNum :: (Num a, Arbitrary (Expr a)) => Gen (Expr a)
--arbNum = liftM ENum arbitrary -- arbitrary arbitrary
