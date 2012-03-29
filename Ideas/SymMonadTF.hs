{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies, MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}

module Ideas.SymMonad( node
                     , sym
                     ) where
--                     , symVec
--                     , symMat
--                     , node
--                     , FunGraph(..)
----                     , woo
----                     , run
----                     , makeFun
--                     ) where

import Control.Monad.State
import Data.Functor.Identity
import Data.Array.Repa(DIM0, Z(..), Shape, listOfShape)
import Data.Vector(Vector)
import qualified Data.Vector as V

import Ideas.BinUn

type Key = Int

class Mul a b where
  type MulT a b
  mul :: a -> b -> MulT a b
--  mul x y = Binary x y

data TensorFromIntegral a = TensorFromIntegral a
data GraphRef d a = GraphRef d Key
data ConstTensor d a = ConstTensor d (Vector a)
data SymTensor d a = SymTensor d String
data Binary d a b = Binary d BinOp a b

class HasDim a where
  type DimT a
  getDim :: a -> DimT a
instance Shape d => HasDim (GraphRef d a) where
  type DimT (GraphRef d a) = d
  getDim (GraphRef d _) = d
instance Shape d => HasDim (ConstTensor d a) where
  type DimT (ConstTensor d a) = d
  getDim (ConstTensor d _) = d
instance Shape d => HasDim (SymTensor d a) where
  type DimT (SymTensor d a) = d
  getDim (SymTensor d _) = d
instance Shape d => HasDim (Binary d a b) where
  type DimT (Binary d a b) = d
  getDim (Binary d _ _ _) = d

---------------------- specialized reductions -------------------------
instance Num a => Mul (TensorFromIntegral a) (TensorFromIntegral a) where
  type MulT (TensorFromIntegral a) (TensorFromIntegral a) = TensorFromIntegral a
  mul (TensorFromIntegral x) (TensorFromIntegral y) = TensorFromIntegral (x*y)

instance (Num a, Eq d) => Mul (ConstTensor d a) (ConstTensor d a) where
  type MulT (ConstTensor d a) (ConstTensor d a) = ConstTensor d a
  mul (ConstTensor dx xs) (ConstTensor dy ys) 
    | dx == dy  = ConstTensor dx (V.zipWith (*) xs ys)
    | otherwise = error "dimension mismatch ya goon"

instance Num a => Mul (TensorFromIntegral a) (ConstTensor d a) where
  type MulT (TensorFromIntegral a) (ConstTensor d a) = ConstTensor d a
  mul (TensorFromIntegral x) (ConstTensor d ys) = ConstTensor d (V.map (x *) ys)
instance Num a => Mul (ConstTensor d a) (TensorFromIntegral a) where
  type MulT (ConstTensor d a) (TensorFromIntegral a) = ConstTensor d a
  mul (ConstTensor d xs) (TensorFromIntegral y) = ConstTensor d (V.map (* y) xs)

-------------------------- just make binary ----------------------------
--type family EasyBinary a b
--type instance EasyBinary 
--instance (EasyBinary a b) => Mul a b where
--  type MulT a b = Binary a b
--  mul a b = Binary a b

--instance Mul a b where
--  type MulT a b = Binary d a b
--  mul x y = Binary d Mul x y

instance Mul (TensorFromIntegral a) (SymTensor d a) where
  type MulT (TensorFromIntegral a) (SymTensor d a) = Binary d (TensorFromIntegral a) (SymTensor d a)
  mul x y@(SymTensor d _) = Binary d Mul x y
instance Mul (SymTensor d a) (TensorFromIntegral a) where
  type MulT (SymTensor d a) (TensorFromIntegral a) = Binary d (SymTensor d a) (TensorFromIntegral a)
  mul x@(SymTensor d _) y = Binary d Mul x y

instance Eq d => Mul (GraphRef d a) (GraphRef d a) where
  type MulT (GraphRef d a) (GraphRef d a) = Binary d (GraphRef d a) (GraphRef d a)
  mul x@(GraphRef dx _) y@(GraphRef dy _) 
    | dx == dy  = Binary dx Mul x y
    | otherwise = error "dimension mismatch ya goon"


instance Eq d => Mul (SymTensor d a) (GraphRef d a) where
  type MulT (SymTensor d a) (GraphRef d a) = Binary d (SymTensor d a) (GraphRef d a)
  mul x@(SymTensor dx _) y@(GraphRef dy _) 
    | dx == dy  = Binary dx Mul x y
    | otherwise = error "dimension mismatch ya goon"

instance Eq d => Mul (GraphRef d a) (SymTensor d a) where
  type MulT (GraphRef d a) (SymTensor d a) = Binary d (GraphRef d a) (SymTensor d a)
  mul x@(GraphRef dx _) y@(SymTensor dy _) 
    | dx == dy  = Binary dx Mul x y
    | otherwise = error "dimension mismatch ya goon"

symE :: String -> SymTensor DIM0 a
symE = SymTensor Z

----------------------------------------------------------
class Shape d => Graphable a d | a -> d where
  node' :: a -> State (FunGraph a) (Key, d)
  
instance Shape d => Graphable (SymTensor d a) d where
  node' (SymTensor d' name) = do r <- insert $ GSym (listOfShape d') name
                                 return (r, d')

node :: Graphable a d => a -> StateT (FunGraph a) Identity (GraphRef d Key)
node expr = do (k,d) <- node' expr
               return (GraphRef d k)

data GExpr a = GBinary BinOp Key Key
--             | GUnary UnOp Key
             | GSym [Int] String
--             | GScale [Int] Key Key
             | GConst [Int] (Vector a) deriving (Show, Eq)

insert :: MonadState (FunGraph a) m => GExpr a -> m Int
insert gexpr = do
  FunGraph xs ins outs <- get
  let k = length xs
  put (FunGraph (xs ++ [(k,gexpr)]) ins outs)
  return k

data FunGraph a = FunGraph [(Key, GExpr a)] [Key] [Key] deriving (Show, Eq)

sym :: String -> StateT (FunGraph (SymTensor DIM0 a)) Identity (GraphRef DIM0 Key)
sym = node . symE

--symVec :: Int -> String -> State (FunGraph a) (Expr DIM1 a)
--symVec d = node . (vsymE d)
--
--symMat :: (Int,Int) -> String -> State (FunGraph a) (Expr DIM2 a)
--symMat (r,c) = node . (msymE (r,c))


-- | take all sub expressions of an Expr and turn them into nodes
--   return an Expr that is just a ref
--node :: Shape d => Expr d a -> State (FunGraph a) (Expr d a)
--node expr = liftM ERef (node' expr)
--  where
----    node' :: Shape d => Expr d a -> State (FunGraph a) Key
--    node' (ESym d name) = insert $ GSym (listOfShape d) name
--    node' (ERef k) = return k
--    node' (EBinary op x y) = do
--      x' <- node' x
--      y' <- node' y
--      insert (GBinary  op x' y')
--    node' (EUnary op x) = do
--      x' <- node' x
--      insert (GUnary op x')
--    node' (EConst d x) = insert $ GConst (listOfShape d) x
--    node' (ESingleton x) = error "don't put ESingleton in graph"
----    node' (EScale x y) = do
----      x' <- node' x
----      y' <- node' y
----      insert $ GScale (listOfShape y) x' y'
--    node' (EDot x y) = do
--      x' <- node' x
--      y' <- node' y
--      insert $ GScale (listOfShape y) x' y'

--    insert gexpr = do
--      FunGraph xs ins outs <- get
--      let k = length xs
--      put (FunGraph (xs ++ [(k,gexpr)]) ins outs)
--      return k



----instance Add SZero Const
--data FOfT = FOfT Int String
--data Const = Const Double
--data Param = Param String
--data SZero = SZero
--data SOne = SOne

--class Tensor d a where
--  type Expr d a
--  dim :: Expr d a -> [Int]
--  node' :: Shape d => Expr d a -> State (FunGraph a) (Expr d a)

--instance Symbolic Binary d a where
--  node' (ESym d name) = insert $ GSym (listOfShape d) name
--  node' (ERef k) = return k
--  node' (EBinary op x y) = do
--    x' <- node' x
--    y' <- node' y
--    insert (GBinary  op x' y')
--
--insert gexpr = do
--  FunGraph xs ins outs <- get
--  let k = length xs
--  put (FunGraph (xs ++ [(k,gexpr)]) ins outs)
--  return k

--data Expr a = EBinary BinOp (Expr a) (Expr a)
--            | EUnary UnOp (Expr a)
----              EScale :: Expr DIM0 -> Expr a -> Expr a
----              EDot :: (Dot b c ~ a) => Expr b -> Expr c -> Expr a
--            | ESym String
--            | EConst a
--            | ERef Key deriving (Show, Eq)

--instance Num a => Num (Expr a) where
--  (*) = EBinary Mul
--  (+) = EBinary Add
--  (-) = EBinary Sub
--  abs = EUnary Abs
--  signum = EUnary Signum
--  fromInteger = EConst . fromInteger

--data GExpr a = GBinary BinOp Key Key
--             | GUnary UnOp Key
--             | GSym [Int] String
--             | GScale [Int] Key Key
--             | GConst [Int] (Vector a) deriving (Show, Eq)
--                                 
--data FunGraph a where 
--  FunGraph :: [(Key, GExpr a)] -> [Key] -> [Key] -> FunGraph a
--  deriving (Show, Eq)

--sym :: String -> State (FunGraph a) (Expr DIM0 a)
--sym = node . symE
--
--symVec :: Int -> String -> State (FunGraph a) (Expr DIM1 a)
--symVec d = node . (vsymE d)
--
--symMat :: (Int,Int) -> String -> State (FunGraph a) (Expr DIM2 a)
--symMat (r,c) = node . (msymE (r,c))


---- | take all sub expressions of an Expr and turn them into nodes
----   return an Expr that is just a ref
--node :: Shape d => Expr d a -> State (FunGraph a) (Expr d a)
--node expr = liftM ERef (node' expr)
--  where
----    node' :: Shape d => Expr d a -> State (FunGraph a) Key
--    node' (ESym d name) = insert $ GSym (listOfShape d) name
--    node' (ERef k) = return k
--    node' (EBinary op x y) = do
--      x' <- node' x
--      y' <- node' y
--      insert (GBinary  op x' y')
--    node' (EUnary op x) = do
--      x' <- node' x
--      insert (GUnary op x')
--    node' (EConst d x) = insert $ GConst (listOfShape d) x
--    node' (ESingleton x) = error "don't put ESingleton in graph"
----    node' (EScale x y) = do
----      x' <- node' x
----      y' <- node' y
----      insert $ GScale (listOfShape y) x' y'
--    node' (EDot x y) = do
--      x' <- node' x
--      y' <- node' y
--      insert $ GScale (listOfShape y) x' y'
--
--    insert gexpr = do
--      FunGraph xs ins outs <- get
--      let k = length xs
--      put (FunGraph (xs ++ [(k,gexpr)]) ins outs)
--      return k

--makeFun :: State (FunGraph a) b -> (b, FunGraph a)
--makeFun f = runState f (FunGraph [] [] [])
--
--woo :: Num a => StateT (FunGraph a) Identity [Expr d a]
--woo = do
--  x <- sym "x"
--  let y = mul x x
--  z <- node (mul x y)
--  return [mul z y]
--
--run :: Num b => ([Expr a b], FunGraph b)
--run = makeFun woo
