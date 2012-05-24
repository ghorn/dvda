{-# OPTIONS_GHC -Wall #-}

module Dvda.GExpr ( GExpr(..)
                  , gdim
                  , getChildren
                  ) where

import Data.IntMap ( Key )
import qualified Data.Vector.Unboxed as V
import Data.Hashable ( Hashable, hash, combine )
import Data.GraphViz ( Labellable, toLabelValue )

import Dvda.BinUn ( BinOp, UnOp, isCommutative )
import Dvda.HomoDim

data GExpr a = GBinary HomoDim BinOp Key Key
             | GUnary HomoDim UnOp Key
             | GSym HomoDim String
             | GSingleton HomoDim a
             | GScale HomoDim Key Key
             | GDot HomoDim Key Key
             | GConst HomoDim (V.Vector a) deriving (Show, Eq)

instance (V.Unbox a, Hashable a) => Hashable (GExpr a) where
  -- if the binary operator is commutative then always put the lesser hash first
  -- so that e.g. x*y and y*x are not computed twice
  hash (GBinary _ op k1 k2) = 24 `combine` hash op `combine` hk1' `combine` hk2'
    where
      hk1 = hash k1
      hk2 = hash k2
      (hk1', hk2')
        | isCommutative op && hk2 < hk1 = (hk2, hk1)
        | otherwise = (hk1, hk2)
  --  hash (GBinary _ op k1 k2) = 24 `combine` hash op `combine` hash k1 `combine` hash k2
  hash (GUnary _ op k)    = 25 `combine` hash op `combine` hash k
  hash (GSym sh name)     = 26 `combine` hash sh `combine` hash name
  hash (GSingleton sh x)  = 27 `combine` hash sh `combine` hash x
  hash (GScale _ k1 k2)   = 28 `combine` hash k1 `combine` hash k2
  hash (GDot _ k1 k2)     = 29 `combine` hash k1 `combine` hash k2
  hash (GConst sh v)      = V.foldl (\acc x -> acc `combine` hash x) (30 `combine` hash sh) v


instance Show a => Labellable (GExpr a) where
  toLabelValue (GBinary _ op _ _) = toLabelValue $ show op
  toLabelValue (GUnary _ op _)    = toLabelValue $ show op
  toLabelValue (GSym (HomoDim []) name) = toLabelValue name
  toLabelValue (GSym (HomoDim sh) name) = toLabelValue $ name ++ "{" ++ (tail . init . show . reverse) sh ++ "}"
  toLabelValue (GSingleton _ x)   = toLabelValue $ show x
  toLabelValue (GScale _ _ _)     = toLabelValue "scale"
  toLabelValue (GDot _ _ _)       = toLabelValue "dot"
  toLabelValue (GConst _ _)       = toLabelValue "const"


gdim :: GExpr a -> HomoDim
gdim (GBinary sh _ _ _) = sh
gdim (GUnary sh _ _) = sh
gdim (GSym sh _) = sh
gdim (GSingleton sh _) = sh
gdim (GScale sh _ _) = sh
gdim (GDot sh _ _) = sh
gdim (GConst sh _) = sh

getChildren :: GExpr a -> [Int]
getChildren (GBinary _ _ k1 k2) = [k1,k2]
getChildren (GUnary _ _ k) = [k]
getChildren (GSym _ _ ) = []
getChildren (GSingleton _ _) = []
getChildren (GScale _ k1 k2) = [k1,k2]
getChildren (GDot _ k1 k2) = [k1,k2]
getChildren (GConst _ _) = []
