{-# OPTIONS_GHC -Wall #-}

module Dvda.GExpr ( GExpr(..)
                  , getChildren
                  , gdim
                  ) where

import Data.IntMap ( Key )
import Data.Hashable ( Hashable, hash, combine )
import Data.GraphViz ( Labellable, toLabelValue )
import qualified Data.Vector.Unboxed as V

import Dvda.BinUn ( BinOp, UnOp, isCommutative )
import Dvda.HomoDim ( HomoDim(..) )
import Dvda.Dot ( dotDims )

simplifyCommutativeOps :: Bool
simplifyCommutativeOps = True

data GExpr a = GBinary HomoDim BinOp Key Key
             | GUnary HomoDim UnOp Key
             | GSym HomoDim String
             | GSingleton HomoDim a
             | GScale HomoDim Key Key
             | GDot HomoDim HomoDim Key Key
             | GConst HomoDim (V.Vector a) deriving Show

instance (Eq a, V.Unbox a) => Eq (GExpr a) where 
  (==) (GBinary shx opx x0 x1) (GBinary shy opy y0 y1) = and [opx == opy, shx == shy, commutativeConditions]
    where
      commutativeConditions = if simplifyCommutativeOps && isCommutative opx
                              then (and [x0 == y0, x1 == y1]) || (and [x0 == y1, x1 == y0])
                              else (and [x0 == y0, x1 == y1])
  (==) (GUnary shx opx x) (GUnary shy opy y) = and [shx == shy, opx == opy, x == y]
  (==) (GSym shx namex) (GSym shy namey) = and [shx == shy, namex == namey]
  (==) (GSingleton shx x) (GSingleton shy y) = and [shx == shy, x == y]
  (==) (GScale shx x0 x1) (GScale shy y0 y1) = and [shx == shy, x0 == y0, x1 == y1]
  (==) (GDot shx0 shx1 x0 x1) (GDot shy0 shy1 y0 y1) = and [shx0 == shy0, shx1==shy1, x0 == y0, x1 == y1]
  (==) (GConst shx x) (GConst shy y) = and [shx == shy, x == y]
  (==) _ _ = False

gdim :: GExpr a -> HomoDim
gdim (GBinary sh _ _ _) = sh
gdim (GUnary sh _ _) = sh
gdim (GSym sh _) = sh
gdim (GSingleton sh _) = sh
gdim (GScale sh _ _) = sh
gdim (GDot shx shy _ _) = dotDims shx shy
gdim (GConst sh _) = sh

instance (V.Unbox a, Hashable a) => Hashable (GExpr a) where
  hash (GBinary _ op k1 k2) = 24 `combine` hash op `combine` hk1 `combine` hk2
    where
      -- if the binary operator is commutative then always put the lesser hash first
      -- so that e.g. x*y and y*x are not computed twice
      (hk1, hk2)
        | simplifyCommutativeOps && isCommutative op && hk2' < hk1' = (hk2', hk1')
        | otherwise = (hk1', hk2')
      hk1' = hash k1
      hk2' = hash k2
  hash (GUnary _ op k)    = 25 `combine` hash op `combine` hash k
  hash (GSym sh name)     = 26 `combine` hash sh `combine` hash name
  hash (GSingleton sh x)  = 27 `combine` hash sh `combine` hash x
  hash (GScale _ k1 k2)   = 28 `combine` hash k1 `combine` hash k2
  hash (GDot _ _ k1 k2)   = 29 `combine` hash k1 `combine` hash k2
  hash (GConst sh v)      = V.foldl (\acc x -> acc `combine` hash x) (30 `combine` hash sh) v


instance Show a => Labellable (GExpr a) where
  toLabelValue (GBinary _ op _ _) = toLabelValue $ show op
  toLabelValue (GUnary _ op _)    = toLabelValue $ show op
  toLabelValue (GSym (HomoDim []) name) = toLabelValue name
  toLabelValue (GSym (HomoDim sh) name) = toLabelValue $ name ++ "{" ++ (tail . init . show . reverse) sh ++ "}"
  toLabelValue (GSingleton _ x)   = toLabelValue $ show x
  toLabelValue (GScale {})        = toLabelValue "scale"
  toLabelValue (GDot {})          = toLabelValue "dot"
  toLabelValue (GConst {})        = toLabelValue "const"


getChildren :: GExpr a -> [Int]
getChildren (GBinary _ _ k1 k2) = [k1,k2]
getChildren (GUnary _ _ k) = [k]
getChildren (GSym _ _ ) = []
getChildren (GSingleton _ _) = []
getChildren (GScale _ k1 k2) = [k1,k2]
getChildren (GDot _ _ k1 k2) = [k1,k2]
getChildren (GConst _ _) = []
