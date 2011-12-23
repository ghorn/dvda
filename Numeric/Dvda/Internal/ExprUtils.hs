{- |
   Module      : Numeric.Dvda.Internal.ExprUtils
   Description : Basic internal Expr api

   Provides many internal functions for `Numeric.Dvda.Expr.Expr` which should not be exposed to the user
 -}

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Internal.ExprUtils( getSyms
                                      , isSymbolic
                                      , Children(..)
                                      , getChildren
                                      , showNode
                                      , showType
                                      , showDim
                                      , symName
                                      , toCCode
                                      ) where

import Numeric.Dvda.Internal.Expr
import Numeric.Dvda.Internal.Tensor
import Numeric.Dvda.Internal.GNode
import Numeric.Dvda.Dim

showNode :: Show a => Expr a -> String
showNode (Expr x) = tShowNode x

showType :: Show a => Expr a -> String
showType (Expr (TNum _ _)) = "N"
showType (Expr (TInt _ _)) = "I"
showType _ = ""

showDim :: Show a => Expr a -> String
showDim (Expr x)
  | tDim x == D0 = ""
  | otherwise    = show $ tDim x

-- | convert Expr gnode to c code string
toCCode :: (Eq a, Show a) => GNode (Expr a) -> String
toCCode (GSource i (Expr x)) = tToCCode $ GSource i x
toCCode (GOutput i (Expr x) cx ox n) = tToCCode $ GOutput i x cx ox n
toCCode (GUnary i (Expr x) ix) = tToCCode $ GUnary i x ix
toCCode (GBinary i (Expr x) (ix, iy)) = tToCCode $ GBinary i x (ix, iy)
toCCode (GBroadcast i (Expr x) ix) = tToCCode $ GBroadcast i x ix

-- | get name if variable is symbolic
symName :: Expr a -> Maybe String
symName (Expr (TSym _ n)) = Just n
symName _ = Nothing

-- | is the expression symbolic or not
isSymbolic :: Expr a -> Bool
isSymbolic (Expr (TSym _ _)) = True
isSymbolic _ = False

-- | get all the symbolic variables in an expression
getSyms :: Expr a -> [Expr a]
getSyms (Expr x) = map Expr $ tGetSyms x

-- | data type containing children of an expression
data Children a = CSource
                | CBroadcast a
                | CUnary a
                | CBinary a a

-- | get all the children of an Expr
getChildren :: Expr a -> Children (Expr a)
getChildren (Expr (TUnary _ x)) = CUnary (Expr x)
getChildren (Expr (TBinary _ x y)) = CBinary (Expr x) (Expr y)
getChildren (Expr (TBroadcast D0 _)) = error "api fail in getChildren"
getChildren (Expr (TBroadcast _ x)) = CBroadcast (Expr x)
getChildren (Expr _) = CSource
