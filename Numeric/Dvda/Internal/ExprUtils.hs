{- |
   Module      : Numeric.Dvda.Internal.ExprUtils
   Description : Basic internal Expr api

   Provides many internal functions for `Numeric.Dvda.Expr.Expr` which should not be exposed to the user
 -}

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Internal.ExprUtils( getSyms
                                      , Children(..)
                                      , getChildren
                                      , showNode
                                      , showType
                                      , showDim
                                      , symName
                                      , toCCode
                                      ) where

import Numeric.Dvda.Internal.Expr
import Numeric.Dvda.Internal.Binary
import Numeric.Dvda.Internal.Unary
import Numeric.Dvda.Internal.Tensor
import Numeric.Dvda.Internal.GNode

showNode :: Show a => Expr a -> String
showNode (EScalar x) = tShowNode x
showNode (EVector x) = tShowNode x
showNode (EMatrix x) = tShowNode x

showType :: Show a => Expr a -> String
showType (EScalar (TNum _ _)) = "N"
showType (EScalar (TInt _ _)) = "I"
showType (EVector (TNum _ _)) = "N"
showType (EVector (TInt _ _)) = "I"
showType (EMatrix (TNum _ _)) = "N"
showType (EMatrix (TInt _ _)) = "I"
showType _ = ""

showDim :: Show a => Expr a -> String
showDim (EScalar _) = ""
showDim (EVector x) = show $ tDim x
showDim (EMatrix x) = show $ tDim x

-- | convert Expr gnode to c code string
toCCode :: (Eq a, Show a) => GNode (Expr a) -> String
toCCode (GSource i (EScalar x)) = tToCCode $ GSource i x
toCCode (GSource i (EVector x)) = tToCCode $ GSource i x
toCCode (GSource i (EMatrix x)) = tToCCode $ GSource i x

toCCode (GOutput i (EScalar x) cx ox) = tToCCode $ GOutput i x cx ox
toCCode (GOutput i (EVector x) cx ox) = tToCCode $ GOutput i x cx ox
toCCode (GOutput i (EMatrix x) cx ox) = tToCCode $ GOutput i x cx ox

toCCode (GUnary i (EScalar x) ix) = tToCCode $ GUnary i x ix
toCCode (GUnary i (EVector x) ix) = tToCCode $ GUnary i x ix
toCCode (GUnary i (EMatrix x) ix) = tToCCode $ GUnary i x ix

toCCode (GBinary i (EScalar x) (ix, iy)) = tToCCode $ GBinary i x (ix, iy)
toCCode (GBinary i (EVector x) (ix, iy)) = tToCCode $ GBinary i x (ix, iy)
toCCode (GBinary i (EMatrix x) (ix, iy)) = tToCCode $ GBinary i x (ix, iy)

toCCode (GBroadcast i (EScalar x) ix) = tToCCode $ GBroadcast i x ix
toCCode (GBroadcast i (EVector x) ix) = tToCCode $ GBroadcast i x ix
toCCode (GBroadcast i (EMatrix x) ix) = tToCCode $ GBroadcast i x ix

-- | get name if variable is symbolic
symName :: Expr a -> Maybe String
symName (EScalar (TSym _ n)) = Just n
symName (EVector (TSym _ n)) = Just n
symName (EMatrix (TSym _ n)) = Just n
symName _ = Nothing


-- | call the correct Expr constructor based on the dimensions of a tensor
tensorToExpr :: Tensor a -> Expr a
tensorToExpr x
  | length (tDim x) == 0 = EScalar x
  | length (tDim x) == 1 = EVector x
  | length (tDim x) == 2 = EMatrix x
  | otherwise            = error $ "can't construct Expr from tensor with dimensions" ++ show (tDim x)


-- | get all the symbolic variables in an expression
getSyms :: Expr a -> [Expr a]
getSyms (EScalar x) = map tensorToExpr $ tGetSyms x
getSyms (EVector x) = map tensorToExpr $ tGetSyms x
getSyms (EMatrix x) = map tensorToExpr $ tGetSyms x


-- | data type containing children of an expression
data Children a = CSource
                | CBroadcast a
                | CUnary a
                | CBinary a a

-- | get all the children of an Expr
getChildren :: Expr a -> Children (Expr a)
getChildren (EScalar (TUnary (Unary _ x))) = CUnary (EScalar x)
getChildren (EScalar (TBinary (Binary _ x y))) = CBinary (EScalar x) (EScalar y)
getChildren (EScalar (TBroadcast _ _)) = error "api fail in getChildren"
getChildren (EScalar _) = CSource

getChildren (EVector (TUnary (Unary _ x))) = CUnary (EVector x)
getChildren (EVector (TBinary (Binary _ x y))) = CBinary (EVector x) (EVector y)
getChildren (EVector (TBroadcast _ x)) = CBroadcast (EScalar x)
getChildren (EVector _) = CSource

getChildren (EMatrix (TUnary (Unary _ x))) = CUnary (EMatrix x)
getChildren (EMatrix (TBinary (Binary _ x y))) = CBinary (EMatrix x) (EMatrix y)
getChildren (EMatrix (TBroadcast _ x)) = CBroadcast (EScalar x)
getChildren (EMatrix _) = CSource


