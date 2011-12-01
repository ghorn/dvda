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
import Numeric.Dvda.Internal.Scalar
import Numeric.Dvda.Internal.Tensor
import Numeric.Dvda.Internal.GNode

showNode :: Show a => Expr a -> String
showNode (EScalar x) = sShowNode x
showNode (EVector x) = tShowNode x
showNode (EMatrix x) = tShowNode x

showType :: Show a => Expr a -> String
showType (EScalar (SNum _)) = "N"
showType (EScalar (SInt _)) = "I"
showType _ = ""

showDim :: Show a => Expr a -> String
showDim (EScalar _) = ""
showDim (EVector x) = show $ tDim x
showDim (EMatrix x) = show $ tDim x

-- | convert Expr gnode to c code string
toCCode :: (Eq a, Show a) => GNode (Expr a) -> String
toCCode (GSource i (EScalar x)) = sToCCode $ GSource i x
toCCode (GSource i (EVector x)) = tToCCode $ GSource i x
toCCode (GSource i (EMatrix x)) = tToCCode $ GSource i x

toCCode (GUnary i (EScalar x) ix) = sToCCode $ GUnary i x ix
toCCode (GUnary i (EVector x) ix) = tToCCode $ GUnary i x ix
toCCode (GUnary i (EMatrix x) ix) = tToCCode $ GUnary i x ix

toCCode (GBinary i (EScalar x) (ix, iy)) = sToCCode $ GBinary i x (ix, iy)
toCCode (GBinary i (EVector x) (ix, iy)) = tToCCode $ GBinary i x (ix, iy)
toCCode (GBinary i (EMatrix x) (ix, iy)) = tToCCode $ GBinary i x (ix, iy)

-- | get name if variable is symbolic
symName :: Expr a -> Maybe String
symName (EScalar (SSym n)) = Just n
symName (EVector (TSym _ n)) = Just n
symName (EMatrix (TSym _ n)) = Just n
symName _ = Nothing


-- | get all the symbolic variables in an expression
getSyms :: Expr a -> [Expr a]
getSyms (EScalar x) = map EScalar $ sGetSyms x
getSyms (EVector x) = map EVector vs ++ map EScalar ss
  where
    (vs, ss) = tGetSyms x
getSyms (EMatrix x) = map EMatrix ms ++ map EScalar ss
  where
    (ms, ss) = tGetSyms x

-- | data type containing children of an expression
data Children a = CSource
                | CUnary a
                | CBinary a a

-- | get all the children of an Expr
getChildren :: Expr a -> Children (Expr a)
getChildren (EScalar (SUnary (Unary _ x))) = CUnary (EScalar x)
getChildren (EScalar (SBinary (Binary _ x y))) = CBinary (EScalar x) (EScalar y)
getChildren (EScalar _) = CSource

getChildren (EVector (TUnary (Unary _ x))) = CUnary (EVector x)
getChildren (EVector (TBinary (Binary _ x y))) = CBinary (EVector x) (EVector y)
getChildren (EVector (TBroadcast _ x)) = CUnary (EScalar x)
getChildren (EVector _) = CSource

getChildren (EMatrix (TUnary (Unary _ x))) = CUnary (EMatrix x)
getChildren (EMatrix (TBinary (Binary _ x y))) = CBinary (EMatrix x) (EMatrix y)
getChildren (EMatrix (TBroadcast _ x)) = CUnary (EScalar x)
getChildren (EMatrix _) = CSource


