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
import Numeric.Dvda.Internal.Vector
import Numeric.Dvda.Internal.Matrix
import Numeric.Dvda.Internal.GNode

showNode :: Show a => Expr a -> String
showNode (EScalar x) = sShowNode x
showNode (EVector x) = vShowNode x
showNode (EMatrix x) = mShowNode x

showType :: Show a => Expr a -> String
showType (EScalar (SNum _)) = "N"
showType (EScalar (SInt _)) = "I"
showType _ = ""

showDim :: Show a => Expr a -> String
showDim (EScalar _) = ""
showDim (EVector x) = show [vecDim x]
showDim (EMatrix x) = show $ matDim x

-- | convert Expr gnode to c code string
toCCode :: (Eq a, Show a) => GNode (Expr a) -> String
toCCode (GSource i (EScalar x)) = sToCCode $ GSource i x
toCCode (GSource i (EVector x)) = vToCCode $ GSource i x
toCCode (GSource i (EMatrix x)) = mToCCode $ GSource i x

toCCode (GUnary i (EScalar x) ix) = sToCCode $ GUnary i x ix
toCCode (GUnary i (EVector x) ix) = vToCCode $ GUnary i x ix
toCCode (GUnary i (EMatrix x) ix) = mToCCode $ GUnary i x ix

toCCode (GBinary i (EScalar x) (ix, iy)) = sToCCode $ GBinary i x (ix, iy)
toCCode (GBinary i (EVector x) (ix, iy)) = vToCCode $ GBinary i x (ix, iy)
toCCode (GBinary i (EMatrix x) (ix, iy)) = mToCCode $ GBinary i x (ix, iy)

-- | get name if variable is symbolic
symName :: Expr a -> Maybe String
symName (EScalar (SSym n)) = Just n
symName (EVector (VSym _ n)) = Just n
symName (EMatrix (MSym _ n)) = Just n
symName _ = Nothing


-- | get all the symbolic variables in an expression
getSyms :: Expr a -> [Expr a]
getSyms (EScalar x) = map EScalar $ sGetSyms x
getSyms (EVector x) = map EVector vs ++ map EScalar ss
  where
    (vs, ss) = vGetSyms x
getSyms (EMatrix x) = map EMatrix ms ++ map EScalar ss
  where
    (ms, ss) = mGetSyms x

-- | data type containing children of an expression
data Children a = CSource
                | CUnary a
                | CBinary a a

-- | get all the children of an Expr
getChildren :: Expr a -> Children (Expr a)
getChildren (EScalar (SUnary (Unary _ x))) = CUnary (EScalar x)
getChildren (EScalar (SBinary (Binary _ x y))) = CBinary (EScalar x) (EScalar y)
getChildren (EScalar _) = CSource

getChildren (EVector (VUnary (Unary _ x))) = CUnary (EVector x)
getChildren (EVector (VBinary (Binary _ x y))) = CBinary (EVector x) (EVector y)
getChildren (EVector (VBroadcast _ x)) = CUnary (EScalar x)
getChildren (EVector _) = CSource

getChildren (EMatrix (MUnary (Unary _ x))) = CUnary (EMatrix x)
getChildren (EMatrix (MBinary (Binary _ x y))) = CBinary (EMatrix x) (EMatrix y)
getChildren (EMatrix (MBroadcast _ x)) = CUnary (EScalar x)
getChildren (EMatrix _) = CSource


