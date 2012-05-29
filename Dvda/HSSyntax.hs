{-# OPTIONS_GHC -Wall #-}

module Dvda.HSSyntax ( writeHSSource
                     ) where

import Data.IntMap ( Key )
import qualified Data.IntMap as IM
import Numeric.LinearAlgebra ( Element )
import Data.Array.Repa ( Shape(rank) )

import Dvda.GExpr ( GExpr(..) )
import Dvda.SymMonad ( MkIO(..) )
import Dvda.Graph ( FunGraph(..) )
import Dvda.BinUn ( BinOp(..), UnOp(..) )
import qualified Dvda.Config as Config


-- assign a scalar
sassign :: Key -> String
sassign k = Config.nameHSVar k ++ " = "

hBinary :: BinOp -> String
hBinary Add = "(+)"
hBinary Sub = "(-)"
hBinary Mul = "(*)"
hBinary Div = "(/)"
hBinary Pow = "(**)"
hBinary LogBase = "logBase"

hUnary :: UnOp -> String
hUnary Abs    = "abs"
hUnary Neg    = "negate"
hUnary Signum = "signum"
hUnary Exp    = "exp"
hUnary Sqrt   = "sqrt"
hUnary Log    = "log"
hUnary Sin    = "sin"
hUnary Cos    = "cos"
hUnary Tan    = "tan"
hUnary ASin   = "asin"
hUnary ACos   = "acos"
hUnary ATan   = "atan"
hUnary Sinh   = "sinh"
hUnary Cosh   = "cosh"
hUnary Tanh   = "tanh"
hUnary ASinh  = "asinh"
hUnary ATanh  = "atanh"
hUnary ACosh  = "acosh"

pretty :: (Show a, Element a) => (Int, GExpr a) -> String
pretty (_, (GBinary _ op kx ky)) = hBinary op ++ " " ++ Config.nameHSVar kx ++ " " ++ Config.nameHSVar ky
pretty (_, (GUnary _ op kx)) = hUnary op ++ " " ++ Config.nameHSVar kx
pretty (_, (GSingleton _ x)) = show x
pretty (_, (GScale _ kx ky)) = "LA.scale " ++ Config.nameHSVar kx ++ " " ++ Config.nameHSVar ky
pretty (_, (GDot dx dy kx ky)) = op ++ " " ++ Config.nameHSVar kx ++ " " ++ Config.nameHSVar ky
  where
    op = case (rank dx, rank dy) of (1, 1) -> "(LA.<.>)"
                                    (2, 2) -> "(LA.mXm)"
                                    (2, 1) -> "(LA.mXv)"
                                    (1, 2) -> "(LA.vXm)"
                                    _ -> error "need moar dottable"
--pretty (k, (GConst _ vec)) = Config.nameHSConst k
pretty (_, (GTensor _ x)) = show x -- Config.nameHSConst k
pretty (_, (GVec _ x)) = show x -- Config.nameHSConst k
pretty (_, (GMat _ x)) = show x -- Config.nameHSConst k
pretty (_, (GSym _ _)) = error "GSym shouldn't be handled here"

writeAssignment :: (Show a, Element a) => (Key, GExpr a) -> String
writeAssignment (k, gexpr@(GSym _ _)) = "-- " ++ Config.nameHSVar k ++ ": " ++ show gexpr
writeAssignment (k, gexpr) = sassign k ++ pretty (k,gexpr) ++ " -- " ++ show gexpr

writeHSSource :: (Show a, Element a, MkIO b, MkIO c) => FunGraph a b c -> String -> String
writeHSSource (FunGraph _ im (ins,inKeys) (outs,outKeys)) hash =
  init $ unlines $
  [ "-- {-# OPTIONS_GHC -Wall #-}"
  , "{-# Language GADTs #-}"
  , "{-# Language FlexibleContexts #-}"
  , "{-# Language TypeOperators #-}"
  , "{-# Language TypeFamilies #-}"
  , ""
  , "module " ++ Config.nameHSModule hash ++ " ( " ++ Config.nameHSFunction hash ++ " ) where"
  , ""
  , "import Dvda"
  , "import qualified Numeric.LinearAlgebra as LA"
  , ""
--  , "-- constants:"
--  , constants
--  , ""
--  , Config.nameHSFunction hash ++ " :: Floating a => " 
--  , spaces ++ rewriteType (show insT) ++ " -> " 
--  , spaces ++ rewriteType (show outsT)
  , Config.nameHSFunction hash ++ " :: " ++ typeSignature ins ++ " ->"
  , spaces ++ typeSignature outs
  , Config.nameHSFunction hash ++ " ( " ++ inputs ++ " ) = " ++ outputs
  , "  where"
  , init $ unlines $ map ("    " ++) body 
  ]
    where
      spaces = replicate ((length (Config.nameHSFunction hash)) + 4) ' '
      inputs  = fst $ patternMatching ins  (map Config.nameHSVar inKeys)
      outputs = fst $ patternMatching outs (map Config.nameHSVar outKeys)
      body = map writeAssignment (IM.toList im)
