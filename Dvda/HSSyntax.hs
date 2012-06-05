{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}

module Dvda.HSSyntax ( writeHSSource
                     ) where

import Data.IntMap ( Key )
import qualified Data.IntMap as IM
import Numeric.LinearAlgebra ( Element )

import Dvda.Expr ( Expr(..), Const(..) )
import Dvda.SymMonad ( MkIO(..) )
import Dvda.Graph ( FunGraph(..), DynamicExpr, asIfExpr )
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

pretty :: (Show a, Element a) => Int -> Expr sh a -> String
pretty _ (EBinary op (ERef _ kx) (ERef _ ky)) = hBinary op ++ " " ++ Config.nameHSVar kx ++ " " ++ Config.nameHSVar ky
pretty _ (EBinary _ _ _) = error "EBinary got non ERef children"
pretty _ (EUnary op (ERef _ kx)) = hUnary op ++ " " ++ Config.nameHSVar kx
pretty _ (EUnary _ _) = error "EUnary got non ERef children"
pretty _ (EScale (ERef _ kx) (ERef _ ky)) = "LA.scale " ++ Config.nameHSVar kx ++ " " ++ Config.nameHSVar ky
pretty _ (EScale _ _) = error "EScale got non ERef children"
pretty _ (EConst (CSingleton _ x)) = show x
pretty _ (EConst (CVec _ x)) = show x -- Config.nameHSConst k
pretty _ (EConst (CMat _ x)) = show x -- Config.nameHSConst k
pretty _ (EConst (CTensor _ x)) = show x -- Config.nameHSConst k
pretty _ (ESym _ _) = error "ESym shouldn't be handled here"
pretty _ (EDimensionless _) = error "EDimensionless shouldn't be handled here"
pretty _ (EJacob _ _) = error "EJacob shouldn't be handled here"
pretty _ (EDeriv _ _) = error "EDeriv shouldn't be handled here"
pretty _ (EGrad _ _)  = error "EGrad shouldn't be handled here"
pretty _ (ERef _ _) = error "ERef shouldn't be handled here"

writeAssignment :: (Show a, Element a) => (Key, DynamicExpr a) -> String
writeAssignment (k, dexpr) 
  | asIfExpr isSym dexpr = "-- " ++ Config.nameHSVar k ++ ": " ++ show dexpr
  | otherwise = sassign k ++ (asIfExpr (pretty k) dexpr) ++ " -- " ++ show dexpr
  where
    isSym (ESym _ _) = True
    isSym _ = False

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
