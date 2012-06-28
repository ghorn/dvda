{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}

module Dvda.OctaveSyntax ( writeOctaveSource
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

octaveBinary :: BinOp -> String
octaveBinary Add = "+"
octaveBinary Sub = "-"
octaveBinary Mul = "*"
octaveBinary Div = "/"
octaveBinary Pow = "^"
octaveBinary LogBase = "error('no logBase here lol')"

octaveUnary :: UnOp -> String
octaveUnary Abs    = "abs"
octaveUnary Neg    = "-"
octaveUnary Signum = "sign"
octaveUnary Exp    = "exp"
octaveUnary Sqrt   = "sqrt"
octaveUnary Log    = "log"
octaveUnary Sin    = "sin"
octaveUnary Cos    = "cos"
octaveUnary Tan    = "tan"
octaveUnary ASin   = "asin"
octaveUnary ACos   = "acos"
octaveUnary ATan   = "atan"
octaveUnary Sinh   = "sinh"
octaveUnary Cosh   = "cosh"
octaveUnary Tanh   = "tanh"
octaveUnary ASinh  = "asinh"
octaveUnary ATanh  = "atanh"
octaveUnary ACosh  = "acosh"

pretty :: (Show a, Element a) => Int -> Expr sh a -> String
pretty _ (EBinary op (ERef _ kx) (ERef _ ky)) = Config.nameHSVar kx ++ " " ++ octaveBinary op ++ " " ++ Config.nameHSVar ky
pretty _ (EBinary _ _ _) = error "EBinary got non ERef children"
pretty _ (EUnary op (ERef _ kx)) = octaveUnary op ++ " " ++ Config.nameHSVar kx
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

writeAssignment :: (Show a, Element a) => (Key, DynamicExpr a) -> (String, String)
writeAssignment (k, dexpr) 
  | asIfExpr isSym dexpr = ("% " ++ Config.nameHSVar k ++ " (input)", show dexpr)
  | otherwise = (sassign k ++ asIfExpr (pretty k) dexpr, show dexpr)
  where
    isSym (ESym _ _) = True
    isSym _ = False

writeOctaveSource :: (Show a, Element a, MkIO b, MkIO c) => FunGraph a b c -> String -> String
writeOctaveSource (FunGraph _ im (ins,inKeys) (outs,outKeys)) hash =
  init $ unlines $
  [ "function " ++ outputs ++ " = " ++ Config.nameHSFunction hash ++ "(" ++ inputs ++ ")"
  , init $ unlines $ map ("    " ++) (zipWith3 (\d s c -> d ++ s ++ "% " ++ c) decls extraSpaces comments)
  , "end"
  ]
    where
      inputs  = fst $ patternMatching ins  (map Config.nameHSVar inKeys)
      outputs = fst $ patternMatching outs (map Config.nameHSVar outKeys)
      (decls, comments) = unzip $ map writeAssignment (IM.toList im)

      lengths = map length decls
      longestDecl = maximum lengths
      extraSpaces = map (\n -> replicate (longestDecl - n + 4) ' ') lengths
      
