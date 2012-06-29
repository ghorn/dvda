{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language TypeOperators #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module Dvda.HSSyntax ( writeHSSource
                     , GenHaskell
                     ) where

import Data.List ( intersperse )
import Data.Array.Repa ( DIM0, DIM1, DIM2 )
import Data.IntMap ( Key )
import qualified Data.IntMap as IM
import Numeric.LinearAlgebra ( Element )

import Dvda.Expr ( Expr(..), Const(..) )
import Dvda.SymMonad ( (:*)(..) )
import Dvda.Graph ( FunGraph(..), DynamicExpr, DvdaDim, asIfExpr )
import Dvda.BinUn ( BinOp(..), UnOp(..) )
import qualified Dvda.Config as Config

class GenHaskell a where
  typeSignature :: a -> String
  patternMatching :: a -> [String] -> (String, [String])

instance GenHaskell (Expr DIM0 Double) where
  typeSignature _ = "Double"
  patternMatching _ varStrings = (head varStrings, tail varStrings)

instance GenHaskell (Expr DIM1 Double) where
  typeSignature _ = "Vector Double"
  patternMatching _ varStrings = (head varStrings, tail varStrings)

instance GenHaskell (Expr DIM2 Double) where
  typeSignature _ = "Matrix Double"
  patternMatching _ varStrings = (head varStrings, tail varStrings)

instance (GenHaskell (Expr sh Double), DvdaDim sh) => GenHaskell [Expr sh Double] where
  typeSignature xs = "[" ++ typeSignature (head xs) ++ "]"
  patternMatching xs varStrings = (\(x0,x1) -> ('[':(concat $ intersperse "," x0) ++ "]", x1)) $
                                  splitAt (length xs) varStrings

instance (GenHaskell (Expr sh Double), DvdaDim sh) => GenHaskell [[Expr sh Double]] where
  typeSignature xs = "[[" ++ typeSignature (head (head xs)) ++ "]]"
  patternMatching xs varStrings = (\(x0,x1) -> ('[':(concat $ intersperse "," x0) ++ "]", x1)) $
                                  splitAt (length xs) varStrings

instance (GenHaskell a, GenHaskell b) => GenHaskell (a :* b) where
  typeSignature (x :* y) = typeSignature x ++ " :* " ++ typeSignature y
  patternMatching (x :* y) varStrings0 = (x' ++ " :* " ++ y', varStrings2)
    where
      (x', varStrings1) = patternMatching x varStrings0
      (y', varStrings2) = patternMatching y varStrings1


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

writeAssignment :: (Show a, Element a) => (Key, DynamicExpr a) -> (String, String)
writeAssignment (k, dexpr) 
  | asIfExpr isSym dexpr = ("-- " ++ Config.nameHSVar k ++ " (input)", show dexpr)
  | otherwise = (sassign k ++ (asIfExpr (pretty k) dexpr), show dexpr)
  where
    isSym (ESym _ _) = True
    isSym _ = False

writeHSSource :: (Show a, Element a, GenHaskell b, GenHaskell c) => FunGraph a b c -> String -> String
writeHSSource (FunGraph _ im inKeys outKeys) hash =
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
  , init $ unlines $ map ("    " ++) (zipWith3 (\d s c -> d ++ s ++ "-- " ++ c) decls extraSpaces comments)
  ]
    where
      spaces = replicate ((length (Config.nameHSFunction hash)) + 4) ' '
      inputs  = fst $ patternMatching ins  (map Config.nameHSVar inKeys)
      outputs = fst $ patternMatching outs (map Config.nameHSVar outKeys)
      (decls, comments) = unzip $ map writeAssignment (IM.toList im)

      lengths = map length decls
      longestDecl = maximum lengths
      extraSpaces = map (\n -> replicate (longestDecl - n + 4) ' ') lengths


