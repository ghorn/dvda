{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Dvda.Codegen.CSyntax ( writeCSource
                            , writeCInclude
                            ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap as IM
import Data.Maybe ( fromJust )
import Data.Char ( toUpper )

import Dvda.Graph
import Dvda.BinUn
import qualified Dvda.Config as Config

name :: Key -> String
name = Config.cName

-- assign a scalar
sassign :: Key -> String
sassign k = "const " ++ Config.cType ++ " " ++ name k ++ " = "

cBinary :: BinOp -> String
cBinary Add = " + "
cBinary Sub = " - "
cBinary Mul = " * "
cBinary Div = " / "
cBinary Pow = error "cBinary pow not infix"
cBinary LogBase = error "C gen doesn't support logbase"

cUnary :: UnOp -> String
cUnary Abs    = "abs"
cUnary Neg    = "-"
cUnary Signum = "signum"
cUnary Exp    = "exp"
cUnary Sqrt   = "sqrt"
cUnary Log    = "log"
cUnary Sin    = "sin"
cUnary Cos    = "cos"
cUnary Tan    = "tan"
cUnary ASin   = "asin"
cUnary ACos   = "acos"
cUnary ATan   = "atan"
cUnary Sinh   = "sinh"
cUnary Cosh   = "cosh"
cUnary Tanh   = "tanh"
cUnary ASinh  = "asinh"
cUnary ATanh  = "atanh"
cUnary ACosh  = "acosh"


writeAssignment :: (V.Unbox a, Show a) => (Int -> Int) -> (Key, GExpr a) -> String
-- inputs
writeAssignment inputMap (k, g@(GSym [] _)) =
  sassign k ++ "*(in[" ++ show (inputMap k) ++ "]); // " ++ show g
writeAssignment inputMap (k, g@(GSym _ _)) =
  "const " ++ Config.cType ++ " * const " ++ name k ++ " = *(in[" ++ show (inputMap k) ++ "]); // " ++ show g
  
writeAssignment _ (k, g@(GBinary Pow kx ky)) =
  sassign k ++ "pow( " ++ name kx ++ ", " ++ name ky ++ " ); // " ++ show g
writeAssignment _ (k, g@(GBinary op kx ky)) =
  sassign k ++ name kx ++ cBinary op ++ name ky ++ "; // " ++ show g
writeAssignment _ (k, g@(GUnary op kx)) =
  sassign k ++ cUnary op ++ "( " ++ name kx ++ " ); // " ++ show g
writeAssignment _ (k, g@(GSingleton _ x)) =
  sassign k ++ show x ++ "; // " ++ show g
writeAssignment _ (k, g@(GScale kx ky)) =
  sassign k ++ name kx ++ " * " ++ name ky ++ "; // " ++ show g
writeAssignment _ (k, g@(GDot kx ky)) =
  sassign k ++ "dotlol( " ++ name kx ++ ", " ++ name ky ++ " ); // " ++ show g
writeAssignment _ (k, g@(GConst _ vec)) =
  sassign k ++ "{ " ++ (tail . init) (show (V.toList vec)) ++ " }; // " ++ show g


prototype :: String -> String
prototype hash = "void " ++ Config.nameCFunction hash ++ "(const double * const in[] __attribute__ ((__unused__)), double * const out[])"
    
writeCInclude :: String -> String
writeCInclude hash = init $ unlines $
                    [ "// " ++ Config.nameCInclude hash
                    , ""
                    , "#ifndef " ++ pragma
                    , "#define " ++ pragma
                    , ""
                    , prototype hash ++ ";"
                    , ""
                    , "#endif // " ++ pragma
                    ]
  where
    pragma = "__" ++ map toUpper hash ++ "__"

writeCSource :: (Show a, V.Unbox a) => FunGraph a b c -> String -> String
writeCSource (FunGraph _ im (_,ins) (_,outs)) hash =
  init $ unlines $
  [ "// " ++ Config.nameCSource hash
  , ""
  , "#include \"math.h\""
  , "#include \"string.h\""
  , "#include \"" ++ Config.nameCInclude hash ++ "\""
  , ""
  , prototype hash
  , "{"
  , init $ unlines $ map ("    " ++) body 
  , "\n    // output assignments:"
  , init $ unlines $ map ("    " ++) outputs
  , "}"
  ]
    where
      gnodes = map (\(k,(y,_)) -> (k,y)) (IM.toList im)
      body = map (writeAssignment (\k -> fromJust $ IM.lookup k inputMap)) gnodes
        where
          -- map node -> input number
          inputMap = IM.fromList $ zip ins [0..]

      outputs = zipWith assignOutput outs [(0::Int)..]
        where
          assignOutput k n = "*(out[" ++ show n ++ "]) = " ++ name k ++ ";"
