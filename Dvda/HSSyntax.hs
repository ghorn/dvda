{-# OPTIONS_GHC -Wall #-}

module Dvda.HSSyntax ( writeHSSource
                     ) where

import Data.IntMap ( Key )
import Data.List ( intersperse )
import qualified Data.IntMap as IM
import qualified Data.Text.Lazy as T
import Numeric.LinearAlgebra ( Element )

import Dvda.GExpr ( GExpr(..) )
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
pretty (_, (GScale _ kx ky)) = "scale " ++ Config.nameHSVar kx ++ " " ++ Config.nameHSVar ky
pretty (_, (GDot _ _ kx ky)) = "dot " ++ Config.nameHSVar kx ++ " " ++ Config.nameHSVar ky
--pretty (k, (GConst _ vec)) = Config.nameHSConst k
pretty (_, (GTensor _ x)) = show x -- Config.nameHSConst k
pretty (_, (GVec _ x)) = show x -- Config.nameHSConst k
pretty (_, (GMat _ x)) = show x -- Config.nameHSConst k
pretty (_, (GSym _ _)) = error "GSym shouldn't be handled here"

writeAssignment :: (Show a, Element a) => (Key, GExpr a) -> String
writeAssignment (k, gexpr@(GSym _ _)) = "-- " ++ Config.nameHSVar k ++ ": " ++ show gexpr
writeAssignment (k, gexpr) = sassign k ++ pretty (k,gexpr) ++ " -- " ++ show gexpr

writeHSSource :: (Show a, Show b, Show c, Element a) => FunGraph a b c -> String -> String
writeHSSource (FunGraph _ im (insT,ins) (outsT,outs)) hash =
  init $ unlines $
  [ "-- {-# OPTIONS_GHC -Wall #-}"
  , "{-# Language GADTs #-}"
  , "{-# Language FlexibleContexts #-}"
  , "{-# Language TypeOperators #-}"
  , "{-# Language TypeFamilies #-}"
  , ""
  , "module " ++ Config.nameHSModule hash ++ " ( " ++ Config.nameHSFunction hash ++ " ) where"
  , ""
  , "import Data.Array.Repa"
  , "import Data.Vector.Unboxed as V"
  , "import Dvda"
  , ""
--  , "-- constants:"
--  , constants
--  , ""
--  , Config.nameHSFunction hash ++ " :: Floating a => " 
--  , spaces ++ rewriteType (show insT) ++ " -> " 
--  , spaces ++ rewriteType (show outsT)
  , Config.nameHSFunction hash ++ " :: " ++ rewriteType (show insT) ++ " ->"
  , spaces ++ rewriteType (show outsT)
  , Config.nameHSFunction hash ++ " ( " ++ inputs ++ " ) = " ++ outputs
  , "  where"
  , init $ unlines $ map ("    " ++) body 
  ]
    where
      spaces = replicate ((length (Config.nameHSFunction hash)) + 4) ' '
      inputs  = concat $ intersperse " :* " (map Config.nameHSVar ins)
      outputs = concat $ intersperse " :* " (map Config.nameHSVar outs)
      body = map writeAssignment (IM.toList im)


intercalate :: String -> [String] -> String
intercalate _ [] = []
intercalate _ [x] = x
intercalate int (x:xs) = (x++int) ++ intercalate int xs

rewriteType :: String -> String
rewriteType typeString = final
  where
    text = T.pack typeString
    -- "Z :* ((Z :. 5) :* ((Z :. 3) :. 5))"
    
    cleaned = T.filter (\x -> not (elem x "() ")) text
    -- "Z:*Z:.5:*Z:.3:.5"
    
    grouped :: [T.Text]
    grouped = T.splitOn (T.pack ":*") cleaned
    -- ["Z", "Z:.5", "Z:.3:.5"]
    
    
    grouped' :: [[T.Text]]
    grouped' = map (T.splitOn (T.pack ":.")) grouped
    -- [["Z"], ["Z","5"], ["Z","3","5"]]

    counted :: [Int]
    counted = map (\x -> length x - 1) grouped'
    -- [0, 1, 2]

    addExpr = map (\x -> "(Expr DIM" ++ show x ++ " Double)")  counted
    -- ["(Expr DIM0 Double)", "(Expr DIM1 Double)", "(Expr DIM2 Double)"]
    
    final = "( " ++ (intercalate " :* " addExpr) ++ " )"
    -- "( (Expr DIM0 Double) :* (Expr DIM1 Double) :* (Expr DIM2 Double) )"


-- rewriteType :: String -> String
-- rewriteType typeString = final
--   where
--     text = T.pack typeString
--     -- "Z :* ((Z :. 5) :* ((Z :. 3) :. 5))"
--     
--     cleaned = T.filter (\x -> not (elem x "() ")) text
--     -- "Z:*Z:.5:*Z:.3:.5"
--     
--     grouped = T.splitOn (T.pack ":*") cleaned
--     -- ["Z", "Z:.5", "Z:.3:.5"]
--     
--     addExpr = map (\x -> T.append "(Expr (" (T.append x ") a)"))  grouped
--     -- ["(Expr (Z) a)", "(Expr (Z:.5) a)", "(Expr (Z:.3:.5) a)"]
--     
--     final = "( " ++ T.unpack (T.intercalate " :* " addExpr) ++ " )"
--     -- "( (Expr (Z) a) :* (Expr (Z:.5) a) :* (Expr (Z:.3:.5) a) )"
