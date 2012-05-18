{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Dvda.Codegen.HSSyntax ( writeHSource
                             , run
                             ) where

import Data.Array.Repa (DIM0) -- ,DIM1,DIM2)
import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap as IM
--import Data.Maybe ( fromJust )
import Data.List ( intersperse )

import Dvda.SymMonad ( (:*)(..), makeFun, inputs_, outputs_ , node )
import Dvda.Expr
import Dvda.Graph
import Dvda.BinUn
import qualified Dvda.Config as Config


--gr :: FunGraph Double (DIM0 :* DIM1 :* DIM2) (DIM2 :* DIM1 :* DIM0)
gr :: FunGraph Double (DIM0 :* DIM0 :* DIM0) (DIM0 :* DIM0 :* DIM0)
gr = snd $ makeFun $ do
  let x = sym "x"
--      y = vsym 5 "y"
      y = sym "y"
--      z = msym (3,5) "Z"
      z = sym "Z"
  inputs_ (x :* z :* y)
  
  z1 <- node $ (scale x z)**3
--  z2 <- node $ (dot z y)**2
  z2 <- node $ (z*y)**2
--  z3 <- node $ diff ((x*x/2)**x) x
  z3 <- node $ ((x*x/2)**x)*x
  
  outputs_ (z1 :* z2 :* z3)


run :: IO ()
run = do
--  putStrLn $ funGraphSummary gr
--  putStrLn $ funGraphSummary' gr
--  putStrLn $ showCollisions gr
  print gr
  putStrLn $ writeHSource gr "214234098"
--  previewGraph gr


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

pretty :: (Show a, V.Unbox a) => (Int, GExpr a) -> String
pretty (_, (GBinary op kx ky)) = hBinary op ++ " " ++ Config.nameHSVar kx ++ " " ++ Config.nameHSVar ky
pretty (_, (GUnary op kx)) = hUnary op ++ " " ++ Config.nameHSVar kx
pretty (_, (GSingleton _ x)) = show x
pretty (_, (GScale kx ky)) = "scale " ++ Config.nameHSVar kx ++ " " ++ Config.nameHSVar ky
pretty (_, (GDot kx ky)) = "dot " ++ Config.nameHSVar kx ++ " " ++ Config.nameHSVar ky
--pretty (k, (GConst _ vec)) = Config.nameHSConst k
pretty (_, (GConst _ vec)) = show vec -- Config.nameHSConst k
pretty (_, (GSym _ _)) = error "GSym shouldn't be handled here"

writeAssignment :: (Show a, V.Unbox a) => (Key, GExpr a) -> String
writeAssignment (k, gexpr@(GSym _ _)) = "-- " ++ Config.nameHSVar k ++ ": " ++ show gexpr
writeAssignment (k, gexpr) = sassign k ++ pretty (k,gexpr) ++ " -- " ++ show gexpr

writeHSource :: (V.Unbox a, Show a, Show b, Show c) => FunGraph a b c -> String -> String
writeHSource (FunGraph _ im (insT,ins) (outsT,outs)) hash =
  init $ unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , "{-# Language GADTs #-}"
  , "{-# Language FlexibleContexts #-}"
  , "{-# Language TypeOperators #-}"
  , "{-# Language TypeFamilies #-}"
  , ""
  , "module " ++ Config.nameHSModule hash ++ " ( " ++ Config.nameHSFunction hash ++ " ) where"
  , ""
  , "import Data.Vector.Unbox as V"
  , "import Dvda"
  , ""
--  , "-- constants:"
--  , constants
--  , ""
  , Config.nameHSFunction hash ++ " :: " ++ show insT ++ " -> " ++ show outsT
  , Config.nameHSFunction hash ++ " ( " ++ inputs ++ " ) = " ++ outputs
  , "  where"
  , init $ unlines $ map ("    " ++) body 
  ]
    where
      inputs  = concat $ intersperse " :* " (map Config.nameHSVar ins)
      outputs = concat $ intersperse " :* " (map Config.nameHSVar outs)
      body = map writeAssignment gnodes
      gnodes = map (\(k,(y,_)) -> (k,y)) (IM.toList im)
