{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}

module Dvda.CGen ( writeCFun
                 , run
                 ) where

import Control.Monad.State (State)
import Data.Array.Repa (DIM0,DIM1,DIM2)
import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap as IM

import Dvda.SymMonad
import Dvda.Expr
import Dvda.Graph
import Dvda.BinUn

name :: Key -> String
name k = "t_" ++ show k

assign :: Key -> String
assign k = name k ++ " = "

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

writeAssignment :: (V.Unbox a, Show a) => (Key, GExpr a) -> String
writeAssignment (k, g@(GBinary Pow kx ky)) = assign k ++ "pow( " ++ name kx ++ ", " ++ name ky ++ " ); // " ++ show g
writeAssignment (k, g@(GBinary op kx ky)) = assign k ++ name kx ++ cBinary op ++ name ky ++ "; // " ++ show g
writeAssignment (k, g@(GUnary op kx)) = assign k ++ cUnary op ++ "( " ++ name kx ++ " ); // " ++ show g
writeAssignment (k, g@(GSym _ _)) = assign k ++ "hmmmmmmm; // " ++ show g
writeAssignment (k, g@(GSingleton _ x)) = assign k ++ show x ++ "; // " ++ show g
writeAssignment (k, g@(GScale kx ky)) = assign k ++ name kx ++ " * " ++ name ky ++ "; // " ++ show g
writeAssignment (k, g@(GDot kx ky)) = assign k ++ "dotlol( " ++ name kx ++ ", " ++ name ky ++ " ); // " ++ show g
writeAssignment (k, g@(GConst _ vec)) = assign k ++ "{ " ++ (tail . init) (show (V.toList vec)) ++ " }; // " ++ show g

writeCFun :: (Show a, V.Unbox a) => FunGraph a b c -> String
writeCFun (FunGraph hm im (_,ins) (_,outs)) = unlines $ map writeAssignment blah
  where
    blah = map (\(k,(y,_)) -> (k,y)) (IM.toList im)

    
exampleFun :: State (FunGraph Double (DIM0 :* DIM1 :* DIM2) (DIM2 :* DIM1 :* DIM0)) ()
exampleFun = do
  let x = sym "x"
      y = vsym 5 "y"
      z = msym (3,5) "Z"
  inputs_ (x :* y :* z)
  
  z1 <- node $ (scale x z)**3
  z2 <- node $ (dot z y)**2
  z3 <- node $ diff ((x*x/2)**x) x
  
  outputs_ (z1 :* z2 :* z3)

run :: IO ()
run = do
  let gr :: FunGraph Double (DIM0 :* DIM1 :* DIM2) (DIM2 :* DIM1 :* DIM0)
      gr = snd $ makeFun exampleFun
  putStrLn $ writeCFun gr
--  putStrLn $ funGraphSummary gr
--  putStrLn $ showCollisions gr
--  previewGraph gr
