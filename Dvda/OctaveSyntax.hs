{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}
{-# Language TypeOperators #-}

module Dvda.OctaveSyntax ( GenOctave
                         , toOctaveSource
                         ) where

import Data.Maybe ( fromJust )
import Data.List ( intersperse )
import Data.IntMap ( Key )
import qualified Data.IntMap as IM
import Numeric.LinearAlgebra ( Element )
import Text.Printf

import Dvda ( DIM0 )
import Dvda.Expr ( Expr(..), Const(..), isVal )
import Dvda.Graph ( FunGraph(..), DynamicExpr, asIfExpr )
import Dvda.BinUn ( BinOp(..), UnOp(..) )
import Dvda.SymMonad ( (:*)(..) )
import qualified Dvda.Config as Config

class GenOctave a where
  numObjects :: a -> Int
  writeOutputs :: a -> Int -> String
  writeInputs :: a -> Int -> (String, IM.IntMap String)

instance GenOctave (Expr DIM0 Double) where
  numObjects _ = 1
  writeOutputs e outputK = printf "%% output %d\noutput%d = %s; %% Expr DIM0 Double\n" outputK outputK (writeExpr e)
  writeInputs e@(ERef _ k) inputK = (printf "%% input %d\n%s\n" inputK decl, IM.singleton k decl)
    where
      decl = printf "%s = x%d;" (writeExpr e) inputK
  writeInputs e inputK = error $ "input " ++ show inputK ++ " is non-symbolic: " ++ show e


instance GenOctave [Expr DIM0 Double] where
  numObjects _ = 1
  writeOutputs [] outputK = printf "%% output %d\noutput%d = []; %% [Expr DIM0 Double]\n" outputK outputK;
  writeOutputs exprs outputK =
    unlines $
    (printf "%% output %d\noutput%d = zeros(%d,1); %% [Expr DIM0 Double]" outputK outputK (length exprs)):
    zipWith f [(1::Int)..] exprs
    where
      f outIdx e = printf "%soutput%d(%d) = %s;" maybeComment outputK outIdx (writeExpr e)
        where
          maybeComment
            | isVal 0 e = "% "
            | otherwise = ""
  writeInputs exprs inputK = ((printf "%% input %d\n" inputK) ++ unlines (map snd keyDecls), IM.fromList keyDecls)
    where
      keyDecls = zipWith f [(1::Int)..] exprs
      f outIdx e@(ERef _ k) = (k, printf "%s = x%d(%d);" (writeExpr e) inputK outIdx)
      f outIdx e = error $ "input " ++ show inputK ++ ", " ++ show outIdx ++" is non-symbolic: " ++ show e

instance GenOctave [[Expr DIM0 Double]] where
  numObjects _ = 1
  writeOutputs [] outputK = printf "%% output %d\noutput%d = []; %% [[Expr DIM0 Double]]\n" outputK outputK;
  writeOutputs exprs outputK =
    unlines $
    (printf "output%d = zeros(%d,%d); %% [[Expr DIM0 Double]]" outputK (length exprs) (length (head exprs))):
    zipWith f [(r,c) | r <- [1..length exprs], c <- [1..(length (head exprs))]] (concat exprs)
    where
      f (rowIdx,colIdx) e = printf "%soutput%d(%d,%d) = %s;" maybeComment outputK rowIdx colIdx (writeExpr e)
        where
          maybeComment
            | isVal 0 e = "% "
            | otherwise = ""
  writeInputs exprs inputK = ((printf "% input %d\n" inputK) ++ unlines (map snd keyDecls), IM.fromList keyDecls)
    where
      keyDecls = zipWith f [(r,c) | r <- [1..length exprs], c <- [1..(length (head exprs))]] (concat exprs)
      f (rowIdx,colIdx) e@(ERef _ k) = (k, printf "%s = x%d(%d,%d);" (writeExpr e) inputK rowIdx colIdx)
      f outIdx e = error $ "input " ++ show inputK ++ ", " ++ show outIdx ++" is non-symbolic: " ++ show e

instance (GenOctave a, GenOctave b) => GenOctave (a :* b) where
  numObjects (x :* y) = numObjects x + numObjects y
  writeOutputs (x :* y) outputK = writeOutputs x outputK ++ "\n" ++ writeOutputs y (outputK + numObjects x)
  writeInputs  (x :* y) inputK = (headerX ++ '\n' : headerY, IM.unionWith err imx imy)
    where
      (headerX,imx) = writeInputs x inputK
      (headerY,imy) = writeInputs y (inputK + numObjects x)
      err = error "writeInputs (x :* y) got inputs from two sources"

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

writeExpr :: (Show a, Element a) => Expr sh a -> String
writeExpr (ERef _ k) = Config.nameHSVar k
writeExpr (EBinary op x y) = writeExpr x ++ " " ++ octaveBinary op ++ " " ++ writeExpr y
writeExpr (EUnary op x) = octaveUnary op ++ "( " ++ writeExpr x ++ " )"
writeExpr (EScale x y) = "LA.scale " ++ writeExpr x ++ " " ++ writeExpr y
writeExpr (EConst (CSingleton _ x)) = show x
writeExpr (EConst (CVec _ x)) = show x -- Config.nameHSConst k
writeExpr (EConst (CMat _ x)) = show x -- Config.nameHSConst k
writeExpr (EConst (CTensor _ x)) = show x -- Config.nameHSConst k
writeExpr (ESym _ _) = error "ESym shouldn't be handled here"
writeExpr (EDimensionless _) = error "EDimensionless shouldn't be handled here"
writeExpr (EJacob _ _) = error "EJacob shouldn't be handled here"
writeExpr (EDeriv _ _) = error "EDeriv shouldn't be handled here"
writeExpr (EGrad _ _)  = error "EGrad shouldn't be handled here"

writeAssignment :: (Show a, Element a) => IM.IntMap String -> (Key, DynamicExpr a) -> (String, String)
writeAssignment inputMap (k, dexpr)
  | asIfExpr isSym dexpr = (fromJust $ IM.lookup k inputMap, drop 13 (show dexpr))
  | otherwise = (sassign k ++ asIfExpr writeExpr dexpr ++ ";", drop 13 (show dexpr))
  where
    isSym (ESym _ _) = True
    isSym _ = False


toOctaveSource :: (Show a, Element a, GenOctave b, GenOctave c) =>
                  FunGraph a b c -> String -> String
toOctaveSource (FunGraph _ im inputs outputs) funName =
  unlines $
  [ "function [" ++ outputHeader ++ "] = " ++ funName ++ "(" ++ inputHeader ++ ")"
  , ""
  , "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% inputs: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  , unlines $ map ('%' :) $ lines inputDecls
  , "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% body: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  , unlines $ (zipWith3 (\d s c -> d ++ s ++ "% " ++ c) decls extraSpaces comments)
  , "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% outputs: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  , writeOutputs outputs 0
  , "end"
  ]
    where
      header prefix num = concat $ intersperse ","  $ map (\k -> prefix ++ show k) [0..(num - 1)]
      inputHeader  = header "x" (numObjects inputs)
      outputHeader  = header "output" (numObjects outputs)
      (decls, comments) = unzip $ map (writeAssignment inputMap) (IM.toList im)

      (inputDecls, inputMap) = writeInputs inputs 0

      lengths = map length decls
      longestDecl = maximum lengths
      extraSpaces = map (\n -> replicate (longestDecl - n + 4) ' ') lengths
