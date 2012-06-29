{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}
{-# Language TypeOperators #-}

module Dvda.OctaveSyntax ( OctaveOutputs
                         , showOctaveSource
                         ) where

import Data.List ( intersperse )
import Data.Maybe ( fromJust )
import Data.IntMap ( IntMap, Key )
import qualified Data.IntMap as IM
import Numeric.LinearAlgebra ( Element )
import Text.Printf

import Dvda ( DIM0 )
import Dvda.Expr ( Expr(..), Const(..) )
import Dvda.Graph ( FunGraph(..), DynamicExpr, asIfExpr )
import Dvda.BinUn ( BinOp(..), UnOp(..) )
import Dvda.SymMonad ( (:*)(..), KeyT )
import qualified Dvda.Config as Config

class OctaveOutputs a where
  numOutputs :: a -> Int
  writeOutputs :: a -> Int -> String

instance OctaveOutputs (Expr DIM0 Double, Int) where
  numOutputs _ = 1
  writeOutputs (_, key) outputK = printf "\noutput%d = %s; %% Expr DIM0 Double" outputK (Config.nameHSVar key)

instance OctaveOutputs ([Expr DIM0 Double], [Int]) where
  numOutputs _ = 1
  writeOutputs (_, []) outputK = printf "\noutput%d = []; %% [Expr DIM0 Double]" outputK;
  writeOutputs (_, keys) outputK = init $ unlines $
                                 [printf "\noutput%d = zeros(%d,1); %% [Expr DIM0 Double]" outputK (length keys)] ++
                                 zipWith f [(1::Int)..] keys
    where
      f outIdx node = printf "output%d(%d) = %s;" outputK outIdx (Config.nameHSVar node)

instance OctaveOutputs ([[Expr DIM0 Double]], [[Int]]) where
  numOutputs _ = 1
  writeOutputs (_, []) outputK = printf "\noutput%d = []; %% [[Expr DIM0 Double]]" outputK;
  writeOutputs (_, keys) outputK =
    init $ unlines $
    (printf "\noutput%d = zeros(%d,%d); %% [[Expr DIM0 Double]]" outputK (length keys) (length (head keys))):
    zipWith f [(r,c) | r <- [1..length (keys)], c <- [1..(length (head keys))]] (concat keys)
    where
      f (rowIdx,colIdx) node = printf "output%d(%d,%d) = %s;" outputK rowIdx colIdx (Config.nameHSVar node)


instance (OctaveOutputs a, OctaveOutputs b) => OctaveOutputs (a :* b) where
  numOutputs (kp0 :* kp1) = numOutputs kp0 + numOutputs kp1
  writeOutputs (kp0 :* kp1) outputK = writeOutputs kp0 outputK ++ "\n" ++
                                      writeOutputs kp1 (outputK + numOutputs kp0)

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

writeAssignment :: (Show a, Element a) => IntMap (Int, Key) -> (Key, DynamicExpr a) -> (String, String)
writeAssignment inputMap (k, dexpr)
  | asIfExpr isSym dexpr = (Config.nameHSVar k ++ " = " ++ octaveInput, drop 13 (show dexpr))
  | otherwise = (sassign k ++ asIfExpr (pretty k) dexpr, drop 13 (show dexpr))
  where
    octaveInput = (\(xk, j) -> "x" ++ show xk ++ "(" ++ show j ++ ")") (fromJust $ IM.lookup k inputMap)
    isSym (ESym _ _) = True
    isSym _ = False


showOctaveSource :: (Show a, Element a, OctaveOutputs c) =>
                    FunGraph a (KeyT ([Expr DIM0 Double] :* [Expr DIM0 Double])) c -> String -> String
showOctaveSource (FunGraph _ im ((_, inKeys0) :* (_, inKeys1)) outs) hash =
  init $ unlines $
  [ "function " ++ outputHeader ++ " = " ++ Config.nameOctaveFunction hash ++ "( x0, x1 )"
  , ""
  , init $ unlines $ (zipWith3 (\d s c -> d ++ s ++ "% " ++ c) decls extraSpaces comments)
  , ""
  , ""
  , "% outputs:"
  , writeOutputs outs 0
  , ""
  , "end"
  ]
    where
      inputMap = IM.fromList $
                 (zipWith (\k inputK -> (k, (0, inputK))) inKeys0 [1..]) ++
                 (zipWith (\k inputK -> (k, (1, inputK))) inKeys1 [1..])
      
      outputHeader = "[" ++
                     concat (intersperse "," (map (\k -> "output"++show k) [0..(numOutputs outs - 1)])) ++
                     "]"
      (decls, comments) = unzip $ map (writeAssignment inputMap) (IM.toList im)

      lengths = map length decls
      longestDecl = maximum lengths
      extraSpaces = map (\n -> replicate (longestDecl - n + 4) ' ') lengths
