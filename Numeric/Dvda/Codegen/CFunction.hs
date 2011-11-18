-- CFunction.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Codegen.CFunction( exprsToCFunction
                                     ) where

import Data.Graph.Inductive hiding(out)
import Data.List(intercalate, nub)
import Data.Maybe(fromJust)

import Numeric.Dvda.Expr.Expr
import Numeric.Dvda.Expr.Apply(getSyms)
import Numeric.Dvda.Expr.ExprToGraph
import Numeric.Dvda.Expr.ElemwiseType
import Numeric.Dvda.Expr.SourceType
import Numeric.Dvda.Expr.Misc(outputNames)

cType :: String
cType = "double"

exprsToCFunction :: (Eq a, Show a) => [Expr a] -> String
exprsToCFunction exprs = intercalate "\n" $ (functionHeader inputs outputs):body++["}"]
  where
    body = exprsToC exprs
    inputs = nub $ concatMap getSyms exprs
    outputs = take (length exprs) outputNames
    
functionHeader :: (Eq a, Show a) => [Expr a] -> [String] -> String
functionHeader inputs outputs = funBeginning++vars++")\n{"
  where
    funBeginning = "void blah("
    whitespace = replicate (length funBeginning) ' '
    vars = intercalate (",\n"++whitespace) (inputProtos++outputProtos)
    inputProtos  = map (\x -> "const double "++show x) inputs
    outputProtos = map (\x -> "const double * const "++ x) outputs

exprsToC :: (Eq a, Show a) => [Expr a] -> [String]
exprsToC exprs = map (\x -> "    "++x)  body
  where
    body = grToC $ exprsToGraph exprs

grToC :: (Eq a, Show a) => Gr (GraphOp a) b -> [String]
grToC gr = map f (topsort gr)
  where
    f idx = toC idx (pre gr idx) (fromJust $ lab gr idx)

toC :: (Eq a, Show a) => Node -> [Node] -> GraphOp a -> String
toC _ (x:[]) (GOutput out) = out ++ " = " ++ nodeName x ++ ";"
toC idx _ (GSource sym@(Sym _)) = assign idx ++ show sym ++ ";"
toC idx _ src@(GSource _) = assign idx ++ show src ++ ";"
toC idx (x:y:[]) (GOp2 op2t) = assign idx ++ nodeName x ++" "++ show op2t ++" "++ nodeName y ++ ";"
toC idx (x:[]) (GElemwise Inv) = assign idx ++ "1 / " ++ show x ++ ";"
toC idx (x:[]) (GElemwise ewt) = assign idx ++ show ewt ++"( " ++ show x ++ " )" ++ ";"

toC idx pres (GElemwise ew) = "GElemwise fail: "++show (idx, pres, ew)
toC idx pres (GOp2 op2) = "GOp2 fail: "++show (idx, pres, op2)
toC idx pres (GOutput out) = "GOutput fail: " ++ show (idx, pres, out)

nodeName :: Int -> String
nodeName idx = 't':show idx

assign :: Int -> String
assign idx = cType ++ " " ++ nodeName idx ++ " = "
