-- GenerateC.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Codegen.GenerateC( functionToCSource
                                     ) where

import Data.Graph.Inductive hiding(out)
import Data.Maybe(fromJust)

import Numeric.Dvda.Function
import Numeric.Dvda.Expr.Expr
import Numeric.Dvda.Expr.ExprToGraph
import Numeric.Dvda.Expr.ElemwiseType
import Numeric.Dvda.Expr.SourceType
--import Numeric.Dvda.Expr.Misc(outputNames)

cType :: String
cType = "double"

functionToCSource :: (Eq a, Show a) => Function a -> (String, String)
functionToCSource fun = (src, include)
  where
    prototype = unlines [ "void call(const double * const in,"
                        , "          double * const out)"
                        ]
    include = prototype ++ ";"
    src = "#include \"math.h\"\n\n" ++ 
          prototype ++
          "\n{\n" ++
          inputDeclarations ++
          "\n" ++
          body ++
          "}"
      where
        inputDeclarations = unlines $ zipWith (\x k -> "    double " ++ show x ++" = in["++show k++"];")
                            (funInputs fun) [(0::Integer)..]
        body = unlines $ exprsToC (funOutputs fun)
    

exprsToC :: (Eq a, Show a) => [Expr a] -> [String]
exprsToC exprs = map (\x -> "    "++x)  body
  where
    body = grToC $ exprsToGraph exprs

grToC :: (Eq a, Show a) => Gr (GraphOp a) b -> [String]
grToC gr = map f (topsort gr)
  where
    f idx = toC idx (pre gr idx) (fromJust $ lab gr idx)

outputArrayHack :: String -> String
outputArrayHack ('o':'u':'t':num) = "out["++num++"]"
outputArrayHack _ = error "outputArrayHack fail"

toC :: (Eq a, Show a) => Node -> [Node] -> GraphOp a -> String
toC _ (x:[]) (GOutput out) = (outputArrayHack out) ++ " = " ++ nodeName x ++ ";"
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
