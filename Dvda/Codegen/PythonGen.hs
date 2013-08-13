{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}

module Dvda.Codegen.PythonGen ( showPy
                              ) where

import Data.Hashable ( Hashable )
import qualified Data.Foldable as F
import Data.List ( intercalate )
import qualified Data.Vector as V
import Text.Printf ( printf )

import Dvda.Expr ( GExpr(..), Floatings(..), Nums(..), Fractionals(..) )
import Dvda.FunGraph ( FunGraph, fgTopSort, fgInputs, fgOutputs, fgLookupGExpr )
import Dvda.HashMap ( HashMap )
import qualified Dvda.HashMap as HM

-- | take a list of pair of inputs to indices which reference them
--  create a hashmap from GSyms to strings which hold the declaration
makeInputMap :: (Eq a, Hashable a, Show a)
                => [V.Vector (GExpr a Int)] -> HashMap (GExpr a Int) String
makeInputMap ins = HM.fromList $ concat $ zipWith writeInput [(0::Int)..] ins
  where
    writeInput inputK gs = zipWith f [(0::Int)..] (V.toList gs)
      where
        f inIdx g = (g, printf "input%d[%d] # %s" inputK inIdx (show g))

writeInputPrototypes :: [V.Vector a] -> [String]
writeInputPrototypes ins = concat $ zipWith inputPrototype [(0::Int)..] ins
  where
    inputPrototype inputK _ = ["input" ++ show inputK]

writeOutputs :: [V.Vector Int] -> [String]
writeOutputs ins = concat (zipWith writeOutput ins [0..]) ++ [retStatement]
  where
    retStatement =
      "return [" ++ intercalate ", " (map (("output"++) . show) [0..(length ins - 1)]) ++ "]"
    
    writeGrefList grefs = '[': intercalate ", " (map nameNode grefs) ++"]"
    
    writeOutput :: V.Vector Int -> Int -> [String]
    writeOutput grefs outputK =
      [ printf "# output %d" outputK
      , printf "output%d = %s\n" outputK (writeGrefList (V.toList grefs))
      ]

-- | Turns a FunGraph into a string containing a python function
showPy :: (Eq a, Show a, Hashable a, F.Foldable f, F.Foldable g) =>
         String-> FunGraph a f g -> String
showPy functionName fg = txt
  where
    inputs = [V.fromList $ F.toList $ fgInputs fg]
    outputs = [V.fromList $ F.toList $ fgOutputs fg]
    inPrototypes = writeInputPrototypes inputs
    outDecls = writeOutputs outputs
    inputMap = makeInputMap inputs
    mainDecls = let f k = case fgLookupGExpr fg k of
                      Just v -> pyAssignment inputMap k v
                      Nothing -> error $ "couldn't find node " ++ show k ++ " in fungraph :("
                in map f $ fgTopSort fg
  
    body = unlines $ map ("    "++) $
           mainDecls ++ [""] ++
           outDecls
  
    txt = "def " ++ functionName ++ " ( " ++ intercalate ", " inPrototypes ++ " ):\n" ++
          body

nameNode :: Int -> String
nameNode k = "v_" ++ show k

pyAssignment :: (Eq a, Hashable a, Show a) => HashMap (GExpr a Int) String -> Int -> GExpr a Int -> String
pyAssignment inputMap k g@(GSym _) = case HM.lookup g inputMap of
  Nothing -> error $ "pyAssignment: couldn't find " ++ show g ++ " in the input map"
  Just str -> nameNode k ++ " = " ++ str
pyAssignment _ k gexpr = nameNode k ++ " = " ++ toPyOp gexpr
  where
    bin :: Int -> Int -> String -> String
    bin x y op = nameNode x ++ " " ++ op ++ " " ++ nameNode y
    
    un :: Int -> String -> String
    un x op = op ++ "( " ++ nameNode x ++ " )"

    asTypeOfG :: a -> GExpr a b -> a
    asTypeOfG x _ = x
    
    toPyOp (GSym _)                       = error "toPyOp (GSym _) should be impossible"
    toPyOp (GConst c)                     = show c
    toPyOp (GNum (Mul x y))               = bin x y "*"
    toPyOp (GNum (Add x y))               = bin x y "+"
    toPyOp (GNum (Sub x y))               = bin x y "-"
    toPyOp (GNum (Negate x))              = un x "-"
    toPyOp (GNum (Abs x))                 = un x "abs"
    toPyOp (GNum (Signum x))              = un x "sign"
    toPyOp (GNum (FromInteger x))         = show x ++ ".0"
    toPyOp (GFractional (Div x y))        = bin x y "/"
    toPyOp (GFractional (FromRational x)) = show (fromRational x `asTypeOfG` gexpr)
    toPyOp (GFloating (Pow x y))          = "pow( " ++ nameNode x ++ ", " ++ nameNode y ++ " )"
    toPyOp (GFloating (LogBase x y))      = "log( " ++ nameNode y ++ ") / log( " ++ nameNode x ++ " )"
    toPyOp (GFloating (Exp x))            = un x "exp"
    toPyOp (GFloating (Log x))            = un x "log"
    toPyOp (GFloating (Sin x))            = un x "sin"
    toPyOp (GFloating (Cos x))            = un x "cos"
    toPyOp (GFloating (ASin x))           = un x "asin"
    toPyOp (GFloating (ATan x))           = un x "atan"
    toPyOp (GFloating (ACos x))           = un x "acos"
    toPyOp (GFloating (Sinh x))           = un x "sinh"
    toPyOp (GFloating (Cosh x))           = un x "cosh"
    toPyOp (GFloating (Tanh x))           = un x "tanh"
    toPyOp (GFloating (ASinh _))          = error "python generation doesn't support ASinh"
    toPyOp (GFloating (ATanh _))          = error "python generation doesn't support ATanh"
    toPyOp (GFloating (ACosh _))          = error "python generation doesn't support ACosh"
