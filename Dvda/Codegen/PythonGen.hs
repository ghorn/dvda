{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}

module Dvda.Codegen.PythonGen ( showPy
                              ) where

import Data.Hashable ( Hashable )
import Data.List ( intercalate )
import Text.Printf ( printf )

import Dvda.Expr ( GExpr(..), Floatings(..), Nums(..), Fractionals(..) )
import Dvda.FunGraph ( FunGraph, topSort, fgInputs, fgOutputs, fgLookupGExpr )
import Dvda.HashMap ( HashMap )
import qualified Dvda.HashMap as HM
import Dvda.HList ( MVS(..), MVSList(toMVSList) )

-- | take a list of pair of inputs to indices which reference them
--  create a hashmap from GSyms to strings which hold the declaration
makeInputMap :: (Eq a, Hashable a, Show a)
                => [MVS (GExpr a Int)] -> HashMap (GExpr a Int) String
makeInputMap ins = HM.fromList $ concat $ zipWith writeInput [(0::Int)..] ins
  where
    writeInput inputK (Sca g) = [(g, printf "input%d # %s" inputK (show g))]
    writeInput inputK (Vec gs) = zipWith f [(0::Int)..] gs
      where
        f inIdx g = (g, printf "input%d[%d] # %s" inputK inIdx (show g))
    writeInput inputK (Mat gs)
      | any ((ncols /=) . length) gs =
          error $ "makeInputMap [[GraphRef]] matrix got inconsistent column dimensions: "++ show (map length gs)
      | otherwise = zipWith f [(r,c) | r <- [0..(nrows-1)], c <- [0..(ncols-1)]] (concat gs)
      where
        nrows = length gs
        ncols = if nrows == 0 then 0 else length (head gs)
        f (rowIdx,colIdx) g = (g,printf "input%d[%d][%d] # %s" inputK rowIdx colIdx (show g))

writeInputPrototypes :: [MVS a] -> [String]
writeInputPrototypes ins = concat $ zipWith inputPrototype [(0::Int)..] ins
  where
    inputPrototype inputK (Sca _) = ["input" ++ show inputK]
    inputPrototype inputK (Vec _) = ["input" ++ show inputK]
    inputPrototype inputK (Mat gs)
      | any ((ncols /=) . length) gs =
          error $ "writeInputPrototypes [[GraphRef]] matrix got inconsistent column dimensions: "++ show (map length gs)
      | otherwise = ["input" ++ show inputK]
      where
        nrows = length gs
        ncols = if nrows == 0 then 0 else length (head gs)

writeOutputs :: [MVS Int] -> [String]
writeOutputs ins = (concat $ zipWith writeOutput ins [0..]) ++ [retStatement]
  where
    retStatement =
      "return [" ++ (intercalate ", " (map (("output"++) . show) [0..(length ins - 1)])) ++ "]"
    
    writeGrefList grefs = '[':(intercalate ", " (map nameNode grefs))++"]"
    
    writeOutput :: MVS Int -> Int -> [String]
    writeOutput (Sca gref) outputK = 
      [ printf "# output %d" outputK
      , printf "output%d = %s\n" outputK (nameNode gref)
      ]
    writeOutput (Vec grefs) outputK =
      [ printf "# output %d" outputK
      , printf "output%d = %s\n" outputK (writeGrefList grefs)
      ]
    writeOutput (Mat grefs) outputK
      | any ((ncols /=) . length) grefs =
          error $ "writeOutputs [[GraphRef]] matrix got inconsistent column dimensions: "++ show (map length grefs)
      | otherwise =
          [ printf "# output %d" outputK
          , printf "output%d = %s\n" outputK ('[':(intercalate ", " (map writeGrefList grefs))++"]")
          ]
      where
        nrows = length grefs
        ncols = if nrows == 0 then 0 else length (head grefs)

-- | Turns a FunGraph into a string containing a python function
showPy :: (Eq a, Show a, Hashable a, MVSList f (GExpr a Int), MVSList g Int) =>
         String-> FunGraph a f g -> String
showPy functionName fg = txt
  where
    inPrototypes = writeInputPrototypes (toMVSList $ fgInputs fg)
    outDecls = writeOutputs (toMVSList $ fgOutputs fg)
    inputMap = makeInputMap (toMVSList $ fgInputs fg)
    mainDecls = let f k = case fgLookupGExpr fg k of
                      Just v -> pyAssignment inputMap k v
                      Nothing -> error $ "couldn't find node " ++ show k ++ " in fungraph :("
                in map f $ reverse $ topSort fg
  
    body = unlines $ map ("    "++) $
           mainDecls ++ [""] ++
           outDecls
  
    txt = "def " ++ functionName ++ " ( " ++ (intercalate ", " (inPrototypes)) ++ " ):\n" ++
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
