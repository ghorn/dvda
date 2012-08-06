{-# OPTIONS_GHC -Wall #-}

module Dvda.MultipleShooting.CoctaveTemplates ( writeMexAll
                                              , writeSetupSource
                                              , writeUnstructConsts
                                              , writeToStruct
                                              , writeUnstruct
                                              , writePlot
                                              )where

import Data.Maybe ( fromMaybe )
import Data.Hashable ( Hashable )
import Data.List ( elemIndex, transpose )

import Dvda.Expr ( Expr(..), Sym(..) )
import Dvda.HashMap ( HashMap )
import qualified Dvda.HashMap as HM

writeMexAll :: String -> String
writeMexAll name = unlines $ map f ["time", "outputs", "sim", "cost", "constraints"]
  where
    f x = "tic\nfprintf('mexing " ++ file ++ "...  ')\n"++"mex " ++ file ++ "\nt1 = toc;\nfprintf('finished in %.2f seconds\\n', t1)"
      where
        file = name ++ "_" ++ x ++ ".c"


writeSetupSource :: Show a => String -> [Expr a] -> [a] -> [a] -> String
writeSetupSource name dvs lbs ubs =
  unlines $
  [ "function [x0, Aineq, bineq, Aeq, beq, lb, ub] = "++ name ++"_setup()"
  , ""
  , "x0 = zeros(" ++ show (length dvs) ++ ",1);"
  , "Aineq = [];"
  , "bineq = [];"
  , "Aeq = [];"
  , "beq = [];"
  , "lb = " ++ show lbs ++ "';"
  , "ub = " ++ show ubs ++ "';"
  ]


-- take nice matlab structs and return vector of design constants
writeUnstructConsts :: Eq a => String -> [Expr a] -> String
writeUnstructConsts name constants =
  unlines $
  [ "function constants = " ++ name ++ "_unstructConstants(constStruct)\n"
  , "constants = zeros(" ++ show (length constants) ++ ", 1);"
  , ""
  , concatMap fromConst constants
  ]
  where
    readName e = case e of
      ESym (Sym nm) -> nm
      _ -> error "const not ESym Sym"
    fromConst e = "constants(" ++ show (1 + (fromJustErr "fromConst error" $ e `elemIndex` constants)) ++ ") = constStruct." ++ readName e ++ ";\n"


---- take vector of design variables and vector of constants and return nice matlab struct
writeToStruct :: (Eq a, Show a, Hashable a)
                 => String -> [Expr a] -> [Expr a] -> [Expr a] -> HashMap String [Expr a] -> String
writeToStruct name dvs params constants outputMap =
  unlines $
  ["function ret = " ++ name ++ "_struct(designVars,constants)"
  , ""
  , "ret.time = " ++ name ++ "_time(designVars, constants);"
  , "outs = " ++ name ++ "_outputs(designVars, constants);"
  , concat $ zipWith (\name' k -> "ret." ++name'++ " = outs("++show k++",:);\n") (HM.keys outputMap) [(1::Int)..]
  ] ++
  toStruct dvs "designVars" (map show params) (map (\x -> [x]) params) ++
  toStruct constants "constants" (map show constants) (map (\x -> [x]) constants)
    where
      dvsToIdx dvs' = (fromJustErr "toStruct error") . (flip HM.lookup (HM.fromList (zip dvs' [(1::Int)..])))

      toStruct dvs' nm = zipWith (\name' vars -> "ret." ++ name' ++ " = " ++ nm ++ "(" ++ show (map (dvsToIdx dvs') vars) ++ ");\n")

-- take nice matlab structs and return vector of design variables
writeUnstruct :: (Eq a, Show a)
                 => String
                 -> [Expr a] -> [Expr a]
                 -> [Expr a] -> [[Expr a]]
                 -> [Expr a] -> [[Expr a]]
                 -> String
writeUnstruct name dvs params states allStates actions allActions =
  unlines $
  [ "function dvs = " ++ name ++ "_unstruct(dvStruct)\n"
  , "dvs = zeros(" ++ show (length dvs) ++ ", 1);"
  , ""
  , concatMap fromParam params
  , concat $ zipWith fromXU states  (transpose allStates)
  , concat $ zipWith fromXU actions (transpose allActions)
  ]
  where
    dvIdx e = fromMaybe (error $ "dvIdx error - " ++ show e ++ " is not a design variable")
              (e `elemIndex` dvs)
    fromParam e = "dvs(" ++ show (1 + dvIdx e) ++ ") = dvStruct." ++ show e ++ ";\n"
    fromXU e es =
      "dvs(" ++ show (map ((1 +) . dvIdx) es) ++ ") = dvStruct." ++ show e ++ ";\n"

writePlot :: String -> HashMap String [Expr a] -> String
writePlot name outputMap =
  unlines $
  [ "function " ++ name ++ "_plot(designVars, constants)\n"
  , "x = " ++ name ++ "_struct(designVars, constants);\n"
  , init $ unlines $ zipWith f (HM.keys outputMap) [(1::Int)..]
  ]
  where
    rows = ceiling $ sqrt $ (fromIntegral ::Int -> Double) $ HM.size outputMap
    cols = (HM.size outputMap `div` rows) + 1
    f name' k = unlines $
                [ "subplot(" ++ show rows ++ "," ++ show cols ++ ","++show k++");"
                , "plot( x.time, x." ++ name' ++ " );"
                , "xlabel('time');"
                , "ylabel('" ++ name'' ++ "');"
                , "title('"  ++ name'' ++ "');"
                ]
      where
        name'' = foldl (\acc x -> if x == '_' then acc ++ "\\_" else acc ++ [x]) "" name'


fromJustErr :: String -> Maybe a -> a
fromJustErr _ (Just x) = x
fromJustErr message Nothing = error $ "fromJustErr got Nothing, message: \"" ++ message ++ "\""
