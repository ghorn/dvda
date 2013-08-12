{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}

module Dvda.Codegen.CGen ( showC
                         , showMex
                         ) where

import Data.Hashable ( Hashable )
import qualified Data.Foldable as F
import Data.List ( intercalate )
import qualified Data.Vector as V
import Text.Printf ( printf )

import Dvda.Expr ( GExpr(..), Floatings(..), Nums(..), Fractionals(..) )
import Dvda.FunGraph ( FunGraph, topSort, fgInputs, fgOutputs, fgLookupGExpr )
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
        f inIdx g = (g, printf "input%d[%d]; /* %s */" inputK inIdx (show g))

writeInputPrototypes :: [V.Vector a] -> [String]
writeInputPrototypes ins = concat $ zipWith inputPrototype [(0::Int)..] ins
  where
    inputPrototype inputK gs =
      ["const double input" ++ show inputK ++ "[" ++ show (V.length gs) ++ "]"]

writeOutputs :: [V.Vector Int] -> ([String], [String])
writeOutputs ins = (concatMap fst dcs, concatMap snd dcs)
  where
    dcs :: [([String],[String])]
    dcs = zipWith writeOutput ins [0..]

    writeOutput :: V.Vector Int -> Int -> ([String], [String])
    writeOutput grefs outputK = (decls, prototype)
      where
        prototype = ["double output" ++ show outputK ++ "[" ++ show (V.length grefs) ++ "]"]
        decls = (printf "/* output %d */" outputK):
                zipWith f [(0::Int)..] (V.toList grefs)
          where
            f outIdx gref = printf "output%d[%d] = %s;" outputK outIdx (nameNode gref)

createMxOutputs :: [V.Vector Int] -> [String]
createMxOutputs xs = concat $ zipWith createMxOutput xs [0..]
  where
    createMxOutput :: V.Vector Int -> Int -> [String]
    createMxOutput grefs outputK =
      [ "    if ( " ++ show outputK ++ " < nlhs ) {"
      , "        plhs[" ++ show outputK ++ "] = mxCreateDoubleMatrix( " ++ show (V.length grefs) ++ ", 1, mxREAL );"
      , "        outputs[" ++ show outputK ++ "] = mxGetPr( plhs[" ++ show outputK ++ "] );"
      , "    } else"
      , "        outputs[" ++ show outputK ++ "] = (double*)malloc( " ++ show (V.length grefs) ++ "*sizeof(double) );"
      ]

checkMxInputDims :: V.Vector a -> String -> Int -> [String]
checkMxInputDims grefs functionName inputK =
  [ "    if ( !( " ++ show nrows ++ " == mxGetM( prhs[" ++ show inputK ++ "] ) && 1 == mxGetN( prhs[" ++ show inputK ++ "] ) ) && !( " ++ show nrows ++ " == mxGetN( prhs[" ++ show inputK ++ "] ) && 1 == mxGetM( prhs[" ++ show inputK ++ "] ) ) ) {"
  , "        char errMsg[200];"
  , "        sprintf(errMsg,"
  , "                \"mex function '" ++ functionName ++ "' got incorrect dimensions for input " ++ show (1+inputK) ++ "\\n\""
  , "                \"expected dimensions: (" ++ show nrows ++ ", 1) or (1, " ++ show nrows ++ ") but got (%zu, %zu)\","
  , "                mxGetM( prhs[" ++ show inputK ++ "] ),"
  , "                mxGetN( prhs[" ++ show inputK ++ "] ) );"
  , "        mexErrMsgTxt(errMsg);"
  , "    }"
  ]
  where
    nrows = V.length grefs

-- | Turns a FunGraph into a string containing C code
showC :: (Eq a, Show a, Hashable a, F.Foldable f, F.Foldable g) =>
         String-> FunGraph a f g -> String
showC functionName fg = txt
  where
    inputs = [V.fromList $ F.toList (fgInputs fg)]
    outputs = [V.fromList $ F.toList (fgOutputs fg)]
    inPrototypes = writeInputPrototypes inputs
    (outDecls, outPrototypes) = writeOutputs outputs
    inputMap = makeInputMap inputs
    mainDecls = let f k = case fgLookupGExpr fg k of
                      Just v -> cAssignment inputMap k v
                      Nothing -> error $ "couldn't find node " ++ show k ++ " in fungraph :("
                in map f $ reverse $ topSort fg
  
    body = unlines $ map ("    "++) $
           mainDecls ++ [""] ++
           outDecls
  
    txt = "#include <math.h>\n\n" ++
          "void " ++ functionName ++ " ( " ++ (intercalate ", " (inPrototypes++outPrototypes)) ++ " )\n{\n" ++
          body ++ "}\n"

nameNode :: Int -> String
nameNode k = "v_" ++ show k

cAssignment :: (Eq a, Hashable a, Show a) => HashMap (GExpr a Int) String -> Int -> GExpr a Int -> String
cAssignment inputMap k g@(GSym _) = case HM.lookup g inputMap of
  Nothing -> error $ "cAssignment: couldn't find " ++ show g ++ " in the input map"
  Just str -> "const double " ++ nameNode k ++ " = " ++ str
cAssignment _ k gexpr = "const double " ++ nameNode k ++ " = " ++ toCOp gexpr ++ ";"
  where
    bin :: Int -> Int -> String -> String
    bin x y op = nameNode x ++ " " ++ op ++ " " ++ nameNode y
    
    un :: Int -> String -> String
    un x op = op ++ "( " ++ nameNode x ++ " )"

    asTypeOfG :: a -> GExpr a b -> a
    asTypeOfG x _ = x
    
    toCOp (GSym _)                       = error "toCOp (GSym _) should be impossible"
    toCOp (GConst c)                     = show c
    toCOp (GNum (Mul x y))               = bin x y "*"
    toCOp (GNum (Add x y))               = bin x y "+"
    toCOp (GNum (Sub x y))               = bin x y "-"
    toCOp (GNum (Negate x))              = un x "-"
    toCOp (GNum (Abs x))                 = un x "abs"
    toCOp (GNum (Signum x))              = un x "sign"
    toCOp (GNum (FromInteger x))         = show x
    toCOp (GFractional (Div x y))        = bin x y "/"
    toCOp (GFractional (FromRational x)) = show (fromRational x `asTypeOfG` gexpr)
    toCOp (GFloating (Pow x y))          = "pow( " ++ nameNode x ++ ", " ++ nameNode y ++ " )"
    toCOp (GFloating (LogBase x y))      = "log( " ++ nameNode y ++ ") / log( " ++ nameNode x ++ " )"
    toCOp (GFloating (Exp x))            = un x "exp"
    toCOp (GFloating (Log x))            = un x "log"
    toCOp (GFloating (Sin x))            = un x "sin"
    toCOp (GFloating (Cos x))            = un x "cos"
    toCOp (GFloating (ASin x))           = un x "asin"
    toCOp (GFloating (ATan x))           = un x "atan"
    toCOp (GFloating (ACos x))           = un x "acos"
    toCOp (GFloating (Sinh x))           = un x "sinh"
    toCOp (GFloating (Cosh x))           = un x "cosh"
    toCOp (GFloating (Tanh x))           = un x "tanh"
    toCOp (GFloating (ASinh _))          = error "C generation doesn't support ASinh"
    toCOp (GFloating (ATanh _))          = error "C generation doesn't support ATanh"
    toCOp (GFloating (ACosh _))          = error "C generation doesn't support ACosh"


showMex :: (Eq a, Show a, Hashable a, F.Foldable f, F.Foldable g)
           => String -> FunGraph a f g -> String
showMex functionName fg = cText ++ "\n\n\n" ++ mexFun functionName [V.fromList $ F.toList $ fgInputs fg] [V.fromList $ F.toList $ fgOutputs fg]
  where
    cText = showC functionName fg -- matlab is column major >_<

mexFun :: String -> [V.Vector a] -> [V.Vector Int] -> String
mexFun functionName ins outs =
  unlines $
  [ "#include \"mex.h\""
  , []
  , "void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])"
  , "{"
  , "    /* check number of inputs  */"
  , "    if ( " ++ show nrhs ++ " != nrhs ) {"
  , "        char errMsg[200];"
  , "        sprintf(errMsg,"
  , "                \"mex function '" ++ functionName ++ "' given incorrect number of inputs\\n\""
  , "                \"expected: " ++ show nrhs ++ " but got %d\","
  , "                nrhs);"
  , "        mexErrMsgTxt(errMsg);"
  , "    }"
  , []
  , "    /* check the dimensions of the input arrays */"
  ] ++ concat (zipWith (\x -> checkMxInputDims x functionName) ins [0..]) ++
  [ []
  , "    /* check number of outputs  */"
  , "    if ( " ++ show nlhs ++ " < nlhs ) {"
  , "        char errMsg[200];"
  , "        sprintf(errMsg,"
  , "                \"mex function '" ++ functionName ++ "' saw too many outputs\\n\""
  , "                \"expected <= " ++ show nlhs ++ " but got %d\","
  , "                nlhs);"
  , "        mexErrMsgTxt(errMsg);"
  , "    }"
  , []
  , "    /* create the output arrays, if no output is provided by user create a dummy output */"
  , "    double * outputs[" ++ show nlhs ++ "];"
  ] ++ createMxOutputs outs ++ -- e.g.: plhs[0] = mxCreateDoubleMatrix(1,ncols,mxREAL);
  [ []
  , "    /* call the c function */"
  , "    " ++ functionName ++ "( " ++ intercalate ", " (inputPtrs ++ outputPtrs) ++ " );"
  , []
  , "    /* free the unused dummy outputs */"
  , "    int k;"
  , "    for ( k = " ++ show (nlhs - 1) ++ "; nlhs <= k; k-- )"
  , "        free( outputs[k] );"
  , "}"
  ]
  where
    nlhs = length outs
    nrhs = length ins
    inputPtrs  = map (\k -> "const " ++ "mxGetPr(prhs[" ++ show k ++ "])") [(0::Int)..]
    outputPtrs = map (\k ->    ""    ++ "(outputs[" ++ show k ++ "])")     [(0::Int)..]
