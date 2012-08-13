{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}

module Dvda.CGen ( showC
                 , showMex
                 , MatrixStorageOrder(..)
                 ) where


import Data.Hashable ( Hashable )
import Data.List ( intercalate )
import Text.Printf ( printf )

import Dvda.Expr ( GExpr(..), Floatings(..), Nums(..), Fractionals(..) )
import Dvda.FunGraph ( FunGraph, MVS(..), topSort, fgInputs, fgOutputs, fgLookupGExpr )
import Dvda.HashMap ( HashMap )
import qualified Dvda.HashMap as HM

data MatrixStorageOrder = RowMajor | ColMajor

-- | take a list of pair of inputs to indices which reference them
--  create a hashmap from GSyms to strings which hold the declaration
makeInputMap :: (Eq a, Hashable a, Show a)
                => MatrixStorageOrder -> [MVS (GExpr a Int)] -> HashMap (GExpr a Int) String
makeInputMap matStorageOrder ins = HM.fromList $ concat $ zipWith writeInput [(0::Int)..] ins
  where
    writeInput inputK (Sca g) = [(g, printf "*input%d; /* %s */" inputK (show g))]
    writeInput inputK (Vec gs) = zipWith f [(0::Int)..] gs
      where
        f inIdx g = (g, printf "input%d[%d]; /* %s */" inputK inIdx (show g))
    writeInput inputK (Mat gs)
      | any ((ncols /=) . length) gs =
          error $ "writeInputs [[GraphRef]] matrix got inconsistent column dimensions: "++ show (map length gs)
      | otherwise = zipWith f [(r,c) | r <- [0..(nrows-1)], c <- [0..(ncols-1)]] (concat gs)
      where
        nrows = length gs
        ncols = if nrows == 0 then 0 else length (head gs)
        f (rowIdx,colIdx) g = (g,printf "input%d[%d][%d]; /* %s */" inputK fstIdx sndIdx (show g))
          where
            (fstIdx,sndIdx) = case matStorageOrder of RowMajor -> (rowIdx,colIdx)
                                                      ColMajor -> (colIdx,rowIdx)

writeInputPrototypes :: MatrixStorageOrder -> [MVS a] -> [String]
writeInputPrototypes matStorageOrder ins = concat $ zipWith inputPrototype [(0::Int)..] ins
  where
    inputPrototype inputK (Sca _) = ["const double * input" ++ show inputK]
    inputPrototype inputK (Vec gs) = ["const double input" ++ show inputK ++ "[" ++ show (length gs) ++ "]"]
    inputPrototype inputK (Mat gs)
      | any ((ncols /=) . length) gs =
          error $ "writeInputs [[GraphRef]] matrix got inconsistent column dimensions: "++ show (map length gs)
      | otherwise = ["const double input" ++ show inputK ++ "[" ++ show fstIdx ++ "][" ++ show sndIdx ++ "]"]
      where
        nrows = length gs
        ncols = if nrows == 0 then 0 else length (head gs)
        (fstIdx,sndIdx) = case matStorageOrder of RowMajor -> (nrows,ncols)
                                                  ColMajor -> (ncols,nrows)

writeOutputs :: MatrixStorageOrder -> [MVS Int] -> ([String], [String])
writeOutputs matStorageOrder ins = (concatMap fst dcs, concatMap snd dcs)
  where
    dcs :: [([String],[String])]
    dcs = zipWith writeOutput ins [0..]

    writeOutput :: MVS Int -> Int -> ([String], [String])
    writeOutput (Sca gref) outputK = (decls, prototype)
      where
        decls = [printf "/* output %d */" outputK, printf "(*output%d) = %s;" outputK (nameNode gref)]
        prototype = ["double * const output" ++ show outputK]
    writeOutput (Vec grefs) outputK = (decls, prototype)
      where
        prototype = ["double output" ++ show outputK ++ "[" ++ show (length grefs) ++ "]"]
        decls = (printf "/* output %d */" outputK):
                zipWith f [(0::Int)..] grefs
          where
            f outIdx gref = printf "output%d[%d] = %s;" outputK outIdx (nameNode gref)
    writeOutput (Mat grefs) outputK
      | any ((ncols /=) . length) grefs =
          error $ "writeOutputs [[GraphRef]] matrix got inconsistent column dimensions: "++ show (map length grefs)
      | otherwise = (decls, prototype)
      where
        nrows = length grefs
        ncols = if nrows == 0 then 0 else length (head grefs)
        prototype = ["double output" ++ show outputK ++ "[" ++ show fstIdx ++ "][" ++ show sndIdx ++ "]"]
          where
            (fstIdx,sndIdx) = case matStorageOrder of RowMajor -> (nrows,ncols)
                                                      ColMajor -> (ncols,nrows)
        decls = (printf "/* output %d */" outputK):
                zipWith f [(r,c) | r <- [0..(nrows-1)], c <- [0..(ncols-1)]] (concat grefs)
          where
            f (rowIdx,colIdx) gref = printf "output%d[%d][%d] = %s;" outputK fstIdx sndIdx (nameNode gref)
              where
                (fstIdx,sndIdx) = case matStorageOrder of RowMajor -> (rowIdx,colIdx)
                                                          ColMajor -> (colIdx,rowIdx)


createMxOutputs :: [MVS Int] -> [String]
createMxOutputs xs = concat $ zipWith createMxOutput xs [0..]
  where
    createMxOutput :: MVS Int -> Int -> [String]
    createMxOutput (Sca _) outputK =
      [ "    if ( " ++ show outputK ++ " < nlhs ) {"
      , "        plhs[" ++ show outputK ++ "] = mxCreateDoubleScalar( 0 );"
      , "        outputs[" ++ show outputK ++ "] = mxGetPr( plhs[" ++ show outputK ++ "] );"
      , "    } else"
      , "        outputs[" ++ show outputK ++ "] = (double*)malloc( sizeof(double) );"
      ]
    createMxOutput (Vec grefs) outputK =
      [ "    if ( " ++ show outputK ++ " < nlhs ) {"
      , "        plhs[" ++ show outputK ++ "] = mxCreateDoubleMatrix( " ++ show (length grefs) ++ ", 1, mxREAL );"
      , "        outputs[" ++ show outputK ++ "] = mxGetPr( plhs[" ++ show outputK ++ "] );"
      , "    } else"
      , "        outputs[" ++ show outputK ++ "] = (double*)malloc( " ++ show (length grefs) ++ "*sizeof(double) );"
      ]
    createMxOutput (Mat grefs) outputK =
      [ "    if ( " ++ show outputK ++ " < nlhs ) {"
      , "        plhs[" ++ show outputK ++ "] = mxCreateDoubleMatrix( " ++ show nrows++ ", " ++ show ncols ++ ", mxREAL );"
      , "        outputs[" ++ show outputK ++ "] = mxGetPr( plhs[" ++ show outputK ++ "] );"
      , "    } else"
      , "        outputs[" ++ show outputK ++ "] = (double*)malloc( " ++ show (nrows*ncols) ++ "*sizeof(double) );"
      ]
      where
        nrows = length grefs
        ncols = if nrows == 0 then 0 else length (head grefs)


checkMxInputDims :: MVS a -> String -> Int -> [String]
checkMxInputDims (Sca _) functionName inputK =
  [ "    if ( 1 != mxGetM( prhs[" ++ show inputK ++ "] ) || 1 != mxGetN( prhs[" ++ show inputK ++ "] ) ) {"
  , "        char errMsg[200];"
  , "        sprintf(errMsg,"
  , "                \"mex function '" ++ functionName ++ "' got incorrect dimensions for input " ++ show (1+inputK) ++ "\\n\""
  , "                \"expected dimensions: (1, 1) but got (%zu, %zu)\","
  , "                mxGetM( prhs[" ++ show inputK ++ "] ),"
  , "                mxGetN( prhs[" ++ show inputK ++ "] ) );"
  , "        mexErrMsgTxt(errMsg);"
  , "    }"
  ]
checkMxInputDims (Vec grefs) functionName inputK =
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
    nrows = length grefs
checkMxInputDims (Mat grefs) functionName inputK =
  [ "    if ( " ++ show nrows ++ " != mxGetM( prhs[" ++ show inputK ++ "] ) || " ++ show ncols ++ " != mxGetN( prhs[" ++ show inputK ++ "] ) ) {"
  , "        char errMsg[200];"
  , "        sprintf(errMsg,"
  , "                \"mex function '" ++ functionName ++ "' got incorrect dimensions for input " ++ show (1+inputK) ++ "\\n\""
  , "                \"expected dimensions: (" ++ show nrows ++ ", " ++ show ncols ++ ") but got (%zu, %zu)\","
  , "                mxGetM( prhs[" ++ show inputK ++ "] ),"
  , "                mxGetN( prhs[" ++ show inputK ++ "] ) );"
  , "        mexErrMsgTxt(errMsg);"
  , "    }"
  ]
  where
    nrows = length grefs
    ncols = if nrows == 0 then 0 else length (head grefs)


-- | Turns a FunGraph into a string containing C code
showC :: (Eq a, Show a, Hashable a) => MatrixStorageOrder -> String -> FunGraph a -> String
showC matStorageOrder functionName fg = txt
  where
    inPrototypes = writeInputPrototypes matStorageOrder (fgInputs fg)
    (outDecls, outPrototypes) = writeOutputs matStorageOrder (fgOutputs fg)
    inputMap = makeInputMap matStorageOrder (fgInputs fg)
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


showMex :: (Eq a, Show a, Hashable a) => String -> FunGraph a -> String
showMex functionName fg = cText ++ "\n\n\n" ++ mexFun functionName (fgInputs fg) (fgOutputs fg)
  where
    cText = showC ColMajor functionName fg -- matlab is column major >_<

mexFun :: String -> [MVS a] -> [MVS Int] -> String
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
    inputPtrs  = zipWith (\i k -> cast i "const " ++ "mxGetPr(prhs[" ++ show k ++ "])") ins  [(0::Int)..]
    outputPtrs = zipWith (\o k -> cast o    ""    ++ "(outputs[" ++ show k ++ "])")     outs [(0::Int)..]

    cast :: MVS a -> String -> String
    cast (Sca _) _ = ""
    cast (Vec _) _ = ""
    cast (Mat xs) cnst = "(" ++ cnst ++ "double (*)[" ++ show nrows ++ "])" -- column major order
      where
        nrows = length xs
