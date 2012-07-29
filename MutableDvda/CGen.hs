{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}

module MutableDvda.CGen ( showC
                        , showMex
                        , showMex'
                        ) where

import Data.Hashable ( Hashable )
import Data.Maybe ( catMaybes )
import Data.List ( intercalate )
import Text.Printf ( printf )

import MutableDvda.Expr
import MutableDvda.FunGraph

import Dvda.Codegen ( writeSourceFile )

run :: IO ()
run = do
  let x = sym "x" :: Expr Double
      y = sym "y"
      z = sym "z"
      w = sym "w"
      w1 = sym "w1"
      w2 = sym "w2"
      w3 = sym "w3"
      f0 = x*y + z + w1 + w2
      f2 = f0 * w2/w3
      
      f1 = [f0/2, f0*y, w, 0.0, 0]
      boo = x

      inputs = boo :* [y]:*[[z]] :* [w3,w1,w2,w]
      outputs = f0:*f1:*f2:*[[f0*f0]]

--  showC "foo" (boo :* [y]:*[[z]]) (f0:*f1:*f2:*[[f0*f0]]) >>= putStrLn

  fg' <- toFunGraph inputs outputs
  putStrLn $ "cost has " ++ show (countNodes fg') ++ " nodes"
  fg <- toFunGraph inputs outputs
  putStrLn $ "cost has " ++ show (countNodes fg) ++ " nodes"
  previewGraph fg

  mexSrc <- showMex "foo" inputs outputs -- (x :* [y,w3]:*[[z,w], [w1,w2]]) (f0:*f1:*[[f0*f0]]:*[f1])
  _ <- writeSourceFile mexSrc "../Documents/MATLAB" $ "foo.c"
  return ()

writeInputs :: [MVS (String,Maybe Int)] -> ([String], [String])
writeInputs ins = (concatMap fst dcs, concatMap snd dcs)
  where
    dcs :: [([String],[String])]
    dcs = zipWith writeInput ins [0..]
    
    writeInput :: MVS (String,Maybe Int) -> Int -> ([String], [String])
    writeInput (Sca k') inputK = ([printf "/* input %d */" inputK, decl], prototype)
      where
        prototype = ["const double * input" ++ show inputK]
        decl = case k' of
          (name,Just k) -> printf "const double %s = *input%d; /* %s */" (nameNode k) inputK name
          (name,_) -> printf "/* *input%d; unused ( %s )*/" inputK name
    writeInput (Vec ks) inputK = (decls, prototype)
      where
        prototype = ["const double input" ++ show inputK ++ "[" ++ show (length ks) ++ "]"]
        decls = (printf "/* input %d */" inputK):(zipWith f [(0::Int)..] ks)
          where
            f inIdx (name,Just k) = printf "const double %s = input%d[%d]; /* %s */" (nameNode k) inputK inIdx name
            f inIdx (name,_) = printf "/* input%d[%d] unused ( %s ) */" inputK inIdx name
    writeInput (Mat ks) inputK
      | any ((ncols /=) . length) ks =
          error $ "writeInputs [[GraphRef]] matrix got inconsistent column dimensions: "++ show (map length ks)
      | otherwise = (decls, prototype)
      where
        nrows = length ks
        ncols = if nrows == 0 then 0 else length (head ks)
        prototype = ["const double input" ++ show inputK ++ "[" ++ show nrows ++ "][" ++ show ncols ++ "]"]
        decls = (printf "/* input %d */" inputK):
                zipWith f [(r,c) | r <- [0..(nrows-1)], c <- [0..(ncols-1)]] (concat ks)
          where
            f (rowIdx,colIdx) (name,Just k) = printf "const double %s = input%d[%d][%d]; /* %s */"
                                              (nameNode k) inputK rowIdx colIdx name
            f (rowIdx,colIdx) (name,_) = printf "/* input%d[%d][%d]; unused ( %s ) */"
                                         inputK rowIdx colIdx name


writeOutputs :: [MVS Int] -> ([String], [String])
writeOutputs ins = (concatMap fst dcs, concatMap snd dcs)
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
        prototype = ["double output" ++ show outputK ++ "[" ++ show nrows ++ "][" ++ show ncols ++ "]"]
        decls = (printf "/* output %d */" outputK):
                zipWith f [(r,c) | r <- [0..(nrows-1)], c <- [0..(ncols-1)]] (concat grefs)
          where
            f (rowIdx,colIdx) gref = printf "output%d[%d][%d] = %s;" outputK rowIdx colIdx (nameNode gref)

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


-- | Turns inputs and outputs into a string containing C code.
-- .
--   Also pass a name to give to the C function
-- .
--   This function simply calls showCWithFunGraph and discards the first two outputs
showC :: (Eq a, Show a, Hashable a, NumT b ~ a, NumT c ~ a, ToFunGraph b, ToFunGraph c)
         => String -> b -> c -> IO String
showC functionName inputs outputs = do
  (txt,_) <- showCWithFunGraph functionName inputs outputs
  return txt

-- | Turns inputs and outputs into a string containing C code. Also return indices of the inputs and outputs
-- .
--   Also pass a name to give to the C function
showCWithFunGraph :: (Eq a, Show a, Hashable a, NumT b ~ a, NumT c ~ a, ToFunGraph b, ToFunGraph c)
                     => String -> b -> c -> IO (String, FunGraph a)
showCWithFunGraph functionName inputs outputs = do
  fg <- toFunGraph inputs outputs
  let (inDecls, inPrototypes) = writeInputs (fgInputs fg)
      (outDecls, outPrototypes) = writeOutputs (fgOutputs fg)
      mainDecls = catMaybes $ map (\k -> fgLookupGExpr fg k >>= cAssignment k) $ reverse $ topSort fg
  
      body = unlines $ map ("    "++) $
             inDecls ++
             ["","/* body */"] ++
             mainDecls ++ [""] ++
             outDecls
  
      txt = "#include <math.h>\n\n" ++
            "void " ++ functionName ++ " ( " ++ (intercalate ", " (inPrototypes++outPrototypes)) ++ " )\n{\n" ++
            body ++ "}\n"
  return (txt, fg)

nameNode :: Int -> String
nameNode k = "v_" ++ show k

cAssignment :: Show a => Int -> GExpr a Int -> Maybe String
cAssignment k gexpr = fmap (\cop -> "const double " ++ nameNode k ++ " = " ++ cop ++ ";") (toCOp gexpr)
  where
    bin :: Int -> Int -> String -> String
    bin x y op = nameNode x ++ " " ++ op ++ " " ++ nameNode y
    
    un :: Int -> String -> String
    un x op = op ++ "( " ++ nameNode x ++ " )"

    asTypeOfG :: a -> GExpr a b -> a
    asTypeOfG x _ = x
    
    toCOp (GSym _)                       = Nothing
    toCOp (GConst c)                     = Just $ show c
    toCOp (GNum (Mul x y))               = Just $ bin x y "*"
    toCOp (GNum (Add x y))               = Just $ bin x y "+"
    toCOp (GNum (Sub x y))               = Just $ bin x y "-"
    toCOp (GNum (Negate x))              = Just $ un x "-"
    toCOp (GNum (Abs x))                 = Just $ un x "abs"
    toCOp (GNum (Signum x))              = Just $ un x "sign"
    toCOp (GNum (FromInteger x))         = Just $ show x
    toCOp (GFractional (Div x y))        = Just $ bin x y "/"
    toCOp (GFractional (FromRational x)) = Just $ show (fromRational x `asTypeOfG` gexpr)
    toCOp (GFloating (Pow x y))          = Just $ "pow( " ++ nameNode x ++ ", " ++ nameNode y ++ " )"
    toCOp (GFloating (LogBase x y))      = Just $ "log( " ++ nameNode y ++ ") / log( " ++ nameNode x ++ " )"
    toCOp (GFloating (Exp x))            = Just $ un x "exp"
    toCOp (GFloating (Log x))            = Just $ un x "log"
    toCOp (GFloating (Sin x))            = Just $ un x "sin"
    toCOp (GFloating (Cos x))            = Just $ un x "cos"
    toCOp (GFloating (ASin x))           = Just $ un x "asin"
    toCOp (GFloating (ATan x))           = Just $ un x "atan"
    toCOp (GFloating (ACos x))           = Just $ un x "acos"
    toCOp (GFloating (Sinh x))           = Just $ un x "sinh"
    toCOp (GFloating (Cosh x))           = Just $ un x "cosh"
    toCOp (GFloating (Tanh x))           = Just $ un x "tanh"
    toCOp (GFloating (ASinh _))          = error "C generation doesn't support ASinh"
    toCOp (GFloating (ATanh _))          = error "C generation doesn't support ATanh"
    toCOp (GFloating (ACosh _))          = error "C generation doesn't support ACosh"


showMex :: (Eq a, Show a, Hashable a, ToFunGraph b, ToFunGraph c, NumT b ~ a, NumT c ~ a)
           => String -> b -> c -> IO String
showMex functionName inputs outputs = fmap fst (showMex' functionName inputs outputs)

showMex' :: (Eq a, Show a, Hashable a, ToFunGraph b, ToFunGraph c, NumT b ~ a, NumT c ~ a)
            => String -> b -> c -> IO (String, FunGraph a)
showMex' functionName inputs outputs = do
  (cText, fg) <- showCWithFunGraph functionName inputs outputs
  return (cText ++ "\n\n\n" ++ mexFun functionName (fgInputs fg) (fgOutputs fg), fg)

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
    cast (Mat []) cnst = "(" ++ cnst ++ "double (*)[0])"
    cast (Mat xs) cnst = "(" ++ cnst ++ "double (*)[" ++ show ncols ++ "])"
      where
        ncols = length (head xs)
