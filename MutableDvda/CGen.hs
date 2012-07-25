{-# OPTIONS_GHC -Wall #-}
{-# Language TypeOperators #-}
{-# Language FlexibleInstances #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}

module MutableDvda.CGen ( GenC(..)
                        , showC
                        , showMex
                        , run
                        ) where

import Data.Hashable ( Hashable )
import Data.Maybe ( catMaybes )
import Data.List ( intercalate )
import Text.Printf ( printf )

import MutableDvda.Expr
import MutableDvda.Graph

run :: IO ()
run = do
  let x = sym "x" :: Expr Double
      y = sym "y"
      z = sym "z"
      w = sym "w"
      w1 = sym "w1"
      w2 = sym "w2"
      f0 = x*y + z + w1 + w2
      
      f1 = [f0/2, f0*y, w]
--  showC "foo" (x :* [y]:*[[z]]) (f0:*f1:*[[f0*f0]]) >>= putStrLn
  showMex "foo" (x :* [y]:*[[z,w], [w1,w2]]) (f0:*f1:*[[f0*f0]]:*[f1]) >>= putStrLn

-------------------------------------------------------------------------
class GenC a where
  numObjects :: a -> Int
  writeOutputs :: a -> Int -> ([String], [String]) -- (output declarations, prototype)
  writeInputs :: a -> Int -> ([String], [String]) -- (input declarations, prototype)
  -- mex function stuff
  createMxOutputs :: a -> Int -> [String]
  checkMxInputShape :: a -> String -> Int -> [String]
  checkMxInputDims :: a -> String -> Int -> [String]

instance GenC GraphRef where
  numObjects _ = 1
  writeOutputs gref outputK = (decls, prototype)
    where
      decls = [printf "/* output %d */" outputK, printf "(*output%d) = %s;" outputK (nameNode gref)]
      prototype = ["double * const output" ++ show outputK]
  writeInputs gref inputK = (decls, prototype)
    where
      decls = [printf "/* input %d */" inputK, printf "const double %s = *input%d;" (nameNode gref) inputK]
      prototype = ["const double * input" ++ show inputK]
  createMxOutputs _ outputK = ["    plhs[" ++ show outputK ++ "] = mxCreateDoubleScalar( 0 );"]
  checkMxInputShape _ functionName inputK =
    [ "    if ( 0 != mxGetNumberOfDimensions( prhs[" ++ show inputK ++ "] ) ) {"
    , "        char errMsg[200];"
    , "        sprintf(errMsg,"
    , "                \"mex function '" ++ functionName ++ "' got incorrect shape for input " ++ show inputK ++ "\\n\""
    , "                \"expected 0 dimensions but got %d\\n\","
    , "                mxGetNumberOfDimensions( prhs[" ++ show inputK ++ "] ) );"
    , "        mexErrMsgTxt(errMsg);"
    , "    }"
    ]
  checkMxInputDims _ _ _ = []

instance GenC [GraphRef] where
  numObjects _ = 1
  writeOutputs grefs outputK = (decls, prototype)
    where
      prototype = ["double output" ++ show outputK ++ "[" ++ show (length grefs) ++ "]"]
      decls = (printf "/* output %d */" outputK):
              zipWith f [(0::Int)..] grefs
        where
          f outIdx gref = printf "output%d[%d] = %s;" outputK outIdx (nameNode gref)
  writeInputs grefs inputK = (decls, prototype)
    where
      prototype = ["const double input" ++ show inputK ++ "[" ++ show (length grefs) ++ "]"]
      decls = (printf "/* input %d */" inputK):(zipWith f [(0::Int)..] grefs)
        where
          f inIdx gref = printf "const double %s = input%d[%d];" (nameNode gref) inputK inIdx
  createMxOutputs grefs outputK =
    ["    plhs[" ++ show outputK ++ "] = mxCreateDoubleMatrix( " ++ show (length grefs) ++ ", 1, mxREAL );"]
  checkMxInputShape _ functionName inputK =
    [ "    if ( 1 != mxGetNumberOfDimensions( prhs[" ++ show inputK ++ "] ) ) {"
    , "        char errMsg[200];"
    , "        sprintf(errMsg,"
    , "                \"mex function '" ++ functionName ++ "' got incorrect shape for input " ++ show inputK ++ "\\n\""
    , "                \"expected 1 dimensions but got %d\\n\","
    , "                mxGetNumberOfDimensions( prhs[" ++ show inputK ++ "] ) );"
    , "        mexErrMsgTxt(errMsg);"
    , "    }"
    ]
  checkMxInputDims grefs functionName inputK =
    [ "    if ( " ++ show (length grefs) ++ " != mxGetNumberOfElements( prhs[" ++ show inputK ++ "] ) ) {"
    , "        char errMsg[200];"
    , "        sprintf(errMsg,"
    , "                \"mex function '" ++ functionName ++ "' got incorrect dimensions for input " ++ show inputK ++ "\\n\""
    , "                \"expected dimension: " ++ show (length grefs) ++ " but got dimension: %d\\n\","
    , "                mxGetNumberOfElements( prhs[" ++ show inputK ++ "] ) );"
    , "        mexErrMsgTxt(errMsg);"
    , "    }"
    ]
    
instance GenC [[GraphRef]] where
  numObjects _ = 1
  writeOutputs grefs outputK = (decls, prototype)
    where
      nrows = length grefs
      ncols = length (head grefs)
      prototype = ["double output" ++ show outputK ++ "[" ++ show nrows ++ "][" ++ show ncols ++ "]"]
      decls = (printf "/* output %d */" outputK):
              zipWith f [(r,c) | r <- [0..(length grefs-1)], c <- [0..(length (head grefs)-1)]] (concat grefs)
        where
          f (rowIdx,colIdx) gref = printf "output%d[%d][%d] = %s;" outputK rowIdx colIdx (nameNode gref)
  writeInputs grefs inputK = (decls, prototype)
    where
      nrows = length grefs
      ncols = length (head grefs)
      prototype = ["const double input" ++ show inputK ++ "[" ++ show nrows ++ "][" ++ show ncols ++ "]"]
      decls = (printf "/* input %d */" inputK):
              zipWith f [(r,c) | r <- [0..(nrows-1)], c <- [0..(ncols-1)]] (concat grefs)
        where
          f (rowIdx,colIdx) gref = printf "const double %s = input%d[%d][%d];" (nameNode gref) inputK rowIdx colIdx
  createMxOutputs grefs outputK =
    ["    plhs[" ++ show outputK ++ "] = mxCreateDoubleMatrix( " ++ show nrows++ ", " ++ show ncols ++ ", mxREAL );"]
    where
      nrows = length grefs
      ncols = length (head grefs)
  checkMxInputShape _ functionName inputK =
    [ "    if ( 2 != mxGetNumberOfDimensions( prhs[" ++ show inputK ++ "] ) ) {"
    , "        char errMsg[200];"
    , "        sprintf(errMsg,"
    , "                \"mex function '" ++ functionName ++ "' got incorrect shape for input " ++ show inputK ++ "\\n\""
    , "                \"expected 2 dimensions but got %d\\n\","
    , "                mxGetNumberOfDimensions( prhs[" ++ show inputK ++ "] ) );"
    , "        mexErrMsgTxt(errMsg);"
    , "    }"
    ]
  checkMxInputDims grefs functionName inputK =
    [ "    if ( " ++ show nrows ++ " != mxGetM( prhs[" ++ show inputK ++ "] ) || " ++ show ncols ++ " != mxGetN( prhs[" ++ show inputK ++ "] ) ) {"
    , "        char errMsg[200];"
    , "        sprintf(errMsg,"
    , "                \"mex function '" ++ functionName ++ "' got incorrect dimensions for input " ++ show inputK ++ "\\n\""
    , "                \"expected dimensions: (" ++ show nrows ++ ", " ++ show ncols ++ ") but got (%d, %d)\\n\","
    , "                mxGetM( prhs[" ++ show inputK ++ "] ),"
    , "                mxGetN( prhs[" ++ show inputK ++ "] ) );"
    , "        mexErrMsgTxt(errMsg);"
    , "    }"
    ]
    where
      nrows = length grefs
      ncols = length (head grefs)

instance (GenC a, GenC b) => GenC (a :* b) where
  numObjects (x :* y) = numObjects x + numObjects y
  writeOutputs  (x :* y) outputK = (dx ++ "" : dy, px ++ py)
    where
      (dx, px) = writeOutputs x outputK
      (dy, py) = writeOutputs y (outputK + numObjects x)
  writeInputs  (x :* y) inputK = (dx ++ "":dy, px ++ py)
    where
      (dx, px) = writeInputs x inputK
      (dy, py) = writeInputs y (inputK + numObjects x)
  createMxOutputs (x :* y) outputK = mx ++ my
    where
      mx = createMxOutputs x outputK
      my = createMxOutputs y (outputK + numObjects x)
  checkMxInputShape (x :* y) functionName inputK = mx ++ my
    where
      mx = checkMxInputShape x functionName inputK
      my = checkMxInputShape y functionName (inputK + numObjects x)
  checkMxInputDims (x :* y) functionName inputK = mx ++ my
    where
      mx = checkMxInputDims x functionName inputK
      my = checkMxInputDims y functionName (inputK + numObjects x)

showC :: (Eq a, Show a, Hashable a, NumT b ~ a, NumT c ~ a,
          ToGExprs b, GenC (ContainerT c GraphRef),
          ToGExprs c, GenC (ContainerT b GraphRef))
         => String -> b -> c -> IO String
showC functionName inputs outputs = do
  (_,_,txt) <- showCWithGraphRefs functionName inputs outputs
  return txt

showCWithGraphRefs :: (Eq a, Show a, Hashable a, NumT b ~ a, NumT c ~ a,
                       ToGExprs b, GenC (ContainerT b GraphRef),
                       ToGExprs c, GenC (ContainerT c GraphRef))
                      => String -> b -> c -> IO (ContainerT b GraphRef, ContainerT c GraphRef, String)
showCWithGraphRefs functionName inputs outputs = do
  (inputIndices, outputIndices, hm, _) <- toFunGraph inputs outputs
  let (inDecls, inPrototypes) = writeInputs inputIndices 0
      (outDecls, outPrototypes) = writeOutputs outputIndices 0
      mainDecls = catMaybes $ map (\(gref,gexpr) -> cAssignment gref gexpr) $ reverse $ topSort hm

      body = unlines $ map ("    "++) $
             inDecls ++
             ["","/* body */"] ++
             mainDecls ++ [""] ++
             outDecls

      txt = "#include <math.h>\n\n" ++
            "void " ++ functionName ++ " ( " ++ (intercalate ", " (inPrototypes++outPrototypes)) ++ " )\n{\n" ++
            body ++ "}\n"

  return (inputIndices, outputIndices, txt)
    
nameNode :: GraphRef -> String
nameNode (GraphRef k) = "v_" ++ show k

cAssignment :: Show a => GraphRef -> GExpr a -> Maybe String
cAssignment gref gexpr = fmap (\cop -> "const double " ++ nameNode gref ++ " = " ++ cop ++ ";") (toCOp gexpr)
  where
    bin :: GraphRef -> GraphRef -> String -> String
    bin x y op = nameNode x ++ " " ++ op ++ " " ++ nameNode y
    
    un :: GraphRef -> String -> String
    un x op = op ++ "( " ++ nameNode x ++ " )"
    
    toCOp :: Show a => GExpr a -> Maybe String
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
    toCOp (GFractional (FromRational x)) = Just $ show x
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


showMex :: (Eq a, Show a, Hashable a, NumT b ~ a, NumT c ~ a,
          ToGExprs b, GenC (ContainerT c GraphRef),
          ToGExprs c, GenC (ContainerT b GraphRef))
         => String -> b -> c -> IO String
showMex functionName inputs outputs = do
  (inputIndices, outputIndices, cText) <- showCWithGraphRefs functionName inputs outputs
  return $ cText ++ "\n\n\n" ++ mexFun functionName inputIndices outputIndices

mexFun :: (GenC a, GenC b) => String -> a -> b -> String
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
  , "                \"expected: " ++ show nrhs ++ " but got %d\\n\","
  , "                nrhs);"
  , "        mexErrMsgTxt(errMsg);"
  , "    }"
  , []
  , "    /* check number of outputs  */"
  , "    if ( " ++ show nlhs ++ " != nlhs ) {"
  , "        char errMsg[200];"
  , "        sprintf(errMsg,"
  , "                \"mex function '" ++ functionName ++ "' given incorrect number of outputs\\n\""
  , "                \"expected: " ++ show nlhs ++ " but got %d\\n\","
  , "                nrhs);"
  , "        mexErrMsgTxt(errMsg);"
  , "    }"
  , []
  , "    /* check the shapes of the input arrays */"
  ] ++ checkMxInputShape ins functionName 0 ++
  [ []
  , "    /* check the dimensions of the input arrays */"
  ] ++ checkMxInputDims ins functionName 0 ++
  [ []
  , "    /* create the output arrays */"
  ] ++ createMxOutputs outs 0 ++ -- e.g.: plhs[0] = mxCreateDoubleMatrix(1,ncols,mxREAL);
  [ []
  , "    /* call the c function */"
  , "    " ++ functionName ++ "( " ++ intercalate ", " (inputPtrs ++ outputPtrs) ++ " );"
  , "}"
  ]
  where
    nlhs = numObjects outs
    nrhs = numObjects ins
    inputPtrs = map (\k -> "mxGetPr(prhs[" ++ show k ++ "])") [0..(nrhs-1)]
    outputPtrs = map (\k -> "mxGetPr(plhs[" ++ show k ++ "])") [0..(nlhs-1)]
