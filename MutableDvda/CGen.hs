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
import qualified Data.HashSet as HS

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
      w3 = sym "w3"
      f0 = x*y + z + w1 + w2 + w3
      
      f1 = [f0/2, f0*y, w, 0.0, 0]
--  showC "foo" (x :* [y]:*[[z]]) (f0:*f1:*[[f0*f0]]) >>= putStrLn
  showMex "foo" (x :* [y,w3]:*[[z,w], [w1,w2]]) (f0:*f1:*[[f0*f0]]:*[f1]) >>= putStrLn

-------------------------------------------------------------------------
class GenC a where
  numObjects :: a -> Int
  writeOutputs :: a -> Int -> ([String], [String]) -- (output declarations, prototype)
  writeInputs :: a -> Int -> ([String], [String]) -- (input declarations, prototype)
  -- mex function stuff
  createMxOutputs :: a -> Int -> [String]
  checkMxInputDims :: a -> String -> Int -> [String]

instance GenC GraphRef where
  numObjects _ = 1
  writeOutputs gref outputK = (decls, prototype)
    where
      decls = [printf "/* output %d */" outputK, printf "(*output%d) = %s;" outputK (nameNode gref)]
      prototype = ["double * const output" ++ show outputK]
  writeInputs gref inputK = (decls, prototype)
    where
      (bc,ec) = case gref of GraphRef (-1) -> ("/* ", " */")
                             _ -> ("","")
      decls =
        [printf "/* input %d */" inputK, printf "%sconst double %s = *input%d;%s" bc (nameNode gref) inputK ec]
      prototype = ["const double * input" ++ show inputK]
  createMxOutputs _ outputK = ["    plhs[" ++ show outputK ++ "] = mxCreateDoubleScalar( 0 );"]
  checkMxInputDims _ functionName inputK =
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
          f inIdx gref = printf "%sconst double %s = input%d[%d];%s" bc (nameNode gref) inputK inIdx ec
            where
              (bc,ec) = case gref of GraphRef (-1) -> ("/* ", " */")
                                     _ -> ("","")

  createMxOutputs grefs outputK =
    ["    plhs[" ++ show outputK ++ "] = mxCreateDoubleMatrix( " ++ show (length grefs) ++ ", 1, mxREAL );"]
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
      nrows = length grefs
    
instance GenC [[GraphRef]] where
  numObjects _ = 1
  writeOutputs grefs outputK
    | any ((ncols /=) . length) grefs =
        error $ "writeOutputs [[GraphRef]] matrix got inconsistent column dimensions: "++ show (map length grefs)
    | otherwise = (decls, prototype)
    where
      nrows = length grefs
      ncols = length (head grefs)
      prototype = ["double output" ++ show outputK ++ "[" ++ show nrows ++ "][" ++ show ncols ++ "]"]
      decls = (printf "/* output %d */" outputK):
              zipWith f [(r,c) | r <- [0..(nrows-1)], c <- [0..(ncols-1)]] (concat grefs)
        where
          f (rowIdx,colIdx) gref = printf "output%d[%d][%d] = %s;" outputK rowIdx colIdx (nameNode gref)
  writeInputs grefs inputK
    | any ((ncols /=) . length) grefs =
        error $ "writeInputs [[GraphRef]] matrix got inconsistent column dimensions: "++ show (map length grefs)
    | otherwise = (decls, prototype)
    where
      nrows = length grefs
      ncols = length (head grefs)
      prototype = ["const double input" ++ show inputK ++ "[" ++ show nrows ++ "][" ++ show ncols ++ "]"]
      decls = (printf "/* input %d */" inputK):
              zipWith f [(r,c) | r <- [0..(nrows-1)], c <- [0..(ncols-1)]] (concat grefs)
        where
          f (rowIdx,colIdx) gref = printf "%sconst double %s = input%d[%d][%d];%s" bc (nameNode gref) inputK rowIdx colIdx ec
            where
              (bc,ec) = case gref of GraphRef (-1) -> ("/* ", " */")
                                     _ -> ("","")

  createMxOutputs grefs outputK =
    ["    plhs[" ++ show outputK ++ "] = mxCreateDoubleMatrix( " ++ show nrows++ ", " ++ show ncols ++ ", mxREAL );"]
    where
      nrows = length grefs
      ncols = length (head grefs)
  checkMxInputDims grefs functionName inputK =
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
      ncols = length (head grefs)

instance (GenC a, GenC b) => GenC (a :* b) where
  numObjects (x :* y) = numObjects x + numObjects y
  writeOutputs (x :* y) outputK = (dx ++ "" : dy, px ++ py)
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
  checkMxInputDims (x :* y) functionName inputK = mx ++ my
    where
      mx = checkMxInputDims x functionName inputK
      my = checkMxInputDims y functionName (inputK + numObjects x)

-- | Turns inputs and outputs into a string containing C code.
-- .
--   Also pass a name to give to the C function
-- .
--   This function simply calls showCWithGraphRefs and discards the first two outputs
showC :: (Eq a, Show a, Hashable a, NumT b ~ a, NumT c ~ a,
          ToGExprs b, GenC (ContainerT c GraphRef),
          ToGExprs c, GenC (ContainerT b GraphRef))
         => String -> b -> c -> IO String
showC functionName inputs outputs = do
  (_,_,txt) <- showCWithGraphRefs functionName inputs outputs
  return txt

-- | Turns inputs and outputs into a string containing C code. Also return indices of the inputs and outputs
-- .
--   Also pass a name to give to the C function
showCWithGraphRefs :: (Eq a, Show a, Hashable a, NumT b ~ a, NumT c ~ a,
                       ToGExprs b, GenC (ContainerT b GraphRef),
                       ToGExprs c, GenC (ContainerT c GraphRef))
                      => String -> b -> c -> IO (ContainerT b GraphRef, ContainerT c GraphRef, String)
showCWithGraphRefs functionName inputs outputs = do
  redundantExprs <- getRedundantExprs inputs
  case redundantExprs of
    Just res -> error $ "showCWithGraphRefs saw redundant inputs: " ++ show (HS.toList res)
    Nothing -> do
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

    asTypeOfG :: a -> GExpr a -> a
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
  , "                \"expected: " ++ show nrhs ++ " but got %d\","
  , "                nrhs);"
  , "        mexErrMsgTxt(errMsg);"
  , "    }"
  , []
  , "    /* check number of outputs  */"
  , "    if ( " ++ show nlhs ++ " != nlhs ) {"
  , "        char errMsg[200];"
  , "        sprintf(errMsg,"
  , "                \"mex function '" ++ functionName ++ "' given incorrect number of outputs\\n\""
  , "                \"expected: " ++ show nlhs ++ " but got %d\","
  , "                nlhs);"
  , "        mexErrMsgTxt(errMsg);"
  , "    }"
  , []
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
