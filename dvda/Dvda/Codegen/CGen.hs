{-# OPTIONS_GHC -Wall #-}
--{-# Language FlexibleContexts #-}
--{-# Language GADTs #-}

module Dvda.Codegen.CGen ( showC
--                         , showMex
                         , go
                         ) where

--import Data.Hashable ( Hashable )
--import qualified Data.Foldable as F
--import Data.List ( intercalate )
import qualified Data.Vector as V
--import Text.Printf ( printf )

import Dvda.Expr ( GExpr(..), Floatings(..), Nums(..), Fractionals(..) )
--import Dvda.HashMap ( HashMap )
--import qualified Dvda.HashMap as HM
import Dvda.Algorithm
import Dvda.Algorithm.Construct
import Dvda

go :: IO ()
go = do
  let x = sym "x" :: Expr Double
      y = sym "y"
      inputs = [x,y]
      outputs = [x*2, y/x]
  alg <- constructAlgorithm (V.fromList inputs) (V.fromList outputs)
  putStrLn (showC "foo" alg)

showC :: Show a => String -> Algorithm a -> String
showC funName alg = unlines $ proto : map ("  " ++) (work ++ (map toCInstr (algOps alg))) ++ ["}"]
  where
    proto = "void " ++ funName ++ "(const double input[" ++ show ninputs ++
            "], double output["++show noutputs++"]){"
    ninputs = algInDims alg
    noutputs = algOutDims alg
    n = algWorkSize alg
    work = map (\k -> "double " ++ nameNode (Node k) ++ ";") (take n [0..])
    
toCInstr :: Show a => AlgOp a -> [Char]
toCInstr (InputOp k (InputIdx inputIdx)) =
  nameNode k ++ " = input[" ++ show inputIdx ++ "];"
  ++ " /* input " ++ show inputIdx ++ " */"
toCInstr (OutputOp k (OutputIdx outputIdx)) =
  "output[" ++ show outputIdx ++ "] = " ++ nameNode k ++ ";"
  ++ " /* output " ++ show outputIdx ++ " */"
toCInstr (NormalOp k op) =
  nameNode k ++ " = " ++ showOp op ++ ";"
  ++ " /* " ++ show op ++ " */"

nameNode :: Node -> String
nameNode (Node k) = "w_" ++ show k

binInfix :: Node -> Node -> String -> String
binInfix x y blah = nameNode x ++ " " ++ blah ++ " " ++ nameNode y

unary :: Node -> String -> String
unary x blah = blah ++ "( " ++ nameNode x ++ " )"

bin :: Node -> Node -> String -> String
bin x y blah = blah ++ "( " ++ nameNode x ++ ", " ++ nameNode y ++ " )"

showOp :: Show a => GExpr a Node -> String
showOp (GNum (FromInteger k)) = show k
showOp (GNum (Mul x y)) = binInfix x y "*"
showOp (GNum (Add x y)) = binInfix x y "+"
showOp (GNum (Sub x y)) = binInfix x y "-"
showOp (GNum (Abs x)) = unary x "abs"
showOp (GNum (Negate x)) = "-" ++ nameNode x
showOp (GNum (Signum x)) = unary x "signum"

showOp (GSym s) = error $ "showOp: got GSym " ++ show s
showOp (GConst c) = show c
showOp (GFractional (FromRational r)) = show r
showOp (GFractional (Div x y)) = binInfix x y "/"
showOp (GFloating (Exp x)) = unary x "exp"
showOp (GFloating (Pow x y)) = bin x y "pow"
showOp (GFloating (LogBase _ _)) = error "C generation doesn't support LogBase"
showOp (GFloating (Log x))   = unary x "log"
showOp (GFloating (Sin x))   = unary x "sin"
showOp (GFloating (Cos x))   = unary x "cos"
showOp (GFloating (Tan x))   = unary x "tan"
showOp (GFloating (ASin x))  = unary x "asin"
showOp (GFloating (ACos x))  = unary x "acos"
showOp (GFloating (ATan x))  = unary x "atan"
showOp (GFloating (Sinh x))  = unary x "sinh"
showOp (GFloating (Cosh x))  = unary x "cosh"
showOp (GFloating (Tanh x))  = unary x "tanh"
showOp (GFloating (ASinh _)) = error "C generation doesn't support ASinh"
showOp (GFloating (ACosh _)) = error "C generation doesn't support ACosh"
showOp (GFloating (ATanh _)) = error "C generation doesn't support ATanh"


--createMxOutputs :: [V.Vector Int] -> [String]
--createMxOutputs xs = concat $ zipWith createMxOutput xs [0..]
--  where
--    createMxOutput :: V.Vector Int -> Int -> [String]
--    createMxOutput grefs outputK =
--      [ "    if ( " ++ show outputK ++ " < nlhs ) {"
--      , "        plhs[" ++ show outputK ++ "] = mxCreateDoubleMatrix( " ++ show (V.length grefs) ++ ", 1, mxREAL );"
--      , "        outputs[" ++ show outputK ++ "] = mxGetPr( plhs[" ++ show outputK ++ "] );"
--      , "    } else"
--      , "        outputs[" ++ show outputK ++ "] = (double*)malloc( " ++ show (V.length grefs) ++ "*sizeof(double) );"
--      ]
--
--checkMxInputDims :: V.Vector a -> String -> Int -> [String]
--checkMxInputDims grefs functionName inputK =
--  [ "    if ( !( " ++ show nrows ++ " == mxGetM( prhs[" ++ show inputK ++ "] ) && 1 == mxGetN( prhs[" ++ show inputK ++ "] ) ) && !( " ++ show nrows ++ " == mxGetN( prhs[" ++ show inputK ++ "] ) && 1 == mxGetM( prhs[" ++ show inputK ++ "] ) ) ) {"
--  , "        char errMsg[200];"
--  , "        sprintf(errMsg,"
--  , "                \"mex function '" ++ functionName ++ "' got incorrect dimensions for input " ++ show (1+inputK) ++ "\\n\""
--  , "                \"expected dimensions: (" ++ show nrows ++ ", 1) or (1, " ++ show nrows ++ ") but got (%zu, %zu)\","
--  , "                mxGetM( prhs[" ++ show inputK ++ "] ),"
--  , "                mxGetN( prhs[" ++ show inputK ++ "] ) );"
--  , "        mexErrMsgTxt(errMsg);"
--  , "    }"
--  ]
--  where
--    nrows = V.length grefs
--
---- | Turns a FunGraph into a string containing C code
--showC :: (Eq a, Show a, Hashable a, F.Foldable f, F.Foldable g) =>
--         String-> FunGraph f g a -> String
--showC functionName fg = txt
--  where
--    inputs = [V.fromList $ F.toList (fgInputs fg)]
--    outputs = [V.fromList $ F.toList (fgOutputs fg)]
--    inPrototypes = writeInputPrototypes inputs
--    (outDecls, outPrototypes) = writeOutputs outputs
--    inputMap = makeInputMap inputs
--    mainDecls = let f k = case fgLookupGExpr fg k of
--                      Just v -> cAssignment inputMap k v
--                      Nothing -> error $ "couldn't find node " ++ show k ++ " in fungraph :("
--                in map f $ fgTopSort fg
--  
--    body = unlines $ map ("    "++) $
--           mainDecls ++ [""] ++
--           outDecls
--  
--    txt = "#include <math.h>\n\n" ++
--          "void " ++ functionName ++ " ( " ++ intercalate ", " (inPrototypes++outPrototypes) ++ " )\n{\n" ++
--          body ++ "}\n"
--
--nameNode :: Int -> String
--nameNode k = "v_" ++ show k
--
--cAssignment :: (Eq a, Hashable a, Show a) => HashMap (GExpr a Int) String -> Int -> GExpr a Int -> String
--cAssignment inputMap k g@(GSym _) = case HM.lookup g inputMap of
--  Nothing -> error $ "cAssignment: couldn't find " ++ show g ++ " in the input map"
--  Just str -> "const double " ++ nameNode k ++ " = " ++ str
--cAssignment _ k gexpr = "const double " ++ nameNode k ++ " = " ++ toCOp gexpr ++ ";"
--  where
--    bin :: Int -> Int -> String -> String
--    bin x y op = nameNode x ++ " " ++ op ++ " " ++ nameNode y
--    
--    un :: Int -> String -> String
--    un x op = op ++ "( " ++ nameNode x ++ " )"
--
--    asTypeOfG :: a -> GExpr a b -> a
--    asTypeOfG x _ = x
--    
--    toCOp (GSym _)                       = error "toCOp (GSym _) should be impossible"
--    toCOp (GConst c)                     = show c
--    toCOp (GNum (Mul x y))               = bin x y "*"
--    toCOp (GNum (Add x y))               = bin x y "+"
--    toCOp (GNum (Sub x y))               = bin x y "-"
--    toCOp (GNum (Negate x))              = un x "-"
--    toCOp (GNum (Abs x))                 = un x "abs"
--    toCOp (GNum (Signum x))              = un x "sign"
--    toCOp (GNum (FromInteger x))         = show x
--    toCOp (GFractional (Div x y))        = bin x y "/"
--    toCOp (GFractional (FromRational x)) = show (fromRational x `asTypeOfG` gexpr)
--    toCOp (GFloating (Pow x y))          = "pow( " ++ nameNode x ++ ", " ++ nameNode y ++ " )"
--    toCOp (GFloating (LogBase x y))      = "log( " ++ nameNode y ++ ") / log( " ++ nameNode x ++ " )"
--    toCOp (GFloating (Exp x))            = un x "exp"
--    toCOp (GFloating (Log x))            = un x "log"
--    toCOp (GFloating (Sin x))            = un x "sin"
--    toCOp (GFloating (Cos x))            = un x "cos"
--    toCOp (GFloating (ASin x))           = un x "asin"
--    toCOp (GFloating (ATan x))           = un x "atan"
--    toCOp (GFloating (ACos x))           = un x "acos"
--    toCOp (GFloating (Sinh x))           = un x "sinh"
--    toCOp (GFloating (Cosh x))           = un x "cosh"
--    toCOp (GFloating (Tanh x))           = un x "tanh"
--    toCOp (GFloating (ASinh _))          = error "C generation doesn't support ASinh"
--    toCOp (GFloating (ATanh _))          = error "C generation doesn't support ATanh"
--    toCOp (GFloating (ACosh _))          = error "C generation doesn't support ACosh"
--
--
--showMex :: (Eq a, Show a, Hashable a, F.Foldable f, F.Foldable g)
--           => String -> FunGraph f g a -> String
--showMex functionName fg = cText ++ "\n\n\n" ++ mexFun functionName [V.fromList $ F.toList $ fgInputs fg] [V.fromList $ F.toList $ fgOutputs fg]
--  where
--    cText = showC functionName fg -- matlab is column major >_<
--
--mexFun :: String -> [V.Vector a] -> [V.Vector Int] -> String
--mexFun functionName ins outs =
--  unlines $
--  [ "#include \"mex.h\""
--  , []
--  , "void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])"
--  , "{"
--  , "    /* check number of inputs  */"
--  , "    if ( " ++ show nrhs ++ " != nrhs ) {"
--  , "        char errMsg[200];"
--  , "        sprintf(errMsg,"
--  , "                \"mex function '" ++ functionName ++ "' given incorrect number of inputs\\n\""
--  , "                \"expected: " ++ show nrhs ++ " but got %d\","
--  , "                nrhs);"
--  , "        mexErrMsgTxt(errMsg);"
--  , "    }"
--  , []
--  , "    /* check the dimensions of the input arrays */"
--  ] ++ concat (zipWith (`checkMxInputDims` functionName) ins [0..]) ++
--  [ []
--  , "    /* check number of outputs  */"
--  , "    if ( " ++ show nlhs ++ " < nlhs ) {"
--  , "        char errMsg[200];"
--  , "        sprintf(errMsg,"
--  , "                \"mex function '" ++ functionName ++ "' saw too many outputs\\n\""
--  , "                \"expected <= " ++ show nlhs ++ " but got %d\","
--  , "                nlhs);"
--  , "        mexErrMsgTxt(errMsg);"
--  , "    }"
--  , []
--  , "    /* create the output arrays, if no output is provided by user create a dummy output */"
--  , "    double * outputs[" ++ show nlhs ++ "];"
--  ] ++ createMxOutputs outs ++ -- e.g.: plhs[0] = mxCreateDoubleMatrix(1,ncols,mxREAL);
--  [ []
--  , "    /* call the c function */"
--  , "    " ++ functionName ++ "( " ++ intercalate ", " (inputPtrs ++ outputPtrs) ++ " );"
--  , []
--  , "    /* free the unused dummy outputs */"
--  , "    int k;"
--  , "    for ( k = " ++ show (nlhs - 1) ++ "; nlhs <= k; k-- )"
--  , "        free( outputs[k] );"
--  , "}"
--  ]
--  where
--    nlhs = length outs
--    nrhs = length ins
--    inputPtrs  = map (\k -> "const " ++ "mxGetPr(prhs[" ++ show k ++ "])") [(0::Int)..]
--    outputPtrs = map (\k ->    ""    ++ "(outputs[" ++ show k ++ "])")     [(0::Int)..]
