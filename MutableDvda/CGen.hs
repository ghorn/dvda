{-# OPTIONS_GHC -Wall #-}
{-# Language TypeOperators #-}
{-# Language FlexibleInstances #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}

module MutableDvda.CGen ( GenC(..)
                        , showC
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
      f0 = x*y + z
      f1 = [f0/2, f0*y]
  showC "foo" (x :* [y]:*[[z]]) (f0:*f1:*[[f0*f0]]) >>= putStrLn

-------------------------------------------------------------------------
class GenC a where
  numObjects :: a -> Int
  writeOutputs :: a -> Int -> ([String], [String]) -- (output declarations, prototype)
  writeInputs :: a -> Int -> ([String], [String]) -- (input declarations, prototype)

instance GenC GraphRef where
  numObjects _ = 1
  writeOutputs gref outputK = (decls, prototype)
    where
      decls = [printf "/* output %d */" outputK, printf "(*output%d) = %s;" outputK (nameNode gref)]
      prototype = ["double * const output" ++ show outputK]
  writeInputs gref inputK = (decls, prototype)
    where
      decls = [printf "/* input %d */" inputK, printf "const double %s = input%d;" (nameNode gref) inputK]
      prototype = ["const double input" ++ show inputK]

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

showC :: (Eq a, Show a, Hashable a, NumT b ~ a, NumT c ~ a,
          ToGExprs b, GenC (ContainerT c GraphRef),
          ToGExprs c, GenC (ContainerT b GraphRef))
         => String -> b -> c -> IO String
showC functionName inputs outputs = do
  (inputIndices, outputIndices, hm, _) <- toFunGraph inputs outputs
  let (inDecls, inPrototypes) = writeInputs inputIndices 0
      (outDecls, outPrototypes) = writeOutputs outputIndices 0
      mainDecls = catMaybes $ map (\(gref,gexpr) -> cAssignment gref gexpr) $ reverse $ topSort hm

      body = unlines $ map ("    "++) $
             inDecls ++
             ["","/* body */"] ++
             mainDecls ++ [""] ++
             outDecls
  
  return $
    "#include <math.h>\n\n" ++
    "void " ++ functionName ++ " ( " ++ (intercalate ", " (inPrototypes++outPrototypes)) ++ " )\n{\n" ++
    body ++ "}\n"
    
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
