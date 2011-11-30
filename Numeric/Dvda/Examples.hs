-- Examples.hs

{-# OPTIONS_GHC -Wall #-}

module Numeric.Dvda.Examples( simpleGraph
                            , tensorGraph
--                            , fadExample
--                            , radExample
--                            , substituteExample
--                            , codegenExample
                            ) where


import Numeric.Dvda.Expr
import Numeric.Dvda.Vis
--import Numeric.Dvda.AD.Fad(fad)
--import Numeric.Dvda.AD.Rad(rad)
--import Numeric.Dvda.Codegen.Codegen(toFunction)
--import Numeric.Dvda.Function

simpleGraph :: IO ()
simpleGraph = do
  let x = sym "x" :: Expr Double
      y = x*2 + cos(x)/5
  previewExprs [y, x*x]

tensorGraph :: IO ()
tensorGraph = do
  let x = symMat (3,5) "x"
      y = sym "y"
      z = x*2 + cos(x)/5 + y
  previewExprs [z :: Expr Double]

--fadExample :: IO ()
--fadExample = do
--  let f x = [x*34 + 5, x*34 + 4/x, sin x]
--      y = symbolic "y" :: Expr Double
--      expr = f y
--      g = map simplify $ fad f y
--
--  print expr
--  print $ expr
--  print $ g
--  previewGraph $ exprsToGraph expr
--  previewGraph $ exprsToGraph g
--  
--
--radExample :: IO ()
--radExample = do
--  let exampleExpr :: Expr Double
--      exampleExpr = (z + x*y)*log(cos(x)/(tanh(y)))**(z/exp(y))
--      x = symbolic "x"
--      y = symbolic "y"
--      z = symbolic "z"
--      args = [x,y,z]
--
--  print exampleExpr
--  print $ rad exampleExpr args
--  
--  previewGraph_ $ lexprToGraph ("f", exampleExpr)
--  previewGraph_ $ lexprsToGraph $ zip ["f", "df/dx", "df/dy", "df/dz"] (exampleExpr:(rad exampleExpr args))
--
--substituteExample :: IO ()
--substituteExample = do
--  let e0 :: Expr Double
--      e0 = (z + x*y)*cos(x) + log(y)/z
--      x = symbolic "x"
--      y = symbolic "y"
--      z = symbolic "z"
--      e1 = substitutes e0 [(x,4), (y,2), (z,-3)]
--      e2 = removeIdentities e1
--
--  print e0
--  print e1
--  print $ evaluate e1
--  print $ evaluate e2
--  previewGraph_ $ lexprToGraph ("f", e0)
--  previewGraph_ $ lexprToGraph ("f", e1)
--  previewGraph_ $ lexprToGraph ("f", e2)
--
--
--
--codegenExample :: IO ()
--codegenExample = do
--
--  let testFun :: Floating a => [a] -> [a]
--      testFun [x',y'] = [y'/cos(x'), 23423*atan(100*x')]
--      testFun _ = error "bad testFun inputs"
--
--      x = symbolic "x"
--      y = symbolic "y"
--  
--      outputs = testFun [x,y]
--      
--  -- make function
--  fun <- toFunction [x,y] outputs
--  
--  -- call different ways
--  putStr "call testFun directly: "
--  print $ testFun [12,13::Double]
--  
--  putStr "callNative Function:   "
--  print $ callNative fun [12,13]
--  
--  putStr "callC Function:        "
--  print $ callC fun [12,13::Double]
