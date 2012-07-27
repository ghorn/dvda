{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}

module Main where

import qualified Dvda.HashMap as HM

import MutableDvda.AD
import MutableDvda.Expr
import MutableDvda.FullShow
import MutableDvda.Graph

bg :: Floating a => a -> a
bg x' = f''
  where
    y' = 2*x'
    z' = 4*y'
--    x' = sym "x"
--    y' = sym "y"
--    z' = sym "z"
    
    f0 x y z = (z + x*y)*log(cos x / tanh y)**(z/exp y)
    fx0 = f0 (f0 x' y' z') (f0 z' y' x') (f0 y' x' z')
    fy0 = f0 (f0 z' x' y') (f0 x' z' y') (f0 z' z' y')
    fz0 = f0 (f0 x' y' z') (f0 x' y' x') (f0 y' x' y')
    f = f0 fx0 fy0 fz0
    f' = f0 fy0 fz0 fx0
    f'' = f0 f' f f'
    f''' = f0 f'' f' f''
    f'''' = f0 f''' f''' f'''
    f''''' = f0 f'''' f'''' f''''
--    f = z' + x'*y'

--    fx = diff f x'
--    fy = diff f y'
--    fz = diff f z'

g :: Expr Double
g = bg (sym "x")

--f :: Expr Double
--f = x + x where x = sym "x"

--main :: IO ()
--main = do
----  (fullShow booboo) >>= putStrLn
--  (toIO $ countNodes booboo) >>= print
----  putStrLn "--------------------------------------------------"
--  bam <- toIO (backprop 1 booboo)
------  mapM (\(_,x) -> fullShow x) bam >>= mapM_ putStrLn
------  mapM_ putStrLn $ map (\(x,_) -> show x) bam
--  toIO (fmap sum (mapM (\(_,x) -> countNodes x) bam)) >>= print
----  print $ length $ bam

--main :: IO ()
--main = do
----  (_, n, _) <- toGExprs [booboo]
--  sf <- fullShow f
--  putStrLn $ "f = " ++ sf ++ " ( " ++ show (unsafePerformIO $ readExpr f) ++ " )"
----  putStrLn "\nrunning rad..."
----  radMap <- rad f
----  sx <- fullShow (head $ HM.keys radMap)
----  sdx <- fullShow (head $ HM.elems radMap)
----  putStrLn $ "df/d" ++ sx ++ " = " ++ sdx
--  putStrLn "\nmaking graph / performing CSE..."
--  (_,n,hm) <- toGExprs [f] -- :(HM.elems radMap))
--  putStrLn $ show n ++ " nodes"
--
--  putStrLn "\nhashmap:"
--  mapM_ print (HM.toList hm)

--main :: IO ()
--main = do
----  (_, n, _) <- toGExprs [g]
--  putStrLn "\nrunning rad..."
--  radMap <- rad g
----  sg <- fullShow g
----  putStrLn sg
--  putStrLn "\nmaking graph / performing CSE..."
----  (_,n,hm,_) <- unsafeToGExprs (g:(HM.elems radMap))
--  (_,n,hm) <- toFunGraph (g:(HM.elems radMap))
--  putStrLn $ show n ++ " nodes"
----  print (graphCount hm)
----  putStrLn (graphShow hm)
