{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}

module MutableDvda.Graph ( GExpr(..)
                         , GraphRef(..)
                         , ToGExprs(..)
                         , toFunGraph
                         , unsafeToFunGraph
                         , topSort
                         ) where

import Control.Concurrent.MVar
import qualified Data.Graph as Graph
import Data.Hashable ( Hashable, hash, combine )

import Dvda.HashMap ( HashMap )
import qualified Dvda.HashMap as HM

import MutableDvda.Expr

data GExpr a where
  GSym :: String -> GExpr a
  GConst :: a -> GExpr a
  GNum :: Nums GraphRef -> GExpr a
  GFractional :: Fractionals GraphRef -> GExpr a
  GFloating :: Floatings GraphRef -> GExpr a

deriving instance Show a => Show (GExpr a)
deriving instance Eq a => Eq (GExpr a)

instance Hashable a => Hashable (GExpr a) where
  hash (GSym name)     = hash "GSym"        `combine` hash name
  hash (GConst x)      = hash "GConst"      `combine` hash x
  hash (GNum x)        = hash "GNum"        `combine` hash x
  hash (GFractional x) = hash "GFractional" `combine` hash x
  hash (GFloating x)   = hash "GFloating"   `combine` hash x

insert :: (Eq a, Hashable a) => HashMap (GExpr a) GraphRef -> HashMap (Expr a) GraphRef
          -> Int -> Expr a -> IO (GraphRef, Int, HashMap (GExpr a) GraphRef, HashMap (Expr a) GraphRef, [MVar (Expr a)])
insert hm im n (EGraphRef _ gr)               = return (gr, n, hm, im, [])
insert hm im0 n expr@(ESym name)              = return $ tupleAppend im [] ret
  where
    ret@(graphRef, _, _) = cseInsert hm n (GSym name)
    im = HM.insert expr graphRef im0
insert hm im n (EConst c)                     = return $ tupleAppend im [] $ cseInsert hm n (GConst c)
insert hm im n (ENum (Mul x y))               = binaryInsert hm im n GNum Mul x y
insert hm im n (ENum (Add x y))               = binaryInsert hm im n GNum Add x y
insert hm im n (ENum (Sub x y))               = binaryInsert hm im n GNum Sub x y
insert hm im n (ENum (Negate x))              = unaryInsert hm im n GNum Negate x
insert hm im n (ENum (Abs x))                 = unaryInsert hm im n GNum Abs x
insert hm im n (ENum (Signum x))              = unaryInsert hm im n GNum Signum x
insert hm im n (ENum (FromInteger x))         = return $ tupleAppend im [] $ cseInsert hm n (GNum (FromInteger x))
insert hm im n (EFractional (Div x y))        = binaryInsert hm im n GFractional Div x y
insert hm im n (EFractional (FromRational x)) = return $ tupleAppend im [] $ cseInsert hm n (GFractional (FromRational x))
insert hm im n (EFloating (Pow x y))          = binaryInsert hm im n GFloating Pow x y
insert hm im n (EFloating (LogBase x y))      = binaryInsert hm im n GFloating LogBase x y
insert hm im n (EFloating (Exp x))            = unaryInsert hm im n GFloating Exp x
insert hm im n (EFloating (Log x))            = unaryInsert hm im n GFloating Log x
insert hm im n (EFloating (Sin x))            = unaryInsert hm im n GFloating Sin x
insert hm im n (EFloating (Cos x))            = unaryInsert hm im n GFloating Cos x
insert hm im n (EFloating (ASin x))           = unaryInsert hm im n GFloating ASin x
insert hm im n (EFloating (ATan x))           = unaryInsert hm im n GFloating ATan x
insert hm im n (EFloating (ACos x))           = unaryInsert hm im n GFloating ACos x
insert hm im n (EFloating (Sinh x))           = unaryInsert hm im n GFloating Sinh x
insert hm im n (EFloating (Cosh x))           = unaryInsert hm im n GFloating Cosh x
insert hm im n (EFloating (Tanh x))           = unaryInsert hm im n GFloating Tanh x
insert hm im n (EFloating (ASinh x))          = unaryInsert hm im n GFloating ASinh x
insert hm im n (EFloating (ATanh x))          = unaryInsert hm im n GFloating ATanh x
insert hm im n (EFloating (ACosh x))          = unaryInsert hm im n GFloating ACosh x
insert hm0 im0 n0 (ERef mv) = do
  e <- takeMVar mv
  case e of (EGraphRef _ gr) -> do -- reuse node
              putMVar mv e
              return (gr, n0, hm0, im0, [])
            _ -> do -- insert and memoize new node
              (gr, n, hm, im, mvs) <- insert hm0 im0 n0 e
              putMVar mv (EGraphRef e gr)
              return (gr, n, hm, im, mv:mvs)

tupleAppend :: d -> e -> (a,b,c) -> (a,b,c,d,e)
tupleAppend d e (a,b,c) = (a,b,c,d,e)

binaryInsert
  :: (Eq a, Hashable a)
     => HashMap (GExpr a) GraphRef
     -> HashMap (Expr a) GraphRef
     -> Int
     -> (t -> GExpr a)
     -> (GraphRef -> GraphRef -> t)
     -> Expr a
     -> Expr a
     -> IO (GraphRef, Int, HashMap (GExpr a) GraphRef, HashMap (Expr a) GraphRef, [MVar (Expr a)])
binaryInsert hm0 im0 n0 gnum mul x y = do
  (kx,n1,hm1,im1,mv1) <- insert hm0 im0 n0 x
  (ky,n2,hm2,im2,mv2) <- insert hm1 im1 n1 y
  return $ tupleAppend im2 (mv1++mv2) $ cseInsert hm2 n2 (gnum (mul kx ky))

unaryInsert
  :: (Eq a, Hashable a)
     => HashMap (GExpr a) GraphRef
     -> HashMap (Expr a) GraphRef
     -> Int
     -> (nf GraphRef -> GExpr a)
     -> (GraphRef -> nf GraphRef)
     -> Expr a
     -> IO (GraphRef, Int, HashMap (GExpr a) GraphRef, HashMap (Expr a) GraphRef, [MVar (Expr a)])
unaryInsert hm0 im0 n0 gnum mul x = do
  (k,n1,hm1,im1,mv1) <- insert hm0 im0 n0 x
  return $ tupleAppend im1 mv1 $ cseInsert hm1 n1 (gnum (mul k))
  
cseInsert :: (Eq k, Hashable k) => HashMap k GraphRef -> Int -> k -> (GraphRef, Int, HashMap k GraphRef)
cseInsert hm n gexpr = case HM.lookup gexpr hm of
  Nothing -> (GraphRef n, n+1, HM.insert gexpr (GraphRef n) hm)
  Just k -> (k, n, hm)


-------------------------------- turn things into function graphs --------------------------------------
resetGraphRefs :: MVar (Expr a) -> IO ()
resetGraphRefs mv = do
  expr <- takeMVar mv
  case expr of (EGraphRef e _) -> putMVar mv e
               _               -> error "shouldn't ever try to resetGraphRefs on non EGraphRef"

class ToGExprs a where
  type NumT a
  type ContainerT a b
  readExprs :: a -> IO a
  mapExprs :: (Expr (NumT a) -> GraphRef) -> a -> ContainerT a GraphRef
  unsafeToGExprs ::
    (Eq (NumT a), Hashable (NumT a))
    => Int -> HashMap (GExpr (NumT a)) GraphRef -> HashMap (Expr (NumT a)) GraphRef -> a
    -> IO (ContainerT a GraphRef, [MVar (Expr (NumT a))], Int, HashMap (GExpr (NumT a)) GraphRef, HashMap (Expr (NumT a)) GraphRef)

instance ToGExprs (Expr a) where
  type NumT (Expr a) = a
  type ContainerT (Expr a) b = b
  readExprs = readExpr
  mapExprs f x = f x
  unsafeToGExprs n0 hm0 im0 e = do
      (gref, n, hm, im, mv) <- insert hm0 im0 n0 e
      return (gref, mv, n, hm, im)

instance ToGExprs a => ToGExprs [a] where
  type NumT [a] = NumT a
  type ContainerT [a] b = [ContainerT a b]
  readExprs = mapM readExprs
  mapExprs f xs = map (mapExprs f) xs
  unsafeToGExprs n0' hm0' im0' = f ([], [], n0', hm0', im0')
    where
      f ret [] = return ret
      f (gref0, mv0, n0, hm0, im0) (e:es) = do
        (gref, mv, n, hm, im) <- unsafeToGExprs n0 hm0 im0 e
        f (gref0 ++ [gref], mv0 ++ mv, n, hm, im) es

---- | This version consumes the Exprs as a side effect, so only use it internally if you generate the Exprs youself and can discard them after calling unsafeToGExprs
unsafeToFunGraph :: (Eq a, Hashable a, Show a, NumT b ~ a, NumT c ~ a, ToGExprs b, ToGExprs c)
                    => b -> c -> IO (ContainerT b GraphRef, ContainerT c GraphRef, HashMap (GExpr a) GraphRef, Int, [MVar (Expr a)])
unsafeToFunGraph inputExprs_ outputExprs = do
  inputExprs <- readExprs inputExprs_
  (outputIndices, mvs, n, hm, inputMap) <- unsafeToGExprs 0 HM.empty HM.empty outputExprs

  let lookupExpr e@(ESym _) = case HM.lookup e inputMap of
        Just x -> x
        Nothing ->   error $ "ERROR: in toFunGraph, input " ++ show e ++ " was not found as parent of outputs"
      lookupExpr e = error $ "ERROR: in toFunGraph, input " ++ show e ++ " is not symbolic type"
      inputIndices = mapExprs lookupExpr inputExprs
  return (inputIndices, outputIndices, hm, n, mvs)

toFunGraph :: (Eq a, Hashable a, Show a, NumT b ~ a, NumT c ~ a, ToGExprs b, ToGExprs c)
              => b -> c -> IO (ContainerT b GraphRef, ContainerT c GraphRef, HashMap (GExpr a) GraphRef, Int)
toFunGraph inputExprs outputExprs = do
  (inputIndices, outputIndices, hm, n, mvs) <- unsafeToFunGraph inputExprs outputExprs
  mapM_ resetGraphRefs mvs
  return (inputIndices, outputIndices, hm, n)

topSort :: HashMap (GExpr a) GraphRef -> [GExpr a]
topSort hm = map ((\(x,_,_) -> x) . vertexToNode) (Graph.topSort graph)
  where
    (graph, vertexToNode) = Graph.graphFromEdges' $ map f (HM.toList hm)
      where
        f (gexpr, GraphRef k) = (gexpr, k, map (\(GraphRef n) -> n) $ getChildren gexpr)

getChildren :: GExpr a -> [GraphRef]
getChildren (GSym _)                       = []
getChildren (GConst _)                     = []
getChildren (GNum (Mul x y))               = [x,y]
getChildren (GNum (Add x y))               = [x,y]
getChildren (GNum (Sub x y))               = [x,y]
getChildren (GNum (Negate x))              = [x]
getChildren (GNum (Abs x))                 = [x]
getChildren (GNum (Signum x))              = [x]
getChildren (GNum (FromInteger _))         = []
getChildren (GFractional (Div x y))        = [x,y]
getChildren (GFractional (FromRational _)) = []
getChildren (GFloating (Pow x y))          = [x,y]
getChildren (GFloating (LogBase x y))      = [x,y]
getChildren (GFloating (Exp x))            = [x]
getChildren (GFloating (Log x))            = [x]
getChildren (GFloating (Sin x))            = [x]
getChildren (GFloating (Cos x))            = [x]
getChildren (GFloating (ASin x))           = [x]
getChildren (GFloating (ATan x))           = [x]
getChildren (GFloating (ACos x))           = [x]
getChildren (GFloating (Sinh x))           = [x]
getChildren (GFloating (Cosh x))           = [x]
getChildren (GFloating (Tanh x))           = [x]
getChildren (GFloating (ASinh x))          = [x]
getChildren (GFloating (ATanh x))          = [x]
getChildren (GFloating (ACosh x))          = [x]
