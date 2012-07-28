{-# OPTIONS_GHC -Wall #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TemplateHaskell #-}

module MutableDvda.Graph ( topSort
                         , insert
--                         , getRedundantExprs
                         ) where

import qualified Data.Graph as DataGraph
import Data.Hashable ( Hashable )

import Dvda.HashMap ( HashMap )
import qualified Dvda.HashMap as HM
import MutableDvda.Expr ( Expr(..), GExpr(..), Floatings(..), Fractionals(..), Nums(..), GraphRef(..) )

insert :: (Eq a, Hashable a) => HashMap (GExpr a GraphRef) GraphRef -> HashMap (Expr a) GraphRef
          -> Int -> Expr a -> (GraphRef, Int, HashMap (GExpr a GraphRef) GraphRef, HashMap (Expr a) GraphRef)
insert hm im n (EGraphRef _ gr)               = (gr, n, hm, im)
insert hm im0 n expr@(ESym name)              = tupleAppend im ret
  where
    ret@(graphRef, _, _) = cseInsert hm n (GSym name)
    im = HM.insert expr graphRef im0
insert hm im n (EConst c)                     = tupleAppend im $ cseInsert hm n (GConst c)
insert hm im n (ENum (Mul x y))               = binaryInsert hm im n GNum Mul x y
insert hm im n (ENum (Add x y))               = binaryInsert hm im n GNum Add x y
insert hm im n (ENum (Sub x y))               = binaryInsert hm im n GNum Sub x y
insert hm im n (ENum (Negate x))              = unaryInsert hm im n GNum Negate x
insert hm im n (ENum (Abs x))                 = unaryInsert hm im n GNum Abs x
insert hm im n (ENum (Signum x))              = unaryInsert hm im n GNum Signum x
insert hm im n (ENum (FromInteger x))         = tupleAppend im $ cseInsert hm n (GNum (FromInteger x))
insert hm im n (EFractional (Div x y))        = binaryInsert hm im n GFractional Div x y
insert hm im n (EFractional (FromRational x)) = tupleAppend im $ cseInsert hm n (GFractional (FromRational x))
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

tupleAppend :: d -> (a,b,c) -> (a,b,c,d)
tupleAppend d (a,b,c) = (a,b,c,d)

binaryInsert
  :: (Eq a, Hashable a)
     => HashMap (GExpr a GraphRef) GraphRef
     -> HashMap (Expr a) GraphRef
     -> Int
     -> (t -> GExpr a GraphRef)
     -> (GraphRef -> GraphRef -> t)
     -> Expr a
     -> Expr a
     -> (GraphRef, Int, HashMap (GExpr a GraphRef) GraphRef, HashMap (Expr a) GraphRef)
binaryInsert hm0 im0 n0 gnum mul x y = tupleAppend im2 $ cseInsert hm2 n2 (gnum (mul kx ky))
  where
    (kx,n1,hm1,im1) = insert hm0 im0 n0 x
    (ky,n2,hm2,im2) = insert hm1 im1 n1 y

unaryInsert
  :: (Eq a, Hashable a)
     => HashMap (GExpr a GraphRef) GraphRef
     -> HashMap (Expr a) GraphRef
     -> Int
     -> (nf GraphRef -> GExpr a GraphRef)
     -> (GraphRef -> nf GraphRef)
     -> Expr a
     -> (GraphRef, Int, HashMap (GExpr a GraphRef) GraphRef, HashMap (Expr a) GraphRef)
unaryInsert hm0 im0 n0 gnum mul x = tupleAppend im1 $ cseInsert hm1 n1 (gnum (mul k))
  where
    (k,n1,hm1,im1) = insert hm0 im0 n0 x

cseInsert :: (Eq k, Hashable k) => HashMap k GraphRef -> Int -> k -> (GraphRef, Int, HashMap k GraphRef)
cseInsert hm n gexpr = case HM.lookup gexpr hm of
  Nothing -> (GraphRef n, n+1, HM.insert gexpr (GraphRef n) hm)
  Just k -> (k, n, hm)


topSort :: HashMap (GExpr a GraphRef) GraphRef -> [(GraphRef, GExpr a GraphRef)]
topSort hm = map ((\(x,k,_) -> (GraphRef k,x)) . vertexToNode) (DataGraph.topSort graph)
  where
    (graph, vertexToNode) = DataGraph.graphFromEdges' $ map f (HM.toList hm)
      where
        f (gexpr, GraphRef k) = (gexpr, k, map (\(GraphRef n) -> n) $ getChildren gexpr)

getChildren :: GExpr a b -> [b]
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


-- | throw an error if there are repeated inputs
--getRedundantExprs :: (ToGExprs a, Eq b, Hashable b, NumT a ~ b) =>
--                     a -> IO (Maybe (HashSet (Expr b)))
--getRedundantExprs exprs_ = do
--  exprs <- readExprs exprs_
--  let redundant = snd $ foldl f (HS.empty, HS.empty) (toList exprs)
--        where
--          f (knownExprs, redundantExprs) expr
--            | HS.member expr knownExprs = (knownExprs, HS.insert expr redundantExprs)
--            | otherwise = (HS.insert expr knownExprs, redundantExprs)
--  return $ if HS.null redundant
--           then Nothing
--           else Just redundant
