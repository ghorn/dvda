{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}

module Dvda.Llvm ( VList(..)
                 , withLlvmJit
                 , toLlvmAsm
                 ) where

import Control.Monad ( unless )
import Control.Monad.Trans.Error
import qualified Data.Foldable as F
import Data.Traversable ( Traversable )
import qualified Data.Vector as V
import Foreign.Ptr ( FunPtr ) -- Ptr, castFunPtr )

import LLVM.General ( withModuleFromAST, moduleString )
import LLVM.General.Analysis ( verify )
import LLVM.General.Context ( withContext )
import LLVM.General.ExecutionEngine ( withJIT, withModuleInEngine, getFunction )
import LLVM.General.PassManager ( defaultCuratedPassSetSpec, runPassManager, withPassManager )

--import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import LLVM.General.AST hiding ( Mul, Add, Sub )
import LLVM.General.AST.AddrSpace ( AddrSpace(..) )
import LLVM.General.AST.CallingConvention ( CallingConvention ( C ) )
import LLVM.General.AST.Constant ( Constant ( Float ) )
import LLVM.General.AST.Linkage ( Linkage ( External ) )
import LLVM.General.AST.Float (  SomeFloat ( Double ) )
import LLVM.General.AST.Visibility ( Visibility( Default ) )

import Dvda.Expr ( GExpr(..), Floatings(..), Nums(..), Fractionals(..) )
import Dvda.Algorithm.Construct ( Algorithm(..), InputIdx(..), OutputIdx(..), AlgOp(..), Node(..) )

doubleType :: Type
doubleType = FloatingPointType 64 IEEE

allIntrinsics :: [Definition]
allIntrinsics = [ unIntrinsic "exp"
                , unIntrinsic "log"
                , unIntrinsic "sin"
                , unIntrinsic "cos"
                , unIntrinsic "fabs"
                , binIntrinsic "pow"
                ]
  where
    intrinsic :: Type -> Int -> String -> Definition
    intrinsic typ nInputs name =
      GlobalDefinition
      Function { G.linkage = External
               , G.visibility = Default
               , G.callingConvention = C
               , G.returnAttributes = []
               , G.returnType = typ
               , G.name = Name $ "llvm."++name++".f64"
               , G.parameters = (replicate nInputs (Parameter typ (Name "") []),False)
               , G.functionAttributes = []
               , G.section = Nothing
               , G.alignment = 0
               , G.basicBlocks = []}

    binIntrinsic :: String -> Definition
    binIntrinsic = intrinsic doubleType 2

    unIntrinsic :: String -> Definition
    unIntrinsic = intrinsic doubleType 1

newtype VList a = VList {mkVList :: V.Vector (V.Vector a)} deriving (Functor, F.Foldable, Traversable)

toLlvmFun :: String -> Algorithm Double -> Global
toLlvmFun functionName alg = mainFun
  where
    mainFun = Function External Default C [] (IntegerType 32) (Name functionName)
              ( map toParameter $ ["input_"  ++ show k | k <- take nInputs  [(0::Int)..]] ++
                                  ["output_" ++ show k | k <- take nOutputs [(0::Int)..]]
              , False)
              [] Nothing 0 [bb]
    toParameter name = Parameter (PointerType doubleType (AddrSpace 0)) (Name name) []

    nInputs  = 1
    nOutputs = 1

    bb = BasicBlock (Name "entry") (concatMap toInstruction (algOps alg))
         (Do $ Ret (Just (ConstantOperand (C.Int 32 0))) [])

--foreign import ccall "dynamic" mkIOStub ::
--  FunPtr (Ptr Double -> Ptr Double -> IO Word32) -> Ptr Double -> Ptr Double -> IO Word32

--runJIT :: ((V.Vector (Ptr Double) -> V.Vector (Ptr Double) -> IO Word32) -> IO a) -> AST.Module
--       -> IO (Either String a)
runJIT :: (FunPtr () -> IO a) -> Module -> IO (Either String a)
runJIT userFun mAST = do
  let --withEE = \c -> withMCJIT c Nothing Nothing Nothing Nothing
      withEE = flip withJIT 2
  withContext $ \context -> withEE context $ \executionEngine ->
    runErrorT $ withModuleFromAST context mAST $ \m ->
      withModuleInEngine executionEngine m $ \em -> do
        Just p <- getFunction em (Name "my_function")
--        let f = mkIOStub (castFunPtr p)
        userFun p

-- | you're on your own for making sure the FunPtr has the right number of inputs/outputs
--   until I can figure out a polyvariadic foreign import "dynamic"
withLlvmJit :: Algorithm Double -> (FunPtr () -> IO a) -> IO (Either String a)
withLlvmJit fg userfun = do
  let fundef = toLlvmFun "my_function" fg
      myModule = defaultModule { moduleName = "come_on_lets_compile"
                               , moduleDefinitions = allIntrinsics ++ [GlobalDefinition fundef]
                               }
  runJIT userfun myModule


toLlvmAsm :: Algorithm Double -> IO ()
toLlvmAsm fg = do
  let fundef = toLlvmFun "my_function" fg
      myModule = defaultModule { moduleName = "come_on_lets_compile"
                               , moduleDefinitions = allIntrinsics ++ [GlobalDefinition fundef]
                               }

  str <- withContext $ \context ->
    runErrorT $ withModuleFromAST context myModule $ \mdl -> do
      --putStrLn "------------------- IR: -----------------"
      --ms <- moduleString mdl
      --putStrLn ms
      let pss = defaultCuratedPassSetSpec
      ret <- withPassManager pss $ \pm -> runPassManager pm mdl
      unless ret (error "pass manager failed")
      vmdl <- runErrorT $ verify mdl
      case vmdl of Left err -> putStrLn ("ERROR:\n" ++ err) >> return Nothing
                   Right _ -> do
                     ms' <- moduleString mdl
                     return (Just ms')

  putStrLn $ case str of
    (Left x) -> x ++ "\n\n----invalid IR----"
    (Right Nothing) -> "invalid module"
    (Right (Just x)) -> x ++ "\n\n------- everything seems to have worked -----"


nameNode :: Node -> String
nameNode (Node k) = 'w' : show k

toInstruction :: AlgOp Double -> [Named Instruction]
toInstruction algOp = instruction algOp
  where
    bin k x y op =
      [Name (nameNode k) :=
       op (LocalReference (Name (nameNode x))) (LocalReference (Name (nameNode y))) []]

    makeDouble k c =
      [Name (nameNode k) :=
       FAdd (ConstantOperand (Float (Double c))) (ConstantOperand (Float (Double 0))) []]

    instruction (InputOp k (InputIdx idx)) =
      [ nm :=
        GetElementPtr { inBounds = True
                      , address = LocalReference (Name "input_0")
                      , indices = [ConstantOperand (C.Int 64 (fromIntegral idx))]
                      , metadata = []
                      }
      , Name (nameNode k) :=
        Load { volatile = False
             , address = LocalReference nm
             , maybeAtomicity = Nothing
             , alignment = 0
             , metadata = []
             }
      ]
      where
        nm = Name $ "tmp_input_" ++ nameNode k
    instruction (OutputOp k (OutputIdx idx)) =
      [ nm :=
        GetElementPtr { inBounds = True
                      , address = LocalReference (Name ("output_0"))
                      , indices = [ConstantOperand (C.Int 64 (fromIntegral idx))]
                      , metadata = []
                      }
      , Name "" :=
        Store { volatile = False
              , address = LocalReference nm
              , value = LocalReference (Name (nameNode k))
              , maybeAtomicity =  Nothing
              , alignment = 0
              , metadata = []
              }
      ]
      where
        nm = Name $ "tmp_output_" ++ nameNode k

    instruction (NormalOp k (GConst c)) = makeDouble k c
    instruction (NormalOp k (GNum (Mul x y))) = bin k x y FMul
    instruction (NormalOp k (GNum (Add x y))) = bin k x y FAdd
    instruction (NormalOp k (GNum (Sub x y))) = bin k x y FSub
    instruction (NormalOp k (GNum (Negate x))) = [Name (nameNode k) := FSub (ConstantOperand (Float (Double 0))) (LocalReference (Name (nameNode x))) []]
    instruction (NormalOp k (GNum (FromInteger x))) = makeDouble k (fromIntegral x)
    instruction (NormalOp k (GFractional (Div x y))) = bin k x y FDiv
    instruction (NormalOp k (GFractional (FromRational x))) = makeDouble k (fromRational x)

    instruction (NormalOp k (GNum (Abs x)))    = callUn k x "fabs"
    instruction (NormalOp _ (GNum (Signum _))) = error "llvm backend doesn't yet support signum"
    instruction (NormalOp k (GFloating (Pow x y))) = callBin k x y "pow"
    instruction (NormalOp _ (GFloating (LogBase _ _))) = error "llvm backend doesn't yet support logbase"
    instruction (NormalOp k (GFloating (Exp x))) = callUn k x "exp"
    instruction (NormalOp k (GFloating (Log x))) = callUn k x "log"
    instruction (NormalOp k (GFloating (Sin x))) = callUn k x "sin"
    instruction (NormalOp k (GFloating (Cos x))) = callUn k x "cos"
    instruction (NormalOp _ (GFloating (ASin _)))  = error "llvm backend doesn't yet support asin"
    instruction (NormalOp _ (GFloating (ATan _)))  = error "llvm backend doesn't yet support atan"
    instruction (NormalOp _ (GFloating (ACos _)))  = error "llvm backend doesn't yet support acos"
    instruction (NormalOp _ (GFloating (Sinh _)))  = error "llvm backend doesn't yet support sinh"
    instruction (NormalOp _ (GFloating (Cosh _)))  = error "llvm backend doesn't yet support cosh"
    instruction (NormalOp _ (GFloating (Tanh _)))  = error "llvm backend doesn't yet support tanh"
    instruction (NormalOp _ (GFloating (ASinh _))) = error "llvm backend doesn't yet support ASinh"
    instruction (NormalOp _ (GFloating (ATanh _))) = error "llvm backend doesn't yet support ATanh"
    instruction (NormalOp _ (GFloating (ACosh _))) = error "llvm backend doesn't yet support ACosh"
    instruction (NormalOp _ (GSym _)) = error "dvda internal error: GSym found in algorithm"

    callUn :: Node -> Node -> String -> [Named Instruction]
    callUn k x name =
      [Name (nameNode k) := Call
      { isTailCall = False
      , callingConvention = C
      , returnAttributes = []
      , function = Right (ConstantOperand (C.GlobalReference (Name ("llvm."++name++".f64"))))
      , arguments = [(LocalReference (Name (nameNode x)),[])]
      , functionAttributes = []
      , metadata = []
      }]

    callBin :: Node -> Node -> Node -> String -> [Named Instruction]
    callBin k x y name =
      [Name (nameNode k) := Call
      { isTailCall = False
      , callingConvention = C
      , returnAttributes = []
      , function = Right (ConstantOperand (C.GlobalReference (Name ("llvm."++name++".f64"))))
      , arguments = [ (LocalReference (Name (nameNode x)),[])
                    , (LocalReference (Name (nameNode y)),[])
                    ]
      , functionAttributes = []
      , metadata = []
      }]
