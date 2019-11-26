{-# LANGUAGE TupleSections #-}

module Interpreter where

import Data.Map
import Control.Monad.Except;
import LanguageStructure

{-
Threads
 - need a way to keep track of the number of 'steps' taken while evaluating so
   you can evaluate for a fixed number of steps before switching to another
   thread

runThread :: Int -> GscThread -> GscThread
-}


type GscEnv = Map LValue Expr

-- GscThread (steps remaining, things to execute, environment, events, application output)
data GscThread = GscThread Int [Stmt] GscEnv [String] (IO ()) | GscThreadTest

-- GscAppState CurrentThread [Threads: ] (possibly some global state)
data GscAppState = GscAppState Int [GscM ()] (Maybe GscEnv)

newtype GscM a = GscM (GscThread -> (a, GscThread, Int))

instance Functor GscM where
  fmap f (GscM t) = GscM (\ thread -> let (x, env', stps) = t thread in (f x, env', pred stps))
    
instance Applicative GscM where
  pure x = GscM (x, , 50) -- the number of steps to execute before swapping
  
  (GscM t0) <*> (GscM t1) = GscM (\ thread -> let (f, thread', stps)   = t0 thread
                                                  (x, thread'', stps') = t1 thread' in
                                                  (f x, thread'', pred stps'))

instance Monad GscM where
  return         = pure
  
--(>>=)  :: m a -> (  a -> m b) -> m b
  (GscM t) >>= f = GscM (\ thread -> let (x, thread', stps) = t thread
                                         (GscM g)  = f x
                                         (x', thread'', stps') = g thread' in
                                         (x', thread'', pred stps'))

isDone :: GscM a -> GscM Bool
isDone (GscM t) = GscM (\ thread -> let (_, thread', stps) = t thread in
                                        (stps == 0, thread', stps))

data Value = VString String
           | VList [Value]
           | VBool Bool
           | VInt Integer
           | VDouble Double
           deriving (Show, Eq)
           
type EvalErr = Either String

type OpInt = Integer -> Integer -> Integer
type OpDouble = Double -> Double -> Double

evalAdd :: Value -> Value -> EvalErr Value
evalAdd = evalOp (+) (+)

evalSub :: Value -> Value -> EvalErr Value
evalSub = evalOp (-) (-)

evalMul :: Value -> Value -> EvalErr Value
evalMul = evalOp (*) (*)

evalDiv :: Value -> Value -> EvalErr Value
evalDiv = evalOp div (/)

evalOp :: OpInt -> OpDouble -> Value -> Value -> EvalErr Value
evalOp opi _   (VInt v1)    (VInt v2)    = Right (VInt (v1 `opi` v2))
evalOp _   opd (VDouble v1) (VInt v2)    = Right (VDouble (v1 `opd` (fromIntegral v2 :: Double)))
evalOp _   opd (VInt v1)    (VDouble v2) = Right (VDouble ((fromIntegral v1 :: Double) `opd` v2))
evalOp _   opd (VDouble v1) (VDouble v2) = Right (VDouble (v1 `opd` v2))
evalOp _   _   _            _            = Left "Incompatible types"

evalBinOp :: BinOp -> Value -> Value -> EvalErr Value
evalBinOp Add      = evalAdd
evalBinOp Subtract = evalSub
evalBinOp Multiply = evalMul
evalBinOp Divide   = evalDiv

evalExpr :: Expr -> GscThread -> Int -> EvalErr (Value, GscThread, Int)
evalExpr (StringLit v) t stps      = Right (VString v, t, stps)
evalExpr (BoolLit v)   t stps      = Right (VBool v, t, stps)
evalExpr (IntLit v)    t stps      = Right (VInt v, t, stps)
evalExpr (FloatLit v)  t stps      = Right (VDouble v, t, stps)
evalExpr (Binary op e1 e2) t stps  = do (val1, t1, stps1) <- evalExpr e1 t  stps
                                        (val2, t2, stps2) <- evalExpr e2 t1 stps1
                                        v'                <- evalBinOp op val1 val2
                                        return (v', t, pred stps2)
