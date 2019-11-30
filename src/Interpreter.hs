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

data GscM a = GscRunning ((GscThread, Int) -> (a, GscThread, Int)) | GscStopped GscThread | GscError String GscThread

instance Functor GscM where
  fmap f (GscRunning t) = GscRunning (\ thread -> let (x, env', stps) = t thread in (f x, env', pred stps))
  fmap _ (GscStopped t) = GscStopped t
  fmap _ (GscError e t) = GscError e t
    
instance Applicative GscM where
  pure x = GscRunning (\ (t, stps) -> (x, t, stps)) -- the number of steps to execute before swapping
  
  (GscRunning t0) <*> (GscRunning t1) = GscRunning (\ (thread, stps) -> let (f, thread', stps')   = t0 (thread, stps)
                                                                            (x, thread'', stps'') = t1 (thread', stps') in
                                                                            (f x, thread'', pred stps''))
  (GscStopped t)  <*> _               = GscStopped t
  (GscError e t)  <*> _               = GscError e t
  
  _               <*> (GscStopped t)  = GscStopped t
  _               <*> (GscError e t)  = GscError e t

instance Monad GscM where
  return         = pure
  
--(>>=)  :: m a -> (  a -> m b) -> m b
  -- (GscRunning t) >>= f = GscRunning (\ (thread, stps) -> let (x, thread', stps')    = t (thread, stps)
  --                                                            (GscRunning g)         = f x
  --                                                            (x', thread'', stps'') = g (thread', stps') in
  --                                                            (x', thread'', pred stps'))
  
  {-
     If g returns an error we want to return the error from this function
  -}
  (GscRunning f) >>= g =  
  
  (GscStopped t) >>= _ = GscStopped t
  (GscError e t) >>= _ = GscError e t
  
getState :: GscM GscThread
getState = GscRunning (\ (t, stps) -> (t, t, stps)) 

isDone :: GscM Bool
isDone = GscRunning (\ (t, stps) -> (stps == 0, t, stps))

checkDone :: GscM ()
checkDone = do done <- isDone
               when done $
                 do t <- getState
                    GscStopped t
                                                    
--gscError :: GscM String -> GscM ()
--gscError merr = do err <- merr
--                   t   <- getState
--                   GscError err t

gscError :: String -> GscM a
gscError msg = do t <- getState
                  GscError msg t

data Value = VString String
           | VList [Value]
           | VBool Bool
           | VInt Integer
           | VDouble Double
           deriving (Show, Eq)
           
type EvalErr = Either String

type OpInt = Integer -> Integer -> Integer
type OpDouble = Double -> Double -> Double

evalAdd :: Value -> Value -> GscM Value
evalAdd = evalOp (+) (+)

evalSub :: Value -> Value -> GscM Value
evalSub = evalOp (-) (-)

evalMul :: Value -> Value -> GscM Value
evalMul = evalOp (*) (*)

evalDiv :: Value -> Value -> GscM Value
evalDiv = evalOp div (/)

evalOp :: OpInt -> OpDouble -> Value -> Value -> GscM Value
evalOp opi _   (VInt v1)    (VInt v2)    = return (VInt (v1 `opi` v2))
evalOp _   opd (VDouble v1) (VInt v2)    = return (VDouble (v1 `opd` (fromIntegral v2 :: Double)))
evalOp _   opd (VInt v1)    (VDouble v2) = return (VDouble ((fromIntegral v1 :: Double) `opd` v2))
evalOp _   opd (VDouble v1) (VDouble v2) = return (VDouble (v1 `opd` v2))
evalOp _   _   _            _            = gscError "Incompatible types"

evalBinOp :: BinOp -> Value -> Value -> GscM Value
evalBinOp Add      = evalAdd
evalBinOp Subtract = evalSub
evalBinOp Multiply = evalMul
evalBinOp Divide   = evalDiv

--evalExpr :: Expr -> GscThread -> Int -> EvalErr (Value, GscThread, Int)
--evalExpr (StringLit v) t stps      = Right (VString v, t, stps)
--evalExpr (BoolLit v)   t stps      = Right (VBool v, t, stps)
--evalExpr (IntLit v)    t stps      = Right (VInt v, t, stps)
--evalExpr (FloatLit v)  t stps      = Right (VDouble v, t, stps)
--evalExpr (Binary op e1 e2) t stps  = do (val1, t1, stps1) <- evalExpr e1 t  stps
--                                        (val2, t2, stps2) <- evalExpr e2 t1 stps1
--                                        v'                <- evalBinOp op val1 val2
--                                        return (v', t, pred stps2)

evalExpr :: Expr -> Either String Value
evalExpr expr = runGscM (evalExpr2 (return expr))
                                        
evalExpr2 :: GscM Expr -> GscM Value
evalExpr2 mexpr = do expr <- mexpr
                     case expr of
                       (IntLit i)         -> return (VInt i)
                       (StringLit s)      -> return (VString s)
                       (Binary Add e1 e2) -> do v1 <- evalExpr2 (return e1)
                                                v2 <- evalExpr2 (return e2)
                                                case (v1, v2) of
                                                  (VInt i1, VInt i2) -> return (VInt (i1 + i2))
                                                  _                  -> gscError "Type Error"
                                                  
startThreadState :: GscThread
startThreadState = GscThread 50 [] empty [] (return ())

runGscM :: GscM a -> Either String a
runGscM (GscRunning f) = case f (startThreadState, 50) of
                              (res, _, _) -> Right res
runGscM (GscError e t) = Left e
runGscM (GscStopped t) = Left "Stopped"

{-
Problem: need to get the current state into this eval, you need the values of the 
         variables that could be used in the expression you're evaluating

How does Haskell know when I say "stmt <- mstmt" that I want to get the statement out 
of the tuple in mstmt?

evalStmt :: GscM Stmt -> GscM ()
evalStmt mstmt = do stmt <- mstmt
                    case stmt of
                      as@(Assign _ _) -> evalAssignStmt stmt

evalAssignStmt :: Stmt -> GscM ()
evalAssignStmt (Assign lv expr) = do stmt <- 
                                     let v = evalExpr expr in
                                     store lv v 
-}