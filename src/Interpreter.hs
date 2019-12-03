{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import Data.Map
import Control.Monad.Except;
import LanguageStructure

{-
Threads
 - need a way to keep track of the number of 'steps' taken while evaluating so
   you can evaluate for a fixed number of steps before switching to another
   thread
   
 - want a monad that can handle the environment, errors, and thread running out
   of steps

runThread :: Int -> GscThread -> GscThread
-}


type GscEnv = Map LValue Expr

data GscThreadState = GscThreadState [Stmt] GscEnv [String] (IO ())

-- GscThread (steps remaining, things to execute, environment, events, application output)
data GscThread = GscTRunning Int GscThreadState | GscTPaused GscThreadState | GscTError String GscThreadState

-- GscAppState CurrentThread [Threads: ] (possibly some global state)
data GscAppState = GscAppState Int [GscM ()] (Maybe GscEnv)

data GscMResult a = GscVal a | GscErr
newtype GscM a = GscM (GscThread -> (a, GscThread)) 

instance Functor GscM where
  --fmap f (GscM t) = GscM (\ thread -> let (x, env') = t thread in (f x, env', pred stps))
  fmap g (GscM f) = GscM (\ t -> let (x, t') = f t in
                                 (g x, t')) 
    
instance Applicative GscM where
  pure x = GscM (x, ) 
  
 -- (GscM t0) <*> (GscM t1) = GscM (\ thread -> let (f, thread', stps')   = t0 (thread, stps)
 --                                                 (x, thread'', stps'') = t1 (thread', stps') in
 --                                                 (f x, thread'', pred stps''))
 
  (GscM f0) <*> (GscM f1) = GscM (\ t -> case f0 t of
                                           (f, GscTRunning stps st) -> case f1 (GscTRunning stps st) of
                                                                         (x, GscTRunning stps' st') -> (f x, GscTRunning stps' st'))
                                                                        -- (_, GscTPaused st)         -> ((), GscTPaused st)
                                                                        -- (_, GscTError err st)      -> ((), GcsTError err st)
                                           -- (_, GscTPaused st)       -> GscTPaused st
                                           -- (_, GscTError err st)    -> GscTError err st)

instance Monad GscM where
  return         = pure
  
  {-
     If g returns an error we want to return the error from this function
  -}
  (GscM f) >>= g = GscM bnd
    where
      checkSwapThread x (GscTRunning 0 st)    = (GscErr, GscTPaused st)
      checkSwapThread x (GscTRunning stps st) = (x, GscTRunning (pred stps) st)
      checkSwapThread x t                     = (GscErr, t)
      
      bnd t@(GscTRunning stps st) = case f t of
                                      (x, t' @ (GscTRunning stps' st')) -> case g x of
                                                                             (GscM f') -> f' x
                                      (_, t')                           -> (GscErr, t')
      bnd t = (GscErr, t) -- GscErr needs to be the same type as the return type of g's monad, how?


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

-- runGscM :: GscM a -> Either String a
-- runGscM (GscRunning f) = case f (startThreadState, 50) of
--                               (res, _, _) -> Right res
-- runGscM (GscError e t) = Left e
-- runGscM (GscStopped t) = Left "Stopped"


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