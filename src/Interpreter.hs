{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import Data.Map
import Control.Monad.Except;
import LanguageStructure

type GscEnv = Map LValue Expr
data GscState = GscState [Stmt] GscEnv (IO ())
data GscProcess = GscPRunning GscState | GscPError String GscState
data GscAppState = GscAppState Int [GscM ()] (Maybe GscEnv)

data GscMResult a = GscVal a | GscErr
newtype GscM a = GscM (GscProcess -> (GscMResult a, GscProcess)) 

instance Functor GscM where
  fmap g (GscM f) = GscM (\ t -> case f t of
                                   (GscVal x, t') -> (GscVal (g x), t')
                                   (GscErr, t')   -> (GscErr, t')) 

instance Applicative GscM where
  pure x = GscM (GscVal x, )
  
  (GscM f) <*> (GscM g) = GscM (\ t -> case f t of
                                         (GscErr, t')     -> (GscErr, t')
                                         (GscVal fun, t') -> case g t' of
                                                               (GscErr, t'')     -> (GscErr, t'')
                                                               (GscVal val, t'') -> (GscVal (fun val), t''))

instance Monad GscM where
  return         = pure
  
  (GscM f) >>= g = GscM (\ t -> case f t of
                                  (GscVal rx, GscPRunning t') -> let GscM f' = g rx
                                                                   in f' t 
                                  (_, GscPError s t')   -> (GscErr, GscPError s t'))


getState :: GscM GscProcess
getState = GscM (\ t -> (GscVal t, t))

gscError :: String -> GscM a
gscError err = GscM (\ t -> case t of
                              (GscPRunning st)    -> (GscErr, GscPError err st)
                              (GscPError err' st) -> (GscErr, GscPError err' st))

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

evalExpr :: Expr -> Either String Value
evalExpr expr = runGscM (evalExpr2 (return expr))

evalExpr2 :: GscM Expr -> GscM Value
evalExpr2 mexpr = do expr <- mexpr
                     case expr of
                       (IntLit i)        -> return (VInt i)
                       (FloatLit n)      -> return (VDouble n)
                       (StringLit s)     -> return (VString s)
                       (Binary op e1 e2) -> do v1 <- evalExpr2 (return e1)
                                               v2 <- evalExpr2 (return e2)
                                               evalBinOp op v1 v2 

startThreadState :: GscProcess
startThreadState = GscPRunning (GscState [] empty (return ()))

runGscM :: GscM a -> Either String a
runGscM (GscM f) = case f startThreadState of
                     (GscErr, GscPError err st) -> Left err
                     (GscVal v, _)              -> Right v

