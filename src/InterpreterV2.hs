module InterpreterV2 where

import LanguageStructure
import Control.Monad.State
import Data.Map


data Value = VString String
           | VList [Value]
           | VBool Bool
           | VInt Integer
           | VDouble Double
           deriving (Show, Eq)


type GscEnv = Map LValue Expr

-- GscThread (steps remaining, things to execute, environment, events, application output)
data GscThreadState = GscThreadState Int [Stmt] GscEnv [String] (IO ()) 

-- GscAppState CurrentThread [Threads: ] (possibly some global state)
data GscAppState = GscAppState Int [GscThreadState] (Maybe GscEnv)

evalExpr :: State GscThreadState Expr -> State GscThreadState Value
evalExpr mexpr = do expr <- mexpr
                    case expr of
                      (IntLit i)         -> return (VInt i)
                      (Binary Add e1 e2) -> do v1 <- evalExpr (return e1)
                                               v2 <- evalExpr (return e2)
                                               case (v1, v2) of
                                                 (VInt i1, VInt i2) -> return (VInt (i1 + i2))
                                                 _                  -> return (VString "Error")

countThreadTick :: State GscThreadState Int
countThreadTick = do (GscThreadState stps stmts env evts io) <- get
                     let newStps = pred stps
                     put (GscThreadState newStps stmts env evts io)
                     return newStps
                      
 

startThreadState :: GscThreadState
startThreadState = GscThreadState 50 [] empty [] (return ())