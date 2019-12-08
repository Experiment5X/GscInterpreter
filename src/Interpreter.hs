{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import Data.Map
import Data.Bits
import Control.Monad.Except;
import LanguageStructure

type Identifier = String

type GscEnv = Map Identifier Value
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
                                  (GscVal rx, t'@(GscPRunning _)) -> let GscM f' = g rx
                                                                     in f' t'
                                  (_, t'@(GscPError _ _))         -> (GscErr, t'))


getState :: GscM GscProcess
getState = GscM (\ t -> (GscVal t, t))

putState :: GscProcess -> GscM ()
putState t = GscM (const (GscVal (), t))

getEnv :: GscM GscEnv
getEnv = do st <- getState
            case st of
              (GscPRunning (GscState _ env _)) -> return env
              (GscPError _ (GscState _ env _)) -> return env

getValue :: Identifier -> GscM Value
getValue i = do st <- getState
                case st of
                  (GscPRunning (GscState _ env _)) -> case Data.Map.lookup i env of
                                                        (Just val) -> return val
                                                        Nothing    -> gscError ("Identifier " ++ show i ++ " not defined")

putValue :: Identifier -> Value -> GscM ()
putValue i v = do st <- getState
                  let (GscPRunning (GscState stmts env io)) = st
                      env'                                  = Data.Map.insert i v env
                      in do putState (GscPRunning (GscState stmts env' io))
                            return ()

gscError :: String -> GscM a
gscError err = GscM (\ t -> case t of
                              (GscPRunning st)    -> (GscErr, GscPError err st)
                              (GscPError err' st) -> (GscErr, GscPError err' st))

data Value = VString String
           | VList [Value]
           | VBool Bool
           | VInt Integer
           | VDouble Double
           | VRef ReferenceValue
           deriving (Show, Eq, Ord)
           
data ReferenceValue = RVObj (Map Value Value) deriving (Show, Eq, Ord)

type EvalErr = Either String

type OpInt    = Integer -> Integer -> Integer
type OpDouble = Double  -> Double  -> Double
type OpBool   = Bool    -> Bool    -> Bool

implicitBoolConvert :: Value -> Value
implicitBoolConvert (VBool b)    = VBool b
implicitBoolConvert (VInt 0)     = VBool False
implicitBoolConvert (VInt _)     = VBool True
implicitBoolConvert (VDouble 0)  = VBool False
implicitBoolConvert (VDouble _)  = VBool True
implicitBoolConvert (VString "") = VBool False
implicitBoolConvert (VString _)  = VBool True

evalListLit :: [Expr] -> GscM Value
evalListLit = mkObj (VInt 0) empty
  where
    mkObj _ obj []                  = return (VRef (RVObj obj))
    mkObj (VInt i) obj (expr:exprs) = do v <- evalExpr expr
                                         mkObj (VInt (succ i)) (insert (VInt i) v obj) exprs

evalAdd :: Value -> Value -> GscM Value
evalAdd = evalOpArith (+) (+)

evalSub :: Value -> Value -> GscM Value
evalSub = evalOpArith (-) (-)

evalMul :: Value -> Value -> GscM Value
evalMul = evalOpArith (*) (*)

evalDiv :: Value -> Value -> GscM Value
evalDiv = evalOpArith div (/)

evalMod :: Value -> Value -> GscM Value
evalMod = evalBinInt mod

evalShiftLeft :: Value -> Value -> GscM Value
evalShiftLeft = evalBinInt (\ v1 v2 -> v1 `shift` fromIntegral v2)

evalShiftRight :: Value -> Value -> GscM Value
evalShiftRight = evalBinInt (\ v1 v2 -> v1 `shift` fromIntegral (-v2))

evalAAnd :: Value -> Value -> GscM Value
evalAAnd = evalBinInt (.&.)

evalAOr :: Value -> Value -> GscM Value
evalAOr = evalBinInt (.|.)

evalAXor :: Value -> Value -> GscM Value
evalAXor = evalBinInt xor

evalBAnd :: Value -> Value -> GscM Value
evalBAnd = evalOpBool (&&)

evalBOr :: Value -> Value -> GscM Value
evalBOr = evalOpBool (||)

evalBinInt :: OpInt -> Value -> Value -> GscM Value
evalBinInt op (VInt v1) (VInt v2) = return (VInt (v1 `op` v2))
evalBinInt _  _         _         = gscError "Incompatible types"

evalOpArith :: OpInt -> OpDouble -> Value -> Value -> GscM Value
evalOpArith opi _   (VInt v1)    (VInt v2)    = return (VInt (v1 `opi` v2))
evalOpArith _   opd (VDouble v1) (VInt v2)    = return (VDouble (v1 `opd` (fromIntegral v2 :: Double)))
evalOpArith _   opd (VInt v1)    (VDouble v2) = return (VDouble ((fromIntegral v1 :: Double) `opd` v2))
evalOpArith _   opd (VDouble v1) (VDouble v2) = return (VDouble (v1 `opd` v2))
evalOpArith _   _   _            _            = gscError "Incompatible types"

evalOpBool :: OpBool -> Value -> Value -> GscM Value
evalOpBool op v1 v2 = let (VBool b1) = implicitBoolConvert v1
                          (VBool b2) = implicitBoolConvert v2 in
                          return (VBool (b1 `op` b2))

evalBinOp :: BinOp -> Value -> Value -> GscM Value
evalBinOp Add        = evalAdd
evalBinOp Subtract   = evalSub
evalBinOp Multiply   = evalMul
evalBinOp Divide     = evalDiv
evalBinOp Modulus    = evalMod
evalBinOp ShiftLeft  = evalShiftLeft
evalBinOp ShiftRight = evalShiftRight
evalBinOp AAnd       = evalAAnd
evalBinOp AOr        = evalAOr
evalBinOp AXor       = evalAXor
evalBinOp BAnd       = evalBAnd
evalBinOp BOr        = evalBOr

evalExpr2 :: Expr -> GscEnv -> Either String Value
evalExpr2 expr = runGscMWithEnv (evalExpr expr)

evalStmt2 :: Stmt -> GscEnv -> Either String GscEnv
evalStmt2 stmt = runGscMWithEnv (evalStmt stmt >> getEnv)

evalExpr :: Expr -> GscM Value
evalExpr (BoolLit b)       = return (VBool b)
evalExpr (IntLit i)        = return (VInt i)
evalExpr (FloatLit n)      = return (VDouble n)
evalExpr (StringLit s)     = return (VString s)
evalExpr (ListLit es)      = evalListLit es
evalExpr (Binary op e1 e2) = do v1 <- evalExpr e1
                                v2 <- evalExpr e2
                                evalBinOp op v1 v2
evalExpr (Var (LValue _ [LValueComp i []])) = getValue i

evalMExpr :: GscM Expr -> GscM Value
evalMExpr mexpr = do expr <- mexpr
                     evalExpr expr
                     

evalPutIntoLValue :: Value -> LValue -> GscM ()
evalPutIntoLValue v (LValue q [LValueComp i []])  = putValue i v
evalPutIntoLValue v (LValue q [LValueComp i [e]]) = do vidx <- evalExpr e
                                                       obj  <- getValue i
                                                       evalPutIntoObj vidx v i obj
  where
    evalPutIntoObj vidx v i (VRef (RVObj obj)) = putValue i (VRef (RVObj (insert vidx v obj)))
    evalPutIntoObj _    _ _ _                  = gscError "Type is not indexable, must be an object/array"

evalAssignEquals :: (Value -> Value -> GscM Value) -> LValue -> Expr -> GscM ()
evalAssignEquals evalOp (LValue _ [LValueComp i []]) expr = do v1 <- getValue i
                                                               v2 <- evalExpr expr
                                                               vf <- evalOp v1 v2
                                                               putValue i vf

evalStmt :: Stmt -> GscM ()
evalStmt (Assign lv expr)       = do v <- evalExpr expr
                                     evalPutIntoLValue v lv
                                     
evalStmt (PlusEquals lv expr)   = evalAssignEquals evalAdd lv expr
evalStmt (MinusEquals lv expr)  = evalAssignEquals evalSub lv expr
evalStmt (TimesEquals lv expr)  = evalAssignEquals evalMul lv expr
evalStmt (DivideEquals lv expr) = evalAssignEquals evalDiv lv expr
evalStmt (ModEquals lv expr)    = evalAssignEquals evalMod lv expr
evalStmt (AndEquals lv expr)    = evalAssignEquals evalAAnd lv expr
evalStmt (OrEquals lv expr)     = evalAssignEquals evalAOr lv expr
evalStmt (XorEquals lv expr)    = evalAssignEquals evalAXor lv expr

evalStmt (AssignExprStmt (PostInc (Var lv))) = evalAssignEquals evalAdd lv (IntLit 1)
evalStmt (AssignExprStmt (PreInc (Var lv)))  = evalAssignEquals evalAdd lv (IntLit 1)
evalStmt (AssignExprStmt (PostDec (Var lv))) = evalAssignEquals evalSub lv (IntLit 1)
evalStmt (AssignExprStmt (PreDec (Var lv)))  = evalAssignEquals evalSub lv (IntLit 1)

startThreadState :: GscProcess
startThreadState = startThreadStateEnv empty

startThreadStateEnv :: Map Identifier Value -> GscProcess
startThreadStateEnv env = GscPRunning (GscState [] env (return ()))

runGscMWithEnv :: GscM a -> GscEnv -> Either String a
runGscMWithEnv (GscM f) env = case f (startThreadStateEnv env) of
                                (GscErr, GscPError err st) -> Left err
                                (GscVal v, _)              -> Right v

runGscM :: GscM a -> Either String a
runGscM mgsc = let env = Data.Map.fromList [("a", VInt 5), ("b", VInt 23)]
               in runGscMWithEnv mgsc env
