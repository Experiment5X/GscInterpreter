{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import Data.Map
import Data.Bits
import qualified Data.List
import Control.Monad.Except
import LanguageStructure

type Identifier = String

type GscEnv = Map Identifier Value
data GscState = GscState [Stmt] GscEnv [StructureType] (IO ())
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

getIO :: GscM (IO ())
getIO = do st <- getState
           case st of
             (GscPRunning (GscState _ _ _ io)) -> return io
             (GscPError _ (GscState _ _ _ io)) -> return io

putIO :: IO () -> GscM ()
putIO io = do st <- getState
              case st of
                (GscPRunning (GscState stmts env sts io'))   -> putState (GscPRunning (GscState stmts env sts (io' >> io)))
                (GscPError err (GscState stmts env sts io')) -> putState (GscPError err (GscState stmts env sts (io' >> io)))

getEnv :: GscM GscEnv
getEnv = do st <- getState
            case st of
              (GscPRunning (GscState _ env _ _)) -> return env
              (GscPError _ (GscState _ env _ _)) -> return env

setEnv :: GscEnv -> GscM ()
setEnv env = do st <- getState
                case st of
                  (GscPRunning (GscState stmts _ sts io)) -> putState (GscPRunning (GscState stmts env sts io))
                  (GscPError err (GscState stmts _ sts io)) -> putState (GscPError err (GscState stmts env sts io))

getValue :: Identifier -> GscM Value
getValue i = do st <- getState
                case st of
                  (GscPRunning (GscState _ env _ _)) -> case Data.Map.lookup i env of
                                                          (Just val) -> return val
                                                          Nothing    -> gscError ("Identifier " ++ show i ++ " not defined")

putValue :: Identifier -> Value -> GscM ()
putValue i v = do st <- getState
                  let (GscPRunning (GscState stmts env stck io)) = st
                      env'                                       = Data.Map.insert i v env
                      in do putState (GscPRunning (GscState stmts env' stck io))
                            return ()

gscError :: String -> GscM a
gscError err = GscM (\ t -> case t of
                              (GscPRunning st)    -> (GscErr, GscPError err st)
                              (GscPError err' st) -> (GscErr, GscPError err' st))

data StructureType   = FunctionCall | Loop | If | Root deriving (Show, Eq)
data StatementResult = Success | BreakResult | ContinueResult | ReturnResult Value deriving (Show, Eq)

getCurStructType :: GscM StructureType
getCurStructType = do st <- getState
                      case st of
                        (GscPRunning (GscState _ _ [] _))     -> gscError "No current structure type"
                        (GscPRunning (GscState _ _ (c:cs) _)) -> return c
                        _                                     -> gscError "Cannot get struct type of errored process"

popCurStructType :: GscM StructureType
popCurStructType = do st <- getState
                      case st of
                        (GscPRunning (GscState _ _ [] _))            -> gscError "No current structure type"
                        (GscPRunning (GscState stmts env (c:cs) io)) -> do putState (GscPRunning (GscState stmts env cs io))
                                                                           return c
                        _                                            -> gscError "Cannot pop struct type of errored process"

pushCurStructType :: StructureType -> GscM ()
pushCurStructType c = do st <- getState
                         case st of
                           (GscPRunning (GscState _ _ [] _))        -> gscError "No current structure type"
                           (GscPRunning (GscState stmts env cs io)) -> putState (GscPRunning (GscState stmts env (c:cs) io))
                           _                                        -> gscError "Cannot pop struct type of errored process"

data Value = VString String
           | VList [Value]
           | VBool Bool
           | VInt Integer
           | VDouble Double
           | VVoid          -- used for returning nothing from a function
           | VRef Integer
           | VStore (Map Integer RVObj)
           | VFunctionDefs (Map Identifier Value)
           | VFunctionDef [Identifier] GscEnv Stmt 
               -- list of parameter names
               -- the environment where the function is defined
               -- the statement inside the function
           deriving (Eq, Ord)

instance Show Value where
  show (VString s)             = show s
  show (VList vs)              = "[" ++ show vs ++ "]"
  show (VBool b)               = show b
  show (VInt i)                = show i
  show (VDouble d)             = show d
  show VVoid                   = "void"
  show (VRef o)                = "Object_" ++ show o
  show (VStore s)              = show s
  show (VFunctionDefs ds)      = show ds
  show (VFunctionDef args _ _) = "::(" ++ unwords (Prelude.map show args) ++ ")"

type RVObj = Map Value Value

type EvalErr = Either String

type OpInt     = Integer -> Integer -> Integer
type OpDouble  = Double  -> Double  -> Double
type OpCInt    = Integer -> Integer -> Bool
type OpCDouble = Double  -> Double  -> Bool
type OpBool    = Bool    -> Bool    -> Bool

putFunctionDef :: Identifier -> Value -> GscM ()
putFunctionDef fname fdef = do fdefs <- getValue functionDefsIdent
                               case fdefs of
                                 (VFunctionDefs defs) -> let defs' = insert fname fdef defs
                                                         in putValue functionDefsIdent (VFunctionDefs defs')
                                 _                    -> gscError "Invalid function defs type"

getFunctionDef :: Identifier -> GscM Value
getFunctionDef fname = do fdefs <- getValue functionDefsIdent
                          case fdefs of
                            (VFunctionDefs defs) -> case Data.Map.lookup fname defs of
                                                      (Just fdef) -> return fdef
                                                      Nothing     -> gscError ("No function named " ++ fname)
                            _                    -> gscError "Invalid function defs type"

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
    mkObj _ obj [] = do vnxtId <- getValue nextObjIdIdent
                        case vnxtId of
                          (VInt nxtId) -> do putValue nextObjIdIdent (VInt (succ nxtId))
                                             vstore <- getValue storeIdent
                                             case vstore of
                                               (VStore store) -> do putValue storeIdent (VStore (insert nxtId obj store))
                                                                    return (VRef nxtId)
    mkObj (VInt i) obj (expr:exprs) = do v <- evalExpr expr
                                         mkObj (VInt (succ i)) (insert (VInt i) v obj) exprs

evalAdd :: Value -> Value -> GscM Value
evalAdd (VString s1) (VString s2) = return (VString (s1 ++ s2))
evalAdd (VString s1) v2           = return (VString (s1 ++ show v2))
evalAdd v1           v2           = evalOpArith (+) (+) v1 v2

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

evalLess :: Value -> Value -> GscM Value
evalLess = evalOpComp (<) (<)

evalLessEq :: Value -> Value -> GscM Value
evalLessEq = evalOpComp (<=) (<=)

evalEq :: Value -> Value -> GscM Value
evalEq = evalOpComp (==) (==)

evalNotEq :: Value -> Value -> GscM Value
evalNotEq = evalOpComp (/=) (/=)

evalGreaterEq :: Value -> Value -> GscM Value
evalGreaterEq = evalOpComp (>=) (>=)

evalGreater :: Value -> Value -> GscM Value
evalGreater = evalOpComp (>) (>)

evalBinInt :: OpInt -> Value -> Value -> GscM Value
evalBinInt op (VInt v1) (VInt v2) = return (VInt (v1 `op` v2))
evalBinInt _  _         _         = gscError "Incompatible types"

evalOpArith :: OpInt -> OpDouble -> Value -> Value -> GscM Value
evalOpArith opi _   (VInt v1)    (VInt v2)    = return (VInt (v1 `opi` v2))
evalOpArith _   opd (VDouble v1) (VInt v2)    = return (VDouble (v1 `opd` (fromIntegral v2 :: Double)))
evalOpArith _   opd (VInt v1)    (VDouble v2) = return (VDouble ((fromIntegral v1 :: Double) `opd` v2))
evalOpArith _   opd (VDouble v1) (VDouble v2) = return (VDouble (v1 `opd` v2))
evalOpArith _   _   _            _            = gscError "Incompatible types"

evalOpComp :: OpCInt -> OpCDouble -> Value -> Value -> GscM Value
evalOpComp opi _   (VInt v1)    (VInt v2)    = return (VBool (v1 `opi` v2))
evalOpComp _   opd (VDouble v1) (VInt v2)    = return (VBool (v1 `opd` (fromIntegral v2 :: Double)))
evalOpComp _   opd (VInt v1)    (VDouble v2) = return (VBool ((fromIntegral v1 :: Double) `opd` v2))
evalOpComp _   opd (VDouble v1) (VDouble v2) = return (VBool (v1 `opd` v2))
evalOpComp _   _   _            _            = gscError "Incompatible types"

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
evalBinOp Less       = evalLess
evalBinOp LessEq     = evalLessEq
evalBinOp Equal      = evalEq
evalBinOp NotEqual   = evalNotEq
evalBinOp GreaterEq  = evalGreaterEq
evalBinOp Greater    = evalGreater

valToString :: Value -> GscM String
valToString (VRef oid) = do obj <- getObject oid
                            let kvs      = toList obj
                                (ks, vs) = unzip kvs
                            kstrs <- mapM valToString ks
                            vstrs <- mapM valToString vs
                            let kvstrs   = Prelude.map (\ (k, v) -> k ++ ": " ++ v) (zip kstrs vstrs)
                                str      = Data.List.intercalate ", " kvstrs
                            return ("[" ++ str ++ "]")
valToString v          = return (show v)

evalExprToString :: Expr -> GscEnv -> Either String String
evalExprToString expr = runGscMWithEnv evalToString
  where
    evalToString = do v <- evalExpr expr
                      valToString v

evalExpr2 :: Expr -> GscEnv -> Either String Value
evalExpr2 expr = runGscMWithEnv (evalExpr expr)

evalStmt2 :: Stmt -> GscEnv -> Either String GscEnv
evalStmt2 stmt = runGscMWithEnv (evalStmt stmt >> getEnv)

evalMainFile :: Stmt -> Either String (IO ())
evalMainFile stmt = runGscMWithEnv evalMain emptyEnv
  where
    evalMain = do evalStmt stmt
                  evalFunctionCallExpr Nothing "main" []
                  getIO

evalObjRefLookup :: Value -> Value -> GscM Value
evalObjRefLookup idx (VRef ref) = do vstore <- getValue storeIdent
                                     case vstore of
                                       (VStore store) -> case Data.Map.lookup ref store of
                                                           (Just obj) -> getObjVal idx obj
                                                           Nothing    -> gscError "Invalid object reference"
  where
    getObjVal (VString "size") obj = return (VInt (toInteger (Data.Map.size obj)))
    getObjVal idx obj              = case Data.Map.lookup idx obj of
                                       (Just v) -> return v
                                       Nothing  -> gscError ("No value at index " ++ show idx)
evalObjRefLookup _   _          = gscError "Variable is not indexible"

lvCompsToExprs :: [LValueComp] -> [Expr]
lvCompsToExprs []                     = []
lvCompsToExprs (LValueComp i es:lvcs) = (StringLit i : es) ++ lvCompsToExprs lvcs

evalArrMapIndices :: Identifier -> [Expr] -> GscM Value
evalArrMapIndices i es = do v <- getValue i
                            evalAllIndices v es
  where
    evalAllIndices v []      = return v
    evalAllIndices v (e:es') = do idx <- evalExpr e
                                  v'  <- evalObjRefLookup idx v
                                  evalAllIndices v' es'

evalLValue :: LValue -> GscM Value
evalLValue (LValue _ (LValueComp i es:lvcs)) = let idxs = es ++ lvCompsToExprs lvcs
                                               in evalArrMapIndices i idxs

-- get the object reference of the second-last object in the lvalue, and return the
-- final expression in the lvalue which will be created for assignment into it
-- ex:
-- a.b["adam"]
-- would return b's object reference and the expression (StringLit "adam")
evalLValueObjRefAndIndex :: LValue -> GscM (Value, Expr)
evalLValueObjRefAndIndex (LValue _ [])                     = gscError "empty lvalue"
evalLValueObjRefAndIndex (LValue _ (LValueComp i es:lvcs)) = let idxs = es ++ lvCompsToExprs lvcs
                                                                 eidx  = last idxs
                                                             in do v <- evalArrMapIndices i (init idxs)
                                                                   return (v, eidx)

handleFunc :: String -> [String] -> GscEnv -> Stmt -> [Expr] -> Value -> GscM Value
handleFunc nm nmArgs fenv stmt args self = do vargs <- mapM evalExpr args
                                              if length vargs /= length nmArgs
                                                 then gscError ("Function " ++ nm ++ " takes " ++ show (length nmArgs) ++ " but " ++ show (length vargs) ++ " were supplied")
                                                 else do envOrig   <- getEnv
                                                         setArgs (("self", self) : zip nmArgs vargs)
                                                         result    <- evalStmt stmt
                                                         store     <- getValue storeIdent
                                                         nextObjId <- getValue nextObjIdIdent
                                                         setEnv envOrig
                                                         putValue storeIdent store
                                                         putValue nextObjIdIdent nextObjId
                                                         case result of
                                                           (ReturnResult v) -> return v
                                                           _                -> return VVoid
  where
    setArgs []                    = return ()
    setArgs ((nmArg, varg):pargs) = do putValue nmArg varg
                                       setArgs pargs

toStrPrint :: Value -> GscM String
toStrPrint (VString s) = return s
toStrPrint v           = valToString v

evalFunctionCallExpr :: Maybe LValue -> Identifier -> [Expr] -> GscM Value
evalFunctionCallExpr Nothing "print" args = do vargs <- mapM evalExpr args
                                               strs  <- mapM toStrPrint vargs
                                               let output = unwords strs
                                                  in do putIO (putStrLn output)
                                                        return VVoid
evalFunctionCallExpr Nothing nm args      = handleLibFunc
  where
    handleLibFunc = case Data.Map.lookup nm libFunctions of
                      (Just f) -> do vargs <- mapM evalExpr args
                                     f vargs
                      Nothing  -> do v <- getFunctionDef nm
                                     case v of
                                       (VFunctionDef nmArgs fenv stmt) -> handleFunc nm nmArgs fenv stmt args VVoid
evalFunctionCallExpr (Just lv) nm args    = do obj <- evalLValue lv
                                               case obj of
                                                 (VRef oid) -> do func <- getObjectValue oid nm
                                                                  case func of
                                                                    (VFunctionDef nmArgs fenv stmt) -> handleFunc nm nmArgs fenv stmt args obj
                                                 _          -> gscError "Cannot call funciton on non-object"

evalFuncDereference :: LValue -> [Expr] -> GscM Value
evalFuncDereference lvfunc args = do func <- evalLValue lvfunc
                                     case func of
                                       (VFunctionDef nmArgs fenv stmt) -> handleFunc "anonymous" nmArgs fenv stmt args VVoid
                                       _                               -> gscError "Cannot call non-function value"

evalExpr :: Expr -> GscM Value
evalExpr (BoolLit b)       = return (VBool b)
evalExpr (IntLit i)        = return (VInt i)
evalExpr (FloatLit n)      = return (VDouble n)
evalExpr (StringLit s)     = return (VString s)
evalExpr (ListLit es)      = evalListLit es
evalExpr (Binary op e1 e2) = do v1 <- evalExpr e1
                                v2 <- evalExpr e2
                                evalBinOp op v1 v2
evalExpr (Var lv)          = evalLValue lv

evalExpr (FuncReference Nothing nm)                                           = getFunctionDef nm
evalExpr (FunctionCallE mobj ctype (Left (LValue _ [LValueComp nm []])) args) = evalFunctionCallExpr mobj nm args
evalExpr (FunctionCallE Nothing ctype (Right (FuncDereference lv)) args)      = evalFuncDereference lv args

evalMExpr :: GscM Expr -> GscM Value
evalMExpr mexpr = do expr <- mexpr
                     evalExpr expr

getObject :: Integer -> GscM (Map Value Value)
getObject oid = do vstore <- getValue storeIdent
                   case vstore of
                     (VStore store) -> case Data.Map.lookup oid store of
                                         (Just obj) -> return obj
                                         Nothing    -> gscError "Invalid object reference"

getObjectValue :: Integer -> Identifier -> GscM Value
getObjectValue oid nm = do obj <- getObject oid
                           case Data.Map.lookup (VString nm) obj of
                             (Just v) -> return v
                             Nothing  -> gscError ("Object doesn't have attribute " ++ nm)

evalPutIntoObj :: Value -> Value -> Value -> GscM ()
evalPutIntoObj vidx v (VRef ref) = do vstore <- getValue storeIdent
                                      case vstore of
                                        (VStore store) -> case Data.Map.lookup ref store of
                                                            (Just obj) -> let obj'   = Data.Map.insert vidx v obj
                                                                              store' = Data.Map.insert ref obj' store
                                                                              in putValue storeIdent (VStore store')
                                                            Nothing    -> gscError "Invalid object reference"
evalPutIntoObj _    _ _          = gscError "Type is not indexable, must be an object/array"

evalPutIntoLValue :: Value -> LValue -> GscM StatementResult
evalPutIntoLValue v (LValue q [LValueComp i []])  = putValue i v >> return Success
evalPutIntoLValue v lv                            = do (objRef, eidx) <- evalLValueObjRefAndIndex lv
                                                       vidx   <- evalExpr eidx
                                                       evalPutIntoObj vidx v objRef
                                                       return Success

evalAssignEquals :: (Value -> Value -> GscM Value) -> LValue -> Expr -> GscM StatementResult
evalAssignEquals evalOp (LValue _ [LValueComp i []]) expr = do v1 <- getValue i
                                                               v2 <- evalExpr expr
                                                               vf <- evalOp v1 v2
                                                               putValue i vf
                                                               return Success

evalCondStmt :: [CondStmt] -> Maybe Stmt -> GscM StatementResult
evalCondStmt [] Nothing                    = return Success
evalCondStmt [] (Just stmt)                = do pushCurStructType If
                                                result <- evalStmt stmt
                                                popCurStructType
                                                return result
evalCondStmt (CondStmt cond stmt:cs) melse = do v <- evalExpr cond
                                                case implicitBoolConvert v of
                                                  (VBool True)  -> evalStmt stmt
                                                  (VBool False) -> evalCondStmt cs melse

evalWhileStmt :: Expr -> Stmt -> GscM StatementResult
evalWhileStmt cond stmt = do v <- evalExpr cond
                             case implicitBoolConvert v of
                               (VBool True)  -> do pushCurStructType Loop
                                                   r <- evalStmt stmt
                                                   popCurStructType
                                                   case r of
                                                     BreakResult      -> return Success
                                                     (ReturnResult v) -> return (ReturnResult v)
                                                     _           -> do evalWhileStmt cond stmt
                                                                       return Success
                               (VBool False) -> return Success

evalForStmt :: Maybe Stmt -> Expr -> Maybe Stmt -> Stmt -> GscM StatementResult
evalForStmt minit cond mupd stmt = case minit of
                                     (Just init) -> evalStmt init >> loop
                                     Nothing     -> loop
  where
    loop = do v <- evalExpr cond
              case v of
                (VBool True)  -> do pushCurStructType Loop
                                    r <- evalStmt stmt
                                    popCurStructType
                                    case r of
                                      BreakResult      -> return Success
                                      (ReturnResult v) -> return (ReturnResult v)
                                      _                -> case mupd of
                                                            (Just upd) -> evalStmt upd >> loop
                                                            Nothing    -> loop
                (VBool False) -> return Success

evalFunctionDef :: String -> [String] -> Stmt -> GscM StatementResult
evalFunctionDef nm args body = do env <- getEnv
                                  let vfunc = VFunctionDef args env body
                                    in do putFunctionDef nm vfunc
                                          return Success

evalSeq :: [Stmt] -> GscM StatementResult
evalSeq []      = return Success
evalSeq (s:sts) = do r <- evalStmt s
                     case r of
                       Success          -> evalSeq sts
                       BreakResult      -> return BreakResult
                       ContinueResult   -> gscError "Continue is not implemented"
                       (ReturnResult v) -> return (ReturnResult v)

evalReturnStmt :: Maybe Expr -> GscM StatementResult
evalReturnStmt (Just expr) = do v <- evalExpr expr
                                return (ReturnResult v)
evalReturnStmt Nothing     = return (ReturnResult VVoid)

evalStmt :: Stmt -> GscM StatementResult
evalStmt (Assign lv expr)               = do v <- evalExpr expr
                                             evalPutIntoLValue v lv
evalStmt (Seq stmts)                    = evalSeq stmts
evalStmt (CondStructStmt cs melse)      = evalCondStmt cs melse
evalStmt (WhileStmt expr stmt)          = evalWhileStmt expr stmt
evalStmt (ForStmt minit cond mupd stmt) = evalForStmt minit cond mupd stmt
evalStmt (FunctionDef nm args body)     = evalFunctionDef nm args body
evalStmt (FunctionCallS funcCallExpr)   = evalExpr funcCallExpr >> return Success
evalStmt Break                          = return BreakResult
evalStmt (ReturnStmt mexpr)             = evalReturnStmt mexpr

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
evalStmt (AssignExprStmt _)                  = gscError "Cannot use operators ++ or -- with non-lvalue"

storeIdent :: Identifier
storeIdent = "*objects"

nextObjIdIdent :: Identifier
nextObjIdIdent = "*nextObjId"

functionDefsIdent :: Identifier
functionDefsIdent = "*functionDefs"

emptyEnv :: GscEnv
emptyEnv = fromList [(storeIdent, VStore empty),
                     (nextObjIdIdent, VInt 0),
                     (functionDefsIdent, VFunctionDefs empty)]

startThreadState :: GscProcess
startThreadState = startThreadStateEnv emptyEnv

startThreadStateEnv :: Map Identifier Value -> GscProcess
startThreadStateEnv env = GscPRunning (GscState [] env [Root] (return ()))

runGscMWithEnv :: GscM a -> GscEnv -> Either String a
runGscMWithEnv (GscM f) env = case f (startThreadStateEnv env) of
                                (GscErr, GscPError err st) -> Left err
                                (GscVal v, _)              -> Right v

runGscM :: GscM a -> Either String a
runGscM mgsc = let env = Data.Map.fromList [("a", VInt 5), ("b", VInt 23)]
               in runGscMWithEnv mgsc env


-- LIBRARY FUNCTIONS


type LibFunction = [Value] -> GscM Value

libFunctions :: Map String LibFunction
libFunctions = fromList [("append", libAppend)]

libAppend :: LibFunction
libAppend [VRef oid, v] = do obj <- getObject oid
                             let intIndices    = Prelude.map negVIntVal (Prelude.filter matchInts (keys obj))
                                 sortedIndices = Data.List.sort intIndices
                                 nextIndex     = if Prelude.null sortedIndices
                                                    then 0
                                                    else (-(head sortedIndices)) + 1
                               in do evalPutIntoObj (VInt nextIndex) v (VRef oid)
                                     return VVoid
  where
    matchInts (VInt _) = True
    matchInts _        = False

    negVIntVal (VInt i) = -i

libAppend _               = gscError "append() must take an array and a value to append"
