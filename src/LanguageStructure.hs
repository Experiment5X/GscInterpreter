module LanguageStructure where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter <|> oneOf "_"
           , Token.identLetter     = alphaNum <|> oneOf "_"
           , Token.reservedNames   = [ "if"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "return"
                                     , "wait"
                                     --, "waittil"
                                     , "waittillframeend"
                                     --, "waittilmatch"
                                     --, "notify"
                                     --, "endon"
                                     --, "assert"
                                     --, "undefined"
                                     , "thread"
                                     , "childthread"
                                     , "for"
                                     , "foreach"
                                     , "in"
                                     , "switch"
                                     , "case"
                                     , "default"
                                     , "break"
                                     , "continue"
                                     , "true"
                                     , "false"
                                     , "include"
                                     , "using_animtree"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", "%", "="
                                     , "<", ">", "&&", "||", "!"
                                     , "++", "--", "==", "!=", "+="
                                     , "-=", "*=", "/=", "%=", ">>="
                                     , "<<=", "&=", "^=", "|="
                                     , ">>", "<<", "~", "===", "!=="
                                     , "::", ":", "#", "/#", "#/"
                                     , "[[", "]]"
                                     ]
           }

data BinOp = Add
            | Subtract
            | Multiply
            | Divide
            | Modulus
            | ShiftRight
            | ShiftLeft
            | AAnd -- arithmetic
            | AOr
            | AXor
            | BAnd -- boolean
            | BOr
            | Greater  -- relational
            | GreaterEq
            | Less
            | LessEq
            | Equal
            | NotEqual
              deriving (Show, Eq, Ord)

newtype Qualifier = Qualifier [String] deriving (Show, Eq, Ord)

newtype FuncDereference = FuncDereference LValue deriving (Show, Eq, Ord)

data FuncCallType = Default | Thread | ChildThread | Call deriving (Show, Eq, Ord)
data Expr = Var LValue
           | RValueExpr Expr [Expr] [LValueComp]
           | IntLit Integer
           | FloatLit Double
           | StringLit String
           | RefStringLit String
           | ListLit [Expr]
           | Vec3Lit Expr Expr Expr
           | BoolLit Bool
           | Neg Expr
           | ANot Expr
           | PreInc Expr
           | PreDec Expr
           | PostInc Expr
           | PostDec Expr
           | PreProc Expr
           | AnimRef Expr
           | Binary BinOp Expr Expr
           | FunctionCallE (Maybe LValue) FuncCallType (Either LValue FuncDereference) [Expr]
           | FuncNameE Qualifier
           | BNot Expr
             deriving (Show, Eq, Ord)

data LValue = LValue Qualifier [LValueComp] deriving (Show, Eq, Ord) -- elements are separated by a dot, for digging into objects
data LValueComp = LValueComp String [Expr] deriving (Show, Eq, Ord)  -- variable name, and any indices like: []

data CondStmt = CondStmt Expr Stmt deriving (Show, Eq, Ord)

data Stmt = Seq [Stmt]
          | Assign LValue Expr
          | AssignExprStmt Expr -- for things like: i++, --i
          | FunctionCallS Expr
          | CondStructStmt [CondStmt] (Maybe Stmt)
          | WhileStmt Expr Stmt
          | ForStmt (Maybe Stmt) Expr (Maybe Stmt) Stmt
          | ForeachStmt [String] Expr Stmt
          | ReturnStmt (Maybe Expr)
          | Continue
          | Break
          | FunctionDef String [String] Stmt
          | IncludeStmt [String]
          | UsingAnimTreeStmt String
          | DebugBlock Stmt
          | WaitStmt Expr
          | WaittillFrameEndStmt
          | PlusEquals LValue Expr
          | MinusEquals LValue Expr
          | TimesEquals LValue Expr
          | DivideEquals LValue Expr
          | ModEquals LValue Expr
          | AndEquals LValue Expr
          | OrEquals LValue Expr
          | XorEquals LValue Expr
            deriving (Show, Eq, Ord)

getOperators reservedOp =
            [  [Prefix  (reservedOp "-"   >> return Neg               )
             ,  Prefix  (reservedOp "~"   >> return ANot)
             ,  Prefix  (reservedOp "!"   >> return BNot              )
             ,  Prefix  (reservedOp "#"   >> return PreProc           )
             ,  Prefix  (reservedOp "%"   >> return AnimRef           )
             ,  Prefix  (reservedOp "++"  >> return PreInc            )
             ,  Prefix  (reservedOp "--"  >> return PreDec            )
             ,  Postfix (reservedOp "++"  >> return PostInc           )
             ,  Postfix (reservedOp "--"  >> return PostDec           )]
             , [Infix   (reservedOp "*"   >> return (Binary Multiply)) AssocLeft,
                Infix   (reservedOp "/"   >> return (Binary Divide  )) AssocLeft]
             , [Infix   (reservedOp "+"   >> return (Binary Add     )) AssocLeft,
                Infix   (reservedOp "-"   >> return (Binary Subtract)) AssocLeft]
             , [Infix   (reservedOp "%"   >> return (Binary Modulus )) AssocLeft]
             , [Infix   (reservedOp "^"   >> return (Binary AXor     )) AssocLeft,
                Infix   (reservedOp "&"   >> return (Binary AAnd)) AssocLeft,
                Infix   (reservedOp "|"   >> return (Binary AOr)) AssocLeft]
             , [Infix   (reservedOp "<<"  >> return (Binary ShiftLeft)) AssocLeft,
                Infix   (reservedOp ">>"  >> return (Binary ShiftRight)) AssocLeft]
             , [Infix   (reservedOp ">"   >> return (Binary Greater)) AssocLeft
             ,  Infix   (reservedOp "<"   >> return (Binary Less)) AssocLeft
             ,  Infix   (reservedOp ">="  >> return (Binary GreaterEq)) AssocLeft
             ,  Infix   (reservedOp "<="  >> return (Binary LessEq)) AssocLeft
             ,  Infix   (reservedOp "=="  >> return (Binary Equal)) AssocLeft
             ,  Infix   (reservedOp "!="  >> return (Binary NotEqual)) AssocLeft]
             , [Infix   (reservedOp "&&"  >> return (Binary BAnd     )) AssocLeft,
                Infix   (reservedOp "||"  >> return (Binary BOr     )) AssocLeft]
               ]

