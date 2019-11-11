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
                                     --, "wait"
                                     --, "waittil"
                                     --, "waittilframeend"
                                     --, "waittilmatch"
                                     --, "notify"
                                     --, "endon"
                                     --, "assert"
                                     --, "undefined"
                                     , "thread"
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
                                     , "::", ":", "#"
                                     ]
           }

data BinOp = Add
            | Subtract
            | Multiply
            | Divide
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
              deriving (Show, Eq)

newtype Qualifier = Qualifier [String] deriving (Show, Eq)

data Expr = Var LValue
           | IntLit Integer
           | FloatLit Double
           | StringLit String
           | RefStringLit String
           | BoolConst Bool
           | Neg Expr
           | ANot Expr
           | PreInc Expr
           | PreDec Expr
           | PostInc Expr
           | PostDec Expr
           | PreProc Expr
           | AnimRef Expr
           | Binary BinOp Expr Expr
           | FunctionCallE (Maybe LValue) Bool LValue [Expr]
           | FuncNameE Qualifier
           | BNot Expr
             deriving (Show, Eq)

data LValue = LValue Qualifier [LValueComp] deriving (Show, Eq) -- elements are separated by a dot, for digging into objects
data LValueComp = LValueComp String [Expr] deriving (Show, Eq)  -- variable name, and any indices like: []

data CondStmt = CondStmt Expr Stmt deriving (Show, Eq)

data Stmt = Seq [Stmt]
          | Assign LValue Expr
          | FunctionCallS Expr
          | CondStructStmt [CondStmt] (Maybe Stmt)
          | WhileStmt Expr Stmt
          | ForStmt Stmt Expr Stmt Stmt
          | ForeachStmt [String] Expr Stmt
          | ReturnStmt (Maybe Expr)
          | FunctionDef String [String] Stmt
          | IncludeStmt [String]
          | UsingAnimTreeStmt String
            deriving (Show, Eq)

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

