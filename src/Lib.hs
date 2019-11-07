module Lib where

{-
  This is based off this tutorial:
  https://wiki.haskell.org/Parsing_a_simple_imperative_language
-}

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.List

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
                                     , "thread"
                                     , "undefined"
                                     , "for"
                                     , "foreach"
                                     , "switch"
                                     , "case"
                                     , "default"
                                     , "break"
                                     , "continue"
                                     , "true"
                                     , "false"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", "%", "="
                                     , "<", ">", "&&", "||", "!"
                                     , "++", "--", "==", "!=", "+="
                                     , "-=", "*=", "/=", "%=", ">>="
                                     , "<<=", "&=", "^=", "|="
                                     , ">>", "<<", "~", "===", "!=="
                                     , "::" 
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

data FuncName = FuncName [String] String deriving (Show, Eq)

data Expr = Var LValue
           | IntLit Integer
           | StringLit String
           | BoolConst Bool
           | Neg Expr
           | ANot Expr
           | PreInc Expr
           | PreDec Expr
           | PostInc Expr
           | PostDec Expr
           | Binary BinOp Expr Expr
           | FunctionCallE (Maybe LValue) Bool FuncName [Expr]
           | FuncNameE FuncName
           | BNot Expr
             deriving (Show, Eq)

type LValue = [LValueComp] -- elements are separated by a dot, for digging into objects
data LValueComp = LValueComp String [Expr] deriving (Show, Eq) -- variable name, and any indices like: []

data Stmt = Seq [Stmt]
          | Assign LValue Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | FunctionCallS Expr
            deriving (Show, Eq)

operators = [ [Prefix (reservedOp "-"   >> return Neg               )
             ,  Prefix (reservedOp "~"   >> return ANot)
             ,  Prefix (reservedOp "!" >> return BNot              )]
             , [Infix  (reservedOp "*"   >> return (Binary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (Binary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (Binary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (Binary Subtract)) AssocLeft]
             , [Infix  (reservedOp "^"   >> return (Binary AXor     )) AssocLeft,
                Infix  (reservedOp "&"   >> return (Binary AAnd)) AssocLeft,
                Infix  (reservedOp "|"   >> return (Binary AOr)) AssocLeft]
             , [Infix  (reservedOp "<<"  >> return (Binary ShiftLeft)) AssocLeft,
                Infix  (reservedOp ">>"  >> return (Binary ShiftRight)) AssocLeft]
             , [Infix  (reservedOp ">"   >> return (Binary Greater)) AssocLeft
             ,  Infix  (reservedOp "<"   >> return (Binary Less)) AssocLeft
             ,  Infix  (reservedOp ">="  >> return (Binary GreaterEq)) AssocLeft
             ,  Infix  (reservedOp "<="  >> return (Binary LessEq)) AssocLeft
             ,  Infix  (reservedOp "=="  >> return (Binary Equal)) AssocLeft
             ,  Infix  (reservedOp "!="  >> return (Binary NotEqual)) AssocLeft]
             , [Infix  (reservedOp "&&"  >> return (Binary BAnd     )) AssocLeft,
                Infix  (reservedOp "||"  >> return (Binary BOr     )) AssocLeft]
               ]
               
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier    lexer -- parses an identifier
reserved   = Token.reserved      lexer -- parses a reserved name
reservedOp = Token.reservedOp    lexer -- parses an operator
parens     = Token.parens        lexer -- parses surrounding parenthesis:
                                       --   parens p
                                       -- takes care of the parenthesis and
                                       -- uses p to parse what's inside them
integer    = Token.integer       lexer -- parses an integer
stringLit  = Token.stringLiteral lexer
semi       = Token.semi          lexer -- parses a semicolon
whiteSpace = Token.whiteSpace    lexer -- parses whitespace
comma      = Token.comma         lexer -- parses a comma
brackets   = Token.brackets      lexer -- parses square brackets, []
dot        = Token.dot           lexer -- parses the dot, .


csvAExpressions :: Parser [Expr]
csvAExpressions = sepBy expression comma

funcCallContext :: Parser (Maybe LValue, Bool)
funcCallContext =   try (do lv <- lvalue
                            reserved "thread"
                            return (Just lv, True))
                <|> do lv <- lvalue
                       return (Just lv, False)
                <|> do reserved "thread"
                       return (Nothing, True)
                       
fqFuncName :: Parser FuncName
fqFuncName = do path  <- sepBy identifier (char '\\')
                ident <- if null path
                            then identifier
                            else reservedOp "::" >> identifier
                return (FuncName path ident)

functionName :: Parser FuncName
functionName =   try fqFuncName
             <|> (reservedOp "::" >> plainName)
             <|> plainName
  where
    plainName = FuncName [] <$> identifier

functionCall :: Parser Expr
functionCall =   try (do (mlv, async) <- funcCallContext
                         helper mlv async)
             <|> helper Nothing False
  where
    helper mlv' async' = do f     <- functionName
                            exprs <- parens csvAExpressions
                            return (FunctionCallE mlv' async' f exprs)

term =   try functionCall
     <|> parens expression
     <|> fmap Var lvalue
     <|> try (FuncNameE <$> functionName)
     <|> fmap IntLit integer
     <|> fmap StringLit stringLit
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))

expression :: Parser Expr
expression = buildExpressionParser operators term

rvalue :: Parser Expr
rvalue =   expression
       <|> term

lvalueComponent :: Parser LValueComp
lvalueComponent = LValueComp <$> identifier <*> parseIndices
  where
    parseIndices =   do expr <- brackets rvalue
                        rest <- parseIndices
                        return (expr : rest)
                 <|> return []
                 
reservedLvalue :: String -> LValue
reservedLvalue s = [LValueComp s []]


lvalue :: Parser LValue
lvalue = sepBy1 lvalueComponent dot

statement :: Parser Stmt
statement = do whiteSpace
               helper
  where
    helper =   parens statement
           <|> do stmts <- sequenceOfStmt
                  if length stmts == 1
                     then return (head stmts)
                     else return (Seq stmts)

sequenceOfStmt :: Parser [Stmt]
sequenceOfStmt = do stmt  <- statement'
                    stmts <- helper
                    return (stmt : stmts)
  where
    helper =   try sequenceOfStmt
           <|> return []


statement' :: Parser Stmt
statement' =   try assignStmt
           <|> funcCallStmt

assignStmt :: Parser Stmt
assignStmt = do var  <- lvalue
                reservedOp "="
                expr <- rvalue
                semi
                return $ Assign var expr

funcCallStmt :: Parser Stmt
funcCallStmt = do e <- functionCall
                  semi
                  return (FunctionCallS e)

parseStatement :: String -> Either ParseError Stmt
parseStatement = parse statement ""

gsc :: IO ()
gsc = do putStr "gsc> "
         s <- getLine
         case parseStatement s of
           (Left e)    -> print e
           (Right ast) -> print ast
         gsc
