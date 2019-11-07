module Parser where

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
import LanguageStructure


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
braces     = Token.braces        lexer -- parses curly braces {}

operators = getOperators reservedOp

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
           <|> ifStmt

assignStmt :: Parser Stmt
assignStmt = do var  <- lvalue
                reservedOp "="
                expr <- rvalue
                semi
                return $ Assign var expr

ifStmt :: Parser Stmt
ifStmt = do conds <- parseConds False
            mstmt <- optionMaybe (reserved "else" >> braces statement)
            return (IfStmt conds mstmt)
  where
    parseConds isElif = if isElif
                           then option [] (try parseStruct)
                           else parseStruct
      where
        parseStruct :: Parser [CondStmt]
        parseStruct = do when isElif (reserved "else")
                         reserved "if"
                         expr  <- parens expression
                         stmt  <- braces statement
                         conds <- parseConds True
                         return (CondStmt expr stmt : conds)

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
