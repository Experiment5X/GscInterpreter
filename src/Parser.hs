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
float      = Token.float       lexer -- parses an integer
stringLit  = Token.stringLiteral lexer
semi       = Token.semi          lexer -- parses a semicolon
whiteSpace = Token.whiteSpace    lexer -- parses whitespace
comma      = Token.comma         lexer -- parses a comma
brackets   = Token.brackets      lexer -- parses square brackets, []
dot        = Token.dot           lexer -- parses the dot, .
braces     = Token.braces        lexer -- parses curly braces {}

operators = getOperators reservedOp

csvExpressions :: Parser [Expr]
csvExpressions = sepBy expression comma

funcCallContext :: Parser (Maybe LValue, Bool)
funcCallContext =   try (do lv <- lvalue
                            reserved "thread"
                            return (Just lv, True))
                <|> do lv <- lvalue
                       return (Just lv, False)
                <|> do reserved "thread"
                       return (Nothing, True)

qualifier :: Parser Qualifier
qualifier =   try (do path  <- sepBy1 identifier (char '\\')
                      reservedOp "::"
                      return (Qualifier path))

          <|> do reservedOp "::"
                 return (Qualifier [])
          <|> return (Qualifier [])

functionCall :: Parser Expr
functionCall =   try (do (mlv, async) <- funcCallContext
                         helper mlv async)
             <|> helper Nothing False
  where
    helper mlv' async' = do lv    <- lvalue
                            exprs <- parens csvExpressions
                            return (FunctionCallE mlv' async' lv exprs)

term =   try functionCall
     <|> parens expression
     <|> fmap Var lvalue
     <|> literal
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

lvalue :: Parser LValue
lvalue = do q     <- qualifier
            comps <- sepBy1 lvalueComponent dot
            return (LValue q comps)

statement :: Parser Stmt
statement = do whiteSpace
               helper
  where
    helper =   parens statement
           <|> do stmts <- sequenceOfStmt
                  if length stmts == 1
                     then return (head stmts)
                     else return (Seq stmts)

skipAllWhitespace :: Parser ()
skipAllWhitespace = optional (   (whiteSpace >> possiblyContinue space  )
                             <|> (whiteSpace >> possiblyContinue tab    )
                             <|> (whiteSpace >> possiblyContinue newline)
                             <|> (whiteSpace >> possiblyContinue (char '\r')))
  where
    possiblyContinue ch = do mc <- optionMaybe ch
                             case mc of
                               Nothing  -> return ()
                               (Just _) -> skipAllWhitespace

sequenceOfStmt :: Parser [Stmt]
sequenceOfStmt = do skipAllWhitespace
                    stmt  <- statement'
                    skipAllWhitespace
                    stmts <- helper
                    return (stmt : stmts)
  where
    helper =   try sequenceOfStmt
           <|> return []

simpleStatement :: Parser Stmt
simpleStatement =   try assignStmt
                <|> funcCallStmt

statement' :: Parser Stmt
statement' =   preprocessStmt
           <|> try (do stmt <- simpleStatement
                       semi
                       return stmt)
           <|> ifStmt
           <|> whileStmt
           <|> switchStmt
           <|> forStmt
           <|> foreachStmt
           <|> returnStmt
           <|> funcDefStmt

assignStmt :: Parser Stmt
assignStmt = Assign <$> lvalue <*> (reservedOp "=" >> rvalue)

returnStmt :: Parser Stmt
returnStmt = do reserved "return"
                mexpr <- optionMaybe expression
                semi
                return (ReturnStmt mexpr)

structureBody :: Parser Stmt
structureBody =   braces statement
              <|> statement'

ifStmt :: Parser Stmt
ifStmt = do conds <- parseConds False
            mstmt <- optionMaybe (reserved "else" >> structureBody)
            return (CondStructStmt conds mstmt)
  where
    parseConds isElif = if isElif
                           then option [] (try parseStruct)
                           else parseStruct
      where
        parseStruct :: Parser [CondStmt]
        parseStruct = do when isElif (reserved "else")
                         reserved "if"
                         expr  <- parens expression
                         stmt  <- structureBody
                         conds <- parseConds True
                         return (CondStmt expr stmt : conds)

literal :: Parser Expr
literal =   fmap FloatLit (try float)
        <|> fmap IntLit integer
        <|> fmap StringLit stringLit
        <|> refStringLit

refStringLit :: Parser Expr
refStringLit = do reservedOp "&"
                  RefStringLit <$> stringLit

switchStmt :: Parser Stmt
switchStmt = do reserved "switch"
                expr  <- parens expression
                braces (do conds <- cases expr
                           mstmt <- optionMaybe default'
                           return (CondStructStmt conds mstmt))
  where
    cases :: Expr -> Parser [CondStmt]
    cases expr' = do reserved "case"
                     lit    <- literal
                     reservedOp ":" 
                     stmt   <- statement
                     cases' <- option [] (cases expr')
                     let cond = Binary Equal expr' lit
                     return (CondStmt cond stmt : cases')
                     
    default' :: Parser Stmt
    default' = do reserved "default"
                  reservedOp ":" 
                  statement

forStmt :: Parser Stmt
forStmt = do reserved "for"
             (asgn, cond, next) <- parens (do asgn <- assignStmt
                                              semi
                                              cond <- expression
                                              semi
                                              next <- simpleStatement
                                              return (asgn, cond, next))
             ForStmt asgn cond next <$> structureBody

foreachStmt :: Parser Stmt
foreachStmt = do reserved "foreach"
                 (vars, expr) <- parens iterateExpr
                 ForeachStmt vars expr <$> structureBody
  where
    iterateExpr :: Parser ([String], Expr)
    iterateExpr = do vars <- sepBy1 identifier comma
                     reserved "in"
                     expr <- rvalue
                     return (vars, expr)

whileStmt :: Parser Stmt
whileStmt = do reserved "while"
               expr <- parens expression
               stmt <- braces statement
               return (WhileStmt expr stmt)

funcCallStmt :: Parser Stmt
funcCallStmt = FunctionCallS <$> functionCall

funcDefStmt :: Parser Stmt
funcDefStmt = do funcName <- identifier
                 args     <- parens (option [] (sepBy identifier comma))
                 stmt     <- braces statement
                 return (FunctionDef funcName args stmt)

preprocessStmt :: Parser Stmt
preprocessStmt = do reservedOp "#"
                    includeStmt <|> usingAnimTreeStmt

includeStmt :: Parser Stmt
includeStmt = do reserved "include"
                 nameComp <- sepBy1 identifier (char '\\')
                 semi
                 return (IncludeStmt nameComp)

usingAnimTreeStmt :: Parser Stmt
usingAnimTreeStmt = do reserved "using_animtree"
                       treeName <- parens stringLit
                       semi
                       return (UsingAnimTreeStmt treeName)

parseStatement :: String -> Either ParseError Stmt
parseStatement = parse statement ""

parseStatements :: String -> Either ParseError [Stmt]
parseStatements = parse sequenceOfStmt ""

parseFile :: String -> IO ()
parseFile fname = do hFile    <- openFile fname ReadMode
                     contents <- hGetContents hFile
                     case parseStatements contents of
                       (Left e)     -> print e
                       (Right asts) -> print asts


gsc :: IO ()
gsc = do putStr "gsc> "
         s <- getLine
         case parseStatement s of
           (Left e)    -> print e
           (Right ast) -> print ast
         gsc

fgsc :: IO ()
fgsc = do putStr "fgsc> "
          fname <- getLine
          parseFile fname
          fgsc
