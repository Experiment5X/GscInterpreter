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
float      = Token.float         lexer -- parses an integer
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

funcDereference :: Parser FuncDereference
funcDereference = FuncDereference <$> between (reservedOp "[[") (reservedOp "]]") lvalue

eitherLvalueOrFuncDeref :: Parser (Either LValue FuncDereference)
eitherLvalueOrFuncDeref = (Left <$> lvalue) <|> (Right <$> funcDereference)

functionCall :: Parser Expr
functionCall =   try (do (mlv, async) <- funcCallContext
                         helper mlv async)
             <|> helper Nothing False
  where
    helper mlv' async' = do elvfd <- eitherLvalueOrFuncDeref
                            exprs <- parens csvExpressions
                            return (FunctionCallE mlv' async' elvfd exprs)

term :: Parser Expr
term =   try functionCall
     <|> try (parens expression)
     <|> fmap Var lvalue
     <|> literal

termIndex :: Parser Expr
termIndex = try functionCall
          <|> try (parens expressionIndex)
          <|> fmap Var lvalue
          <|> fmap FloatLit (try floatStartDec)
          <|> fmap FloatLit (try float)
          <|> fmap IntLit integer
          <|> fmap StringLit stringLit
          <|> (reserved "true"  >> return (BoolLit True ))
          <|> (reserved "false" >> return (BoolLit False))
          <|> refStringLit

expression :: Parser Expr
expression = buildExpressionParser operators term

expressionIndex :: Parser Expr
expressionIndex = buildExpressionParser operators termIndex

rvalue :: Parser Expr
rvalue =   expression
       <|> term

-- rvalues that are allowed inside indices []
-- definitely not an ideal solution, but need to be able to support the function
-- dereference operator, object [[ my_func ]](), which could be parsed as object[ [my_func] ]()
rvalueIndex :: Parser Expr
rvalueIndex =   expressionIndex
            <|> termIndex

lvalueComponent :: Parser LValueComp
lvalueComponent = LValueComp <$> identifier <*> parseIndices
  where
    parseIndices =  try(do expr <- brackets rvalueIndex
                           rest <- parseIndices
                           return (expr : rest))
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
skipAllWhitespace =   (whiteSpace >> possiblyContinue space  )
                  <|> (whiteSpace >> possiblyContinue tab    )
                  <|> (whiteSpace >> possiblyContinue newline)
                  <|> (whiteSpace >> possiblyContinue (char '\r'))
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


sequenceOfFileStmt :: Parser [Stmt]
sequenceOfFileStmt =   do skipAllWhitespace
                          stmt  <- statement'
                          skipAllWhitespace
                          stmts <- sequenceOfFileStmt
                          return (stmt : stmts)
                   <|> (eof >> return [])

-- here I should parse an lvalue, and then try to parse the rest of it
simpleStatement :: Parser Stmt
simpleStatement =   try assignStmt
                <|> try assignExprStmt
                <|> try funcCallStmt
                <|> updateExprStmt

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
           <|> (reserved "break" >> semi >> return Break)
           <|> (reserved "continue" >> semi >> return Continue)
           <|> debugBlockStmt
           <|> waitStmt
           <|> funcDefStmt

assignStmt :: Parser Stmt
assignStmt = Assign <$> lvalue <*> (reservedOp "=" >> rvalue)

assignExprStmt :: Parser Stmt
assignExprStmt = do expr <- expression
                    case expr of
                      (PostInc e) -> return (AssignExprStmt (PostInc e))
                      (PostDec e) -> return (AssignExprStmt (PostDec e))
                      (PreInc e)  -> return (AssignExprStmt (PreInc e))
                      (PreDec e)  -> return (AssignExprStmt (PreDec e))
                      _           -> fail "Expected statement, found expression."

updateExprStmt :: Parser Stmt
updateExprStmt =   updateExpr "+=" PlusEquals
               <|> updateExpr "-=" MinusEquals
               <|> updateExpr "*=" TimesEquals
               <|> updateExpr "/=" DivideEquals
               <|> updateExpr "%=" ModEquals
               <|> updateExpr "&=" AndEquals
               <|> updateExpr "|=" OrEquals
               <|> updateExpr "^=" XorEquals
  where
    updateExpr op tc = tc <$> lvalue <*> (reservedOp op >> rvalue)


debugBlockStmt :: Parser Stmt
debugBlockStmt = DebugBlock <$> between (reservedOp "/#") (reservedOp "#/") statement

returnStmt :: Parser Stmt
returnStmt = do reserved "return"
                mexpr <- optionMaybe expression
                semi
                return (ReturnStmt mexpr)

waitStmt :: Parser Stmt
waitStmt =   do reserved "wait"
                expr <- expression
                semi
                return (WaitStmt expr)
         <|> (reserved "waittillframeend" >> semi >> return WaittillFrameEndStmt)

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

floatStartDec :: Parser Double
floatStartDec = do dot
                   fract <- integer
                   let digits = length (show fract)
                       value  = (fromIntegral fract :: Double) * 10.0 ** (-fromIntegral digits :: Double)
                   return value

literal :: Parser Expr
literal =   fmap FloatLit (try floatStartDec)
        <|> fmap FloatLit (try float)
        <|> fmap IntLit integer
        <|> fmap StringLit stringLit
        <|> (reserved "true"  >> return (BoolLit True ))
        <|> (reserved "false" >> return (BoolLit False))
        <|> refStringLit
        <|> listLiteral
        <|> vec3Literal

refStringLit :: Parser Expr
refStringLit = do reservedOp "&"
                  RefStringLit <$> stringLit
                  
vec3Literal :: Parser Expr
vec3Literal = do parens (do e1 <- expression
                            comma
                            e2 <- expression
                            comma
                            e3 <- expression
                            return (Vec3Lit e1 e2 e3))

listLiteral :: Parser Expr
listLiteral = ListLit <$> brackets (sepBy expression comma)

switchStmt :: Parser Stmt
switchStmt = do reserved "switch"
                expr  <- parens expression
                braces (do conds <- cases expr
                           mstmt <- optionMaybe default'
                           return (CondStructStmt conds mstmt))
  where
    case' :: Parser Expr
    case' = do reserved "case"
               lit <- literal
               reservedOp ":"
               return lit

    cases :: Expr -> Parser [CondStmt]
    cases expr' = do lits   <- many1 case'
                     stmt   <- statement
                     cases' <- option [] (cases expr')
                     let cond = foldr makeMultCases (BoolLit False) lits
                     return (CondStmt cond stmt : cases')
      where
        makeMultCases lit = Binary BOr (Binary Equal lit expr')
                     
    default' :: Parser Stmt
    default' = do reserved "default"
                  reservedOp ":" 
                  statement

forStmt :: Parser Stmt
forStmt = do reserved "for"
             (masgn, cond, mnext) <- parens (do masgn <- optionMaybe assignStmt
                                                semi
                                                cond  <- option (IntLit 1) expression
                                                semi
                                                mnext <- optionMaybe simpleStatement
                                                return (masgn, cond, mnext))
             ForStmt masgn cond mnext <$> structureBody

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
whileStmt = WhileStmt <$> (reserved "while" >> parens expression) <*> structureBody

funcCallStmt :: Parser Stmt
funcCallStmt = FunctionCallS <$> functionCall

funcDefStmt :: Parser Stmt
funcDefStmt = do funcName <- identifier
                 args     <- parens (option [] (sepBy identifier comma))
                 stmt     <- braces (option (Seq []) statement)
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

parseFileStatements :: String -> Either ParseError [Stmt]
parseFileStatements = parse sequenceOfFileStmt ""

parseFile :: String -> IO ()
parseFile fname = do hFile    <- openFile fname ReadMode
                     contents <- hGetContents hFile
                     case parseFileStatements contents of
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
