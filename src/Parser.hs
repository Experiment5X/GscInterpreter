module Parser where

{-
  This is based off this tutorial:
  https://wiki.haskell.org/Parsing_a_simple_imperative_language
-}

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
semi       = Token.semi          lexer -- parses a semicolon
whiteSpace = Token.whiteSpace    lexer -- parses whitespace
comma      = Token.comma         lexer -- parses a comma
brackets   = Token.brackets      lexer -- parses square brackets, []
colon      = Token.colon         lexer -- parses a colon, :
dot        = Token.dot           lexer -- parses the dot, .
braces     = Token.braces        lexer -- parses curly braces {}
symbol     = Token.symbol        lexer -- parses curly braces {}


-- MW2 has strings like this:
--   println( "maps\\\_createpath::path_create(\"");
-- but it looks like they're parser is just more lax about multiple
-- back slashes in a row instead of there being a \_ escape sequence
-- because I only see it used where it looks like they just want
-- a _ anyways.

-- copied from: https://stackoverflow.com/a/24106749
-- need to add escape characters and the built-in parsec one doesn't
-- provide a way to add any escape characters
escape :: Parser String
escape = do
    d <- char '\\'
    c <- anyChar
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

stringLit :: Parser String
stringLit = do
    whiteSpace
    char '"'
    strings <- many character
    char '"'
    whiteSpace
    return $ concat strings

-- if the operator is immediately followed by the beginning of a float
-- constant, then the parser had trouble before, as in this expression:
-- a = 3*.5;
checkDot :: String -> Parser ()
checkDot s = do symbol s
                try (lookAhead dot)
                return ()

-- if the operator is immediately followed by a negated variable then
-- the parser had trouble before, as in this expression:
-- a = b*-1;
checkDash :: String -> Parser ()
checkDash s = do symbol s
                 if s == "-"
                    then fail ""
                    else do try (lookAhead singleDash)
                            return ()
  where
    singleDash = do symbol "-"
                    c <- anyChar
                    when (c == '-') (fail "Cannot have double dash here")

regularOrDot :: String -> Parser ()
regularOrDot s = choice [reservedOp s, try (checkDot s), try (checkDash s)]

operators = getOperators regularOrDot

csvExpressions :: Parser [Expr]
csvExpressions = sepBy expression comma

funcCallContext :: Parser (Maybe LValue, FuncCallType)
funcCallContext =   try (do lv <- lvalue
                            ((reserved "thread" >> return (Just lv, Thread))
                              <|> (reserved "call" >> return (Just lv, Call)))
                              <|> (reserved "childthread" >> return (Just lv, Call))
                              <|> return (Just lv, Default))
                <|> do reserved "thread"
                       return (Nothing, Thread)
                <|> do reserved "childthread"
                       return (Nothing, ChildThread)
                <|> do reserved "call"
                       return (Nothing, Thread)

qualifier :: Parser Qualifier
qualifier =   try (do path  <- sepBy1 identifier (char '\\')
                      reservedOp "::"
                      return (Qualifier path))
          <|> return (Qualifier [])
          <?> "qualifier"

funcDereference :: Parser FuncDereference
funcDereference =   (FuncDereference <$> between (reservedOp "[[") (reservedOp "]]") lvalue)
                <?> "function dereference"

eitherLvalueOrFuncDeref :: Parser (Either LValue FuncDereference)
eitherLvalueOrFuncDeref = (Left <$> lvalue) <|> (Right <$> funcDereference)

functionCall :: Parser Expr
functionCall =   try (do (mlv, t) <- funcCallContext
                         helper mlv t)
             <|> helper Nothing Default
             <?> "function call"
  where
    helper mlv' async' = do elvfd <- eitherLvalueOrFuncDeref
                            exprs <- parens csvExpressions
                            return (FunctionCallE mlv' async' elvfd exprs)

rvalue :: Parser Expr
rvalue =   functionCall
       <|> literal
       <?> "r-value"

term :: Parser Expr
term =   try rvalueExpr
     <|> try (parens expression)
     <|> try (fmap Var lvalue)
     <|> try funcReference
     <?> "term"

rvalueExpr :: Parser Expr
rvalueExpr = do rv    <- rvalue
                is    <- parseIndices
                comps <- option [] trailingObjs
                if null is && null comps
                   then return rv
                   else return (RValueExpr rv is comps)
  where
    trailingObjs = do dot
                      sepBy1 lvalueComponent dot


value :: Parser Expr
value =   expression
      <|> term

funcReference :: Parser Expr
funcReference = do reservedOp "::"
                   FuncReference Nothing <$> identifier

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


-- values that are allowed inside indices []
-- definitely not an ideal solution, but need to be able to support the function
-- dereference operator, object [[ my_func ]](), which could be parsed as object[ [my_func] ]()
valueIndex :: Parser Expr
valueIndex =   expressionIndex
            <|> termIndex

parseIndices :: Parser [Expr]
parseIndices =  try(do expr <- brackets valueIndex
                       rest <- parseIndices
                       return (expr : rest))
             <|> return []

lvalueComponent :: Parser LValueComp
lvalueComponent = LValueComp <$> identifier <*> parseIndices

lvalue :: Parser LValue
lvalue =   lv
       <|> try (parens lv)
       <?> "l-value"
  where
    lv = do q     <- qualifier
            comps <- sepBy1 lvalueComponent dot
            return (LValue q comps)

statement :: Parser Stmt
statement = do whiteSpace
               helper
  where
    helper =   try (parens statement)
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
           <|> braces statement
           <|> (semi >> return (Seq []))
           <?> "statement"

assignStmt :: Parser Stmt
assignStmt = Assign <$> lvalue <*> (reservedOp "=" >> value)

assignExprStmt :: Parser Stmt
assignExprStmt = do expr <- expression
                    case expr of
                      (PostInc e) -> return (AssignExprStmt (PostInc e))
                      (PostDec e) -> return (AssignExprStmt (PostDec e))
                      (PreInc e)  -> return (AssignExprStmt (PreInc e))
                      (PreDec e)  -> return (AssignExprStmt (PreDec e))
                      _           -> fail "Expected statement, found expression."

updateExprStmt :: Parser Stmt
updateExprStmt = do lv <- lvalue
                    updateExprAll lv
  where
    updateExprAll :: LValue -> Parser Stmt
    updateExprAll lv =   updateExpr lv "+=" PlusEquals
                     <|> updateExpr lv "-=" MinusEquals
                     <|> updateExpr lv "*=" TimesEquals
                     <|> updateExpr lv "/=" DivideEquals
                     <|> updateExpr lv "%=" ModEquals
                     <|> updateExpr lv "&=" AndEquals
                     <|> updateExpr lv "|=" OrEquals
                     <|> updateExpr lv "^=" XorEquals
    updateExpr lv op tc = do reservedOp op
                             expr <- value
                             return (tc lv expr)


debugBlockStmt :: Parser Stmt
debugBlockStmt = (DebugBlock <$> between (reservedOp "/#") (reservedOp "#/") (statement <|> emptyStmt)) <?> "Debug block"
  where
    emptyStmt = do whiteSpace
                   return (Seq [])

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
structureBody =   try (reservedOp "{" >> reservedOp "}" >> return (Seq []))
              <|> braces statement
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
floatStartDec = do char '.'
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
        <?> "literal"

refStringLit :: Parser Expr
refStringLit = do reservedOp "&"
                  RefStringLit <$> stringLit
                  
vec3Literal :: Parser Expr
vec3Literal = do parens (do e1 <- value
                            comma
                            e2 <- value
                            comma
                            e3 <- value
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
               colon
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
                  colon
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
                     expr <- value
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

parseExpression :: String -> Either ParseError Expr
parseExpression = parse expression ""

parseFileStatements :: String -> Either ParseError [Stmt]
parseFileStatements = parse sequenceOfFileStmt ""
