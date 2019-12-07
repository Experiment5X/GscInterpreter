module Main where

import System.IO
import System.Environment
import Parser
import Interpreter
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Error
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Map as Map

gsc :: IO ()
gsc = do putStr "gsc> "
         s <- getLine
         case parseStatement s of
           (Left e)    -> print e
           (Right ast) -> print ast
         gsc

displayError :: String -> ParseError -> IO ()
displayError fc err = do putStr labeledls
                         print err
  where
    lns       = lines fc
    l         = sourceLine (errorPos err)
    c         = min (length (lns !! (pred l))) (sourceColumn (errorPos err))
    slns      = take (min 5 l) (drop (max 0 (l - 5)) lns)
    labeled   = snd (foldr lineIter (l, []) slns)
    colSpace  = "    " ++ replicate (pred c + length (show l)) ' '
    colPtr    = colSpace ++ "⬆"
    labeledls = unlines (labeled ++ [colPtr])

    lineIter l' (ln, ls) = (pred ln, (show ln ++ "    " ++ l') : ls)


parseFile :: String -> IO ()
parseFile fname = do hFile    <- openFile fname ReadMode
                     contents <- hGetContents hFile
                     case parseFileStatements contents of
                       (Left e)     -> displayError contents e
                       (Right asts) -> print asts

fgsc :: IO ()
fgsc = do putStr "fgsc> "
          fname <- getLine
          parseFile fname
          fgsc
          
repl :: GscEnv -> IO ()
repl env = do putStr "~> "
              hFlush stdout
              s <- getLine
              case parseStatement s of
                (Left e)     -> case parseExpression s of
                                  (Left err)   -> print err >> repl env
                                  (Right expr) -> case evalExpr2 expr env of
                                                    (Left err) -> putStrLn err >> repl env
                                                    (Right v)  -> print v      >> repl env
                (Right stmt) -> case evalStmt2 stmt env of
                                  (Left err)   -> putStrLn err >> repl env
                                  (Right env') -> print env'   >> repl env'

main :: IO ()
main = do args <- getArgs
          if null args
             then repl Map.empty
             else mapM_ parseFile args
          
