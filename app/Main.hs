module Main where

import System.IO
import System.Environment
import Parser
import Interpreter
import LanguageStructure
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Error
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Map as Map
import System.Console.Haskeline

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

runProgram :: String -> IO ()
runProgram fname = do hFile    <- openFile fname ReadMode
                      contents <- hGetContents hFile
                      case parseFileStatements contents of
                        (Left e)     -> displayError contents e
                        (Right asts) -> case evalMainFile (Seq asts) of
                                          (Left e)   -> putStrLn e
                                          (Right io) -> io

fgsc :: IO ()
fgsc = do putStr "fgsc> "
          fname <- getLine
          parseFile fname
          fgsc
          
repl :: GscEnv -> IO ()
repl e = runInputT defaultSettings (runRepl e)
  where
    runRepl :: GscEnv -> InputT IO ()
    runRepl env = do ms <- getInputLine "~> "
                     case ms of
                       Nothing  -> runRepl env
                       (Just s) -> case parseStatement s of
                                     (Left e)     -> case parseExpression s of
                                                       (Left err)   -> outputStrLn (show err) >> runRepl env
                                                       (Right expr) -> case evalExpr2 expr env of
                                                                         (Left err) -> outputStrLn err      >> runRepl env
                                                                         (Right v)  -> outputStrLn (show v) >> runRepl env 
                                     (Right stmt) -> case evalStmt2 stmt env of
                                                       (Left err)   -> outputStrLn err >> runRepl env
                                                       (Right env') -> runRepl env'

main :: IO ()
main = do args <- getArgs
          if null args
             then repl emptyEnv
             else if length args == 1
                      then runProgram (head args)
                      else mapM_ parseFile args
          
