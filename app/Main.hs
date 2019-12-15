{-# LANGUAGE DeriveDataTypeable #-}

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
import System.Console.CmdArgs
import Control.Monad

gsc :: IO ()
gsc = do putStr "gsc> "
         s <- getLine
         case parseStatement s of
           (Left e)    -> print e
           (Right ast) -> print ast
         gsc

displayError :: String -> String -> ParseError -> IO ()
displayError fname fc err = do putStrLn fname
                               putStr labeledls
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


parseFile :: Bool -> String -> IO ()
parseFile past fname = do hFile    <- openFile fname ReadMode
                          contents <- hGetContents hFile
                          case parseFileStatements contents of
                            (Left e)     -> displayError fname contents e
                            (Right asts) -> when past (print asts)

runProgram :: String -> IO ()
runProgram fname = do hFile    <- openFile fname ReadMode
                      contents <- hGetContents hFile
                      case parseFileStatements contents of
                        (Left e)     -> displayError fname contents e
                        (Right asts) -> case evalMainFile (Seq asts) of
                                          (Left e)   -> putStrLn e
                                          (Right io) -> io

fgsc :: IO ()
fgsc = do putStr "fgsc> "
          fname <- getLine
          parseFile True fname
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
                                                       (Right expr) -> case evalExprToString expr env of
                                                                         (Left err)    -> outputStrLn err  >> runRepl env
                                                                         (Right vstr)  -> outputStrLn vstr >> runRepl env 
                                     (Right stmt) -> case evalStmt2 stmt env of
                                                       (Left err)   -> outputStrLn err >> runRepl env
                                                       (Right env') -> runRepl env'

data Options = Options {
  files    :: [String],
  check    :: Bool,
  printAst :: Bool
} deriving (Data, Typeable)

options :: Options
options = Options { files = []
                              &= args
                              &= typFile
                  , check = False
                         &= help "Only check the files for syntax"
                  , printAst = False
                         &= help "Print the parsed AST data structure when checking for syntax errors"
                  }
       &= summary "Call of Duty Gsc Interpreter v1.0, Adam Spindler"
       &= program "codgsc"

main :: IO ()
main = do
  opts <- cmdArgs options

  let fs = files    opts
      c  = check    opts
      p  = printAst opts

  handleFiles c p fs

handleFiles :: Bool -> Bool -> [String] -> IO ()
handleFiles _     _ []  = repl emptyEnv
handleFiles True  p fs  = mapM_ (parseFile p) fs
handleFiles False _ [f] = runProgram f
handleFiles False _ _   = putStrLn "Unable to interpret more than one file, it's currently unsupported."

