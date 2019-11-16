module Main where

import System.IO
import Parser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Error
import qualified Text.ParserCombinators.Parsec.Token as Token

gsc :: IO ()
gsc = do putStr "gsc> "
         s <- getLine
         case parseStatement s of
           (Left e)    -> print e
           (Right ast) -> print ast
         gsc
         
displayError :: String -> ParseError -> IO ()
displayError fc err = do print err
                         print line
  where
    l = sourceLine (errorPos err)
    c = sourceColumn (errorPos err)
    line = lines fc !! (pred l)
    
         
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

main :: IO ()
main = gsc
