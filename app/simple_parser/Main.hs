module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import SExpression

main :: IO ()
main = do
          args <- getArgs
          putStrLn (readExpr (args !! 0))

