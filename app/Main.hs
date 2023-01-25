module Main where

import Control.Monad
import System.Environment
-- import Data.Char
-- import SExpression
-- import Text.ParserCombinators.Parsec
import System.IO
import Scheme

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
