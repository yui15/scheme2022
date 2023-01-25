module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import SExpression
import Eval

main :: IO ()
main = undefined -- print . readExpr . head =<< getArgs
