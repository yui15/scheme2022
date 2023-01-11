module Main where

import System.Environment
import Data.Char
import SExpression
import Text.ParserCombinators.Parsec

-- main :: IO ()
-- main = interact . drive . run =<< readFile . head =<< getArgs

-- drive :: ([String] -> [String]) -> (String -> String)
-- drive f = unlines . f . lines

-- run :: String -> ([String] -> [String])
-- run prog = map (map toUpper)

main :: IO ()
main = do
    args <- getArgs
    print $ parse parseLispVal "Lisp" $ head args
