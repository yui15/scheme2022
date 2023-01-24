module Main where

import Control.Monad
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

-- main :: IO ()
-- main = do
--     args <- getArgs
--     print $ parse parseLispVal "Lisp" $ head args

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM showVal $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
