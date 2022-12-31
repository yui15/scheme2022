module Main where

import System.Environment
import Data.Char

main :: IO ()
main = interact . drive . run =<< readFile . head =<< getArgs

drive :: ([String] -> [String]) -> (String -> String)
drive f = unlines . f . lines

run :: String -> ([String] -> [String])
run prog = map (map toUpper)
