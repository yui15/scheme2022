module SExpression where

import Text.ParserCombinators.Parsec hiding (spaces)

data SExpr
    = Atom String
    | List [SExpr]
    | DottedList [SExpr] SExpr
    | Number Integer
    | String String
    | Bool Bool
    deriving (Eq, Show)

showSExpr :: SExpr -> String
showSExpr e = case e of 
    String contents -> "\"" ++ contents ++ "\""
    Atom name       -> name
    Number contents -> show contents
    Bool True       -> "#t"
    Bool False      -> "#f"
    List contents   -> "(" ++ unwordsList contents ++ ")"
    DottedList hd tl -> "(" ++ unwordsList hd ++ "." ++ showSExpr tl ++ ")"

-- instance Show SExpr where 
--     show :: SExpr -> String
--     show = showSExpr

unwordsList :: [SExpr] -> String
unwordsList = unwords . map showSExpr

sample :: SExpr 
sample = List [Atom "+", Number 1 , Number 2]

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
    