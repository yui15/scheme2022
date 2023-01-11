module SExpression where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    deriving (Eq, Show)

showLispVal :: LispVal -> String
showLispVal e = case e of 
    String contents -> "\"" ++ contents ++ "\""
    Atom name       -> name
    Number contents -> show contents
    Bool True       -> "#t"
    Bool False      -> "#f"
    List contents   -> "(" ++ unwordsList contents ++ ")"
    DottedList hd tl -> "(" ++ unwordsList hd ++ "." ++ showLispVal tl ++ ")"

-- instance Show LispVal where 
--     show :: LispVal -> String
--     show = showLispVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showLispVal

sample :: LispVal 
sample = List [Atom "+", Number 1 , Number 2]

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left  err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

parseLispVal :: Parser LispVal
parseLispVal = parseAtom <|> parseString <|> parseNumber

{- | parseString
>>> parse parseString "parseString" "\"\\\"hoge\\\" is\""
Right (String "\"hoge\" is")
-}
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"\n\r\t\\" <|> parseEscChar)
                --  x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseEscChar :: Parser Char
parseEscChar = parseDQ <|> parseLF <|> parseCR <|> parseTAB <|> parseBS

parseDQ, parseLF, parseCR, parseTAB, parseBS :: Parser Char
parseDQ = char '\\' *> char '"'
parseLF = '\n' <$ (char '\\' *> char 'n')
parseCR = '\r' <$ (char '\\' *> char 'r')
parseTAB = '\t' <$ (char '\\' *> char 't')
parseBS = char '\\' *> char '\\'

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit
