module SExpression where

import Text.ParserCombinators.Parsec
import System.Environment
import Data.Char

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char
    deriving (Eq)

instance Show LispVal where
    show = showLispVal

showLispVal :: LispVal -> String
showLispVal e = case e of 
    String contents -> "\"" ++ contents ++ "\""
    Atom name       -> name
    Number contents -> show contents
    Bool True       -> "#t"
    Bool False      -> "#f"
    List contents   -> "(" ++ unwordsList contents ++ ")"
    DottedList hd tl -> "(" ++ unwordsList hd ++ "." ++ showLispVal tl ++ ")"

sample :: LispVal 
sample = List [Atom "+", Number 1 , Number 2]

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

{- |
parseExpr
>>> testparse parseExpr "(define (f n) (if (= n 0) 1 (* n (f (- n 1)))))"
Right (List [Atom "define",List [Atom "f",Atom "n"],List [Atom "if",List [Atom "=",Atom "n",Number 0],Number 1,List [Atom "*",Atom "n",List [Atom "f",List [Atom "-",Atom "n",Number 1]]]]])
-}

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "parse error: " ++ show err
    Right v -> v

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseLispVal :: Parser LispVal
parseLispVal = parseExpr

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

{- |
parseAtom
>>> testparse parseAtom "+-abc"
Right (Atom "+-abc")
>>> testparse parseAtom " #t "
Right (Bool True)
-}

parseAtom :: Parser LispVal
parseAtom = do spaces
               first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> case splitAt 2 atom of
                            ("#\\", "newline") -> Character '\n'
                            ("#\\", "space")   -> Character ' '
                            ("#\\", [c])
                                | isAlpha c    -> Character c
                                | otherwise    -> error "parseAtom"
                            _                  -> Atom atom

{- |
parseNumber 
>>> testparse parseNumber "123"
Right (Number 123)
-}

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

{- |
parseList
>>> testparse parseList "hoge huga +"
Right (List [Atom "hoge",Atom "huga",Atom "+"])
-}

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces1

{- |
parseDottedList
>>> testparse parseDottedList "hoge huga . hage"
Right (DottedList [Atom "hoge",Atom "huga"] (Atom "hage"))
-}

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

{- |
parseQuoted
>>> testparse parseQuoted "'hoge"
Right (List [Atom "quote",Atom "hoge"])
-}

parseQuoted :: Parser LispVal
parseQuoted = do
   char '\''
   x <- parseExpr
   return $ List [Atom "quote", x]

testparse :: Parser a -> String -> Either ParseError a
testparse p = parse p "test"

parse' :: Parser a -> String -> a
parse' p s = case testparse p s of
    Right r -> r
    Left err -> error (show err)

{- |
showVal
>>> testshow showVal (String "hoge")
"hoge"
>>> testshow showVal (List [Atom "define",List [Atom "f",Atom "n"],List [Atom "if",List [Atom "=",Atom "n",Number 0],Number 1,List [Atom "*",Atom "n",List [Atom "f",List [Atom "-",Atom "n",Number 1]]]]])
(define (f n) (if (= n 0) 1 (* n (f (- n 1)))))
>>> testshow showVal (parse' parseLispVal "(define (f n) (if (= n 0) 1 (* n (f (- n 1)))))")
(define (f n) (if (= n 0) 1 (* n (f (- n 1)))))
>>> ast = parse' parseLispVal "(define (f n) (if (= n 0) 1 (* n (f (- n 1)))))"
>>> ast == parse' parseLispVal (showVal ast)
True
-}

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

testshow :: (LispVal -> String) -> LispVal -> IO ()
testshow s v = putStrLn (s v)
