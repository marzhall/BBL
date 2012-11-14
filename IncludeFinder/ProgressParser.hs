module ProgressParser (includes) where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

escapedDoubleQuote :: Parser String
escapedDoubleQuote = do
   many $ noneOf "~\""
   char '~'
   char '\"'
   return ""

escapedSingleQuote :: Parser String
escapedSingleQuote = do
   many $ noneOf "~\'"
   char '~'
   char '\''
   return ""

doubleQuoted :: Parser String
doubleQuoted = do
   char '\"'
   many $ try escapedDoubleQuote 
   many $ noneOf "\""
   char '\"'
   return ""

singleQuoted :: Parser String
singleQuoted = do
   char '\''
   many $ try escapedSingleQuote 
   many $ noneOf "\'"
   char '\''
   return ""

quoted :: Parser String
quoted = do
   try singleQuoted <|> doubleQuoted

endComment :: Parser String
endComment = do
   many $ noneOf "*"
   char '*'
   char '/'
   return ""

randomStar :: Parser String
randomStar = do
   many $ noneOf "*"
   char '*'
   noneOf "/"
   return ""

comment :: Parser String
comment = do
        char '/' >> char '*'
        many $ try randomStar 
        endComment
        return ""

include :: Parser String
include = do
    char '{'
    fileName <- many1 $ alphaNum <|> noneOf "\\/?*\"><|&} "
    many $ noneOf "}"
    char '}'
    return fileName

junk :: Parser String
junk = do
    many1 $ noneOf "/{\'\""
    return ""

preprocessor :: Parser String
preprocessor = do
    char '{'
    oneOf "&1234567890"
    instructions <- many $ noneOf "}"
    char '}'
    return ""

includes :: Parser [String]
includes  = do
    bracedCode <- many $ try junk <|> try comment <|> try quoted <|> try preprocessor <|> try include
    eof
    return $ populated bracedCode
    where
        populated list = filter (\x -> x /= "") list
