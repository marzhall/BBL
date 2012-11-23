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
   char '*'
   char '/'
   return ""

randomStar :: Parser String
randomStar = do
   char '*'
   notFollowedBy $ char '/'
   return ""

randomSlash :: Parser String
randomSlash = do
   char '/'
   notFollowedBy $ char '*'
   return ""

commentJunk :: Parser String
commentJunk = do
   many1 $ try $ noneOf "*/"
   return ""

comment :: Parser String
comment = do
        char '/' >> char '*'
        many $ try commentJunk <|> try comment <|> try randomSlash <|> try randomStar
        endComment
        return ""

includeJunk :: Parser String
includeJunk = do
   many1 $ noneOf "{}"

include :: Parser String
include = do
    char '{'
    many $ char '"' <|> char '\''
    fileName <- many1 $ alphaNum <|> noneOf "\\/?*\"'><|&}{ "
    try $ many $ includeJunk <|> preprocessor 
    char '}'
    return fileName

junk :: Parser String
junk = do
    many1 $ noneOf "/{\'\""
    return ""

preprocessor :: Parser String
preprocessor = do
    char '{'
    lookAhead $ oneOf "{&1234567890"
    many $ try includeJunk <|> preprocessor
    instructions <- many $ noneOf "}"
    char '}'
    return ""

includes :: Parser [String]
includes  = do
    bracedCode <- many $ try junk <|> try randomStar <|> try randomSlash <|> try comment <|> try quoted <|> try preprocessor <|> try include
    eof
    return $ populated bracedCode
    where
        populated list = filter (\x -> x /= "") list
