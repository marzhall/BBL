module ProgressParser (includes) where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

comment :: Parser String
comment = do
    char '/' >> char '*' >> many (noneOf "*") >> char '*' >> char '/'
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
    many1 $ noneOf "/{"
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
    bracedCode <- many $ try comment <|> try preprocessor <|> try include <|> junk
    eof
    return $ populated bracedCode
    where
        populated list = filter (\x -> x /= "") list
