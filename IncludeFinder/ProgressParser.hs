module ProgressParser (includes) where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

include :: Parser String
include = do
              char '{'
              fileName <- many1 $ alphaNum <|> noneOf "\\/?*\"><|&} "
              many $ noneOf "}"
              char '}'
              return fileName

junk :: Parser String
junk = do
       many1 $ noneOf "{"
       return ""

preprocessor :: Parser String
preprocessor = do
                  char '{'
                  char '&'
                  instructions <- many $ noneOf "}"
                  char '}'
                  return ""

includes :: Parser [String]
includes  = do
            bracedCode <- many $ try preprocessor <|> include <|> junk
            eof
            return $ populated bracedCode
            where 
                populated list = filter (\x -> x /= "") list
