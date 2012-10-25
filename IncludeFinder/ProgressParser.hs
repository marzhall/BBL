module ProgressParser (includes) where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

include :: Parser String
include = do
              char '{'
              fileName <- many1 $ alphaNum <|> noneOf "\\/?*\"><|&}"
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

--testString  = "{lol.bat}\nhdjklasdhsjk\n{herp.derp}"
--testString2 = "{&preprocessor}"
--testString3 = "{one.p}{two.p}"
--testString4 = "{one.p}lol{two.p}butts{three.p}"

--main = do
        --case parse includes "test" testString of
            --Left err     -> print err
            --Right result -> print result
        --case parse includes "test2" testString2 of
            --Left err     -> print err
            --Right result -> print result
        --case parse includes "test3" testString3 of
            --Left err     -> print err
            --Right result -> print result
        --case parse includes "test4" testString4 of
            --Left err     -> print err
            --Right result -> print result
