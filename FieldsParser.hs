module FieldsParser (  Field
                     , tables) where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Map

data Field = Field {  dataType :: String
                    , flag :: String 
                    , format :: String }
    deriving (Show, Eq)

getFieldName :: Parser String
getFieldName = many1 $ alphaNum <|> oneOf "-_#"

tableHeader :: Parser String
tableHeader = do
                newline
                many $ noneOf " "
                spaces
                name <- many1 $ alphaNum <|> char '-'
                newline
                newline
                many $ noneOf "\n"
                newline
                many $ noneOf "\n"
                newline
                option "" $ many1 newline
                return name

getDataType :: Parser String
getDataType = many1 $ alphaNum <|> oneOf "-[]"

getFlag :: Parser String
getFlag = many1 $ oneOf "vimc" --view component, index member, 
                               --mandatory, and case sensitive are the 
                               --flags used by progress
getFormat :: Parser String
getFormat = many $ noneOf "\n"  

fieldsWithoutFlags :: Parser (String, Field)
fieldsWithoutFlags = do
                     tempName <- getFieldName
                     spaces
                     tempType <- getDataType
                     spaces
                     tempFormat <- getFormat
                     newline
                     return $ (tempName, Field {  dataType = tempType
                                                , flag = ""
                                                , format = tempFormat})

fieldsWithFlags :: Parser (String, Field)
fieldsWithFlags = do
                  tempName <- getFieldName
                  spaces
                  tempType <- getDataType
                  spaces
                  tempFlag <- getFlag
                  spaces
                  tempFormat <- getFormat
                  newline
                  return $ ( tempName, Field {  dataType = tempType
                                              , flag = tempFlag
                                              , format = tempFormat})

field :: Parser (String, Field)
field = try fieldsWithFlags <|> fieldsWithoutFlags

fields :: Parser [(String, Field)]
fields = do inFields <- many field
            return inFields

table :: Parser (String, (Map (String) (Field)))
table = do
        tempName <- tableHeader
        tempFields <- fields
        return (tempName, fromList tempFields)

tables :: Parser (Map (String) (Map (String) (Field)))
tables = do
         database <- many1 table
         return $ fromList database
