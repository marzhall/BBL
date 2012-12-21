module LanguageDefinition (openedge) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

openedge :: TokenParser st
openedge = makeTokenParser openedgeDef

{-- Reserved Strings --}

labels = ["do ", "end", "proc", "procedure", "func", "function", "return", "returns", "next", "last"]

definitions = [  "def", "defi", "defin", "define"
               , "var", "vari", "varia", "variab", "variabl", "variable"
               , "input", "input-output", "output", "param", "parameter"
               , "buffer" ]

queryOperators = ["for", "first", "each", "exclusive-lock", "no-lock"]

types = [  "int", "inte", "integ", "intege", "integer"
         , "log", "logi", "logic", "logica", "logical"
         , "date", "time", "datetime"
         , "char", "chara", "charac", "charact", "characte", "character"]

values = ["true", "false", "yes", "no", "now", "today", "etime", "?"]

logicals = ["if", "else", "then", "GE", "GT", "LT", "LE", "EQ"]



openedgeDef :: LanguageDef st
openedgeDef = LanguageDef
           { commentStart   = "/*"
           , commentEnd     = "*/"
           , commentLine    = ""
           , nestedComments = True
           , identStart     = letter
           , identLetter	= alphaNum <|> oneOf "_#-"
           , opStart	 = opLetter openedge
           , opLetter	 = oneOf ":!#$%&*+./<=>?@\\^|-~"
           , reservedOpNames= []
           , reservedNames  = labels ++ definitions ++ queryOperators ++ types ++ values ++ logicals
           , caseSensitive  = False
           }
