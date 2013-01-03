module LanguageDefinition (openedge) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

openedge :: TokenParser st
openedge = makeTokenParser openedgeDef

{-- Reserved Strings --}

progressLabels = [  "do", "end", "proc", "procedure", "func", "function", "return", "returns"
          , "next", "last", "undo", "retry", "no-apply", "error", "while"]

definitions = [  "var", "vari", "varia", "variab", "variabl", "variable"
               , "input", "input-output", "output", "param", "parameter"
               , "buffer" ]

queryOperators = [  "for", "find", "first", "each", "exclusive-lock", "no-lock"
                  , "ge", "gt", "lt", "le", "eq"]

types = [  "int", "inte", "integ", "intege", "integer" , "decimal" , "int64"
         , "log", "logi", "logic", "logica", "logical"
         , "date", "time", "datetime" , "datetime-tz"
         , "char", "chara", "charac", "charact", "characte", "character"
         , "blob", "clob", "raw", "recid", "field", "like", "as"
         , "index", "primary", "unique", "temp-table", "table", "no-undo"]

values = ["true", "false", "yes", "no", "now", "today", "?"]

logicals = ["if", "else", "then", "case", "when"]

internalfunctions = ["can-find", "etime", "lookup", "asc", "chr", "avail", "available"]

operations = [  "ASSIGN", "UPDATE", "RUN", "def", "defi", "defin", "define", "DISP", "DISPLAY"
              , "MESSAGE"]


openedgeDef :: LanguageDef st
openedgeDef = LanguageDef
           { commentStart   = "/*"
           , commentEnd     = "*/"
           , commentLine    = ""
           , nestedComments = True
           , identStart     = letter
           , identLetter	= alphaNum <|> oneOf "_#-"
           , opStart	 = opLetter openedgeDef
           , opLetter	 = oneOf "!#$%&*+./<=>^|-~"
           , reservedOpNames = operations
           , reservedNames  = progressLabels ++ definitions ++ queryOperators ++ types 
               ++ values ++ logicals ++ internalfunctions
           , caseSensitive  = False
           }
