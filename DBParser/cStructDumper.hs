import Control.Monad
import Data.Char
import Data.Map
import System.Exit
import System.IO
import Text.Parsec
import FieldsParser

fieldType                     :: Map [Char] (Map [Char] Field) -> [Char] -> [Char] -> [Char] 
fieldType database field name = dataType ((database ! name) ! field)

fields               :: Map [Char] (Map [Char] Field) -> [Char] -> [Char] 
fields database name = Prelude.foldr (\field acc -> (fieldType database field name) ++ " " 
                                                     ++ field ++ "; \n "
                                                     ++ acc) "" (keys (database ! name))

structTables          :: Map [Char] (Map [Char] Field) -> [Char]
structTables database = Prelude.foldr (\table acc -> "typedef struct {\n" 
                                                  ++ (fields database table) ++ "} " 
                                                  ++ table ++";\n\n" 
                                                  ++ acc) "" (keys database)

main =  do
        let filename = "progressDB.txt"
        handle <- openFile filename ReadMode
        contents <- hGetContents handle
        case parse tables filename contents of
           Left err -> do
                       print err
                       System.Exit.exitWith $ ExitFailure 1
           Right database -> do
                             structFileHandle <- openFile "CStructs.c" WriteMode
                             hPutStr structFileHandle $ structTables database
                             


