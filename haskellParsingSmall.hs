import Control.Monad
import Data.Char
import Data.Map
import System.Exit
import System.IO
import Text.Parsec
import FieldsParser
import DatabaseCommands

-- for interacting on the command prompt
getDB = do
        let filename = "progressDB.txt"
        handle <- openFile filename ReadMode
        contents <- hGetContents handle
        return $ parse tables filename contents

main =  do
        let filename = "progressDB.txt"
        handle <- openFile filename ReadMode
        contents <- hGetContents handle
        case parse tables filename contents of
           Left err -> do
                       print err
                       System.Exit.exitWith $ ExitFailure 1
           Right database -> forever $ do
                             putStrLn "What would you like to do? (For help, type \"Halp.\")"
                             answer <- getLine
                             case member answer commandList of
                                 True -> (commandList ! answer) database
                                 False -> do 
                                          print "That's not an option. Here are your commands: "
                                          (commandList ! "halp") database

