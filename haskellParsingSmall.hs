import Control.Monad
import Data.Char
import Data.Map
import System.Exit
import System.IO
import Text.Parsec
import FieldsParser
import DatabaseCommands
import System.Environment
import TcpServer
import Control.Concurrent.Chan

main =  do
        let filename = "progressDB.txt"
        args <- getArgs
        let runServer = (>=) (length args) 1             --if there's an argument, we're running the server
        handle <- openFile filename ReadMode
        contents <- hGetContents handle
        case parse tables filename contents of
           Left err -> do
                       print err
                       System.Exit.exitWith $ ExitFailure 1
           Right database -> if runServer then do start database
                             else do
                                  let localCommands = (commandList print getLine)
                                  forever $ do
                                     putStrLn "What would you like to do? (For help, type \"Halp.\")"
                                     answer <- getLine
                                     case member answer localCommands of
                                         True -> (localCommands ! answer) database
                                         False -> do 
                                                  print "That's not an option. Here are your commands: "
                                                  (localCommands ! "halp") database
