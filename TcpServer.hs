module TcpServer(start) where

import Control.Concurrent.Chan
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPrint, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Data.Map
import DatabaseCommands
import FieldsParser

start    :: Map String (Map [Char] Field) -> IO ()
start db = withSocketsDo $ do
           sock <- listenOn $ PortNumber 9001
           sockHandler db sock

sockHandler         :: Map String (Map [Char] Field) -> Socket -> IO ()
sockHandler db sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ commandProcessor db handle
    sockHandler db sock

commandProcessor                 :: Map String (Map [Char] Field) -> Handle -> IO ()
commandProcessor database handle = do
     let localCommands = (commandList (hPrintLn handle) (hGetLine handle))
     hPutStrLn handle "What would you like to do? (For help, type \"Halp.\")"
     answer <- hGetLine handle
     case member answer localCommands of
         True -> do
                  (localCommands ! answer) database
                  commandProcessor database handle
         False -> do 
                  hPutStrLn handle "That's not an option. Here are your commands: "
                  (localCommands ! "halp") database
                  commandProcessor database handle
