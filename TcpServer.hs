module TcpServer(start) where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Data.Map
import DatabaseCommands

--startListen :: IO ()
start db = withSocketsDo $ do
           sock <- listenOn $ PortNumber 9001
           sockHandler db sock

--sockHandler :: Socket -> IO ()
sockHandler db sock = do
                     (handle, _, _) <- accept sock
                      hSetBuffering handle NoBuffering
                      forkIO $ (commandProcessor db handle)
                      sockHandler sock

--commandProcessor :: Handle -> IO ()
commandProcessor database handle = do
                             let localCommands = (commandList (hGetLine handle) (hPutStrLn handle))
                             hPutStrLn handle "What would you like to do? (For help, type \"Halp.\")"
                             answer <- hGetLine handle
                             case member answer localCommands of
                                 True -> (localCommands ! answer) database
                                          commandProcessor handle
                                 False -> do 
                                          hPutStrLn handle "That's not an option. Here are your commands: "
                                          (localCommands ! "halp") database
                                          commandProcessor handle
