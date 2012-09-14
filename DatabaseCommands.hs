module DatabaseCommands (commandList) where

import Data.Map
import System.Exit
import System.IO
import FieldsParser

--intersect    :: (Ord k, Show k) => Map String (Map k Field) -> IO ()
intersect printFunc readFunc db = do 
                         putStrLn "What two tables would you like to see the intersection of?"
                         printFunc "Table One:"
                         mapOne <- getLine
                         if (member mapOne db) then do
                             printFunc "Table Two:"
                             mapTwo <- getLine
                             if (member mapTwo db) then
                                 mapM_ printFunc (keys (intersection (db ! mapOne) (db ! mapTwo)))
                             else
                                 printFunc "The second table does not exist; returning you to main."
                         else
                             printFunc "The first table does not exist; returning you to main."

--printFields    :: Show k => Map String (Map k Field) -> IO () 
printFields printFunc readFunc db = do
                           putStrLn "Which table would you like to see the fields of?"
                           answer <- getLine
                           if (member answer db) then 
                              mapM_ printFunc $ keys (db ! answer)
                           else
                              printFunc "Not a table."

              
--findField    :: Show k => Map String (Map k Field) -> IO () 
findField printFunc readFunc db = do
                         putStrLn "What field would you like to search for?"
                         answer <- getLine
                         mapM_ printFunc $ keys $ Data.Map.filter (\f -> member answer f) db

--commandList :: Map [Char] (Map String (Map String Field) -> IO ())
commandList printFunc readFunc = fromList [  ("halp", (\_ -> mapM_ printFunc $ keys (commandList printFunc readFunc))) --eat the db in order to match the other functions
                                           , ("intersection", intersect printFunc readFunc)
                                           , ("printfields", printFields printFunc readFunc)
                                           , ("printtables", (\database -> mapM_ printFunc $ keys database))
                                           , ("findfield", findField printFunc readFunc)
                                           , ("exit", (\_ -> System.Exit.exitWith ExitSuccess))]
