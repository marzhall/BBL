module DatabaseCommands (commandList) where

import Data.Map
import System.Exit
import System.IO
import FieldsParser

intersect    :: (Ord k, Show k) => Map String (Map k Field) -> IO ()
intersect db = do 
               putStrLn "What two tables would you like to see the intersection of?"
               print "Table One:"
               mapOne <- getLine
               if (member mapOne db) then do
                   print "Table Two:"
                   mapTwo <- getLine
                   if (member mapTwo db) then
                       mapM_ print (keys (intersection (db ! mapOne) (db ! mapTwo)))
                   else
                       print "The second table does not exist; returning you to main."
               else
                   print "The first table does not exist; returning you to main."

printFields    :: Show k => Map String (Map k Field) -> IO () 
printFields db = do
                 putStrLn "Which table would you like to see the fields of?"
                 answer <- getLine
                 if (member answer db) then 
                    mapM_ print $ keys (db ! answer)
                 else
                    print "Not a table."

              
--findField    :: Show k => Map String (Map k Field) -> IO () 
findField db = do
               putStrLn "What field would you like to search for?"
               answer <- getLine
               mapM_ print $ keys $ Data.Map.filter (\f -> member answer f) db

commandList :: Map [Char] (Map String (Map String Field) -> IO ())
commandList = fromList [  ("halp", (\n -> mapM_ print $ keys commandList)) --eat the db in order to match the other functions
                        , ("intersection", intersect)
                        , ("printfields", printFields)
                        , ("printtables", (\n -> mapM_ print $ keys n))
                        , ("findfield", findField)
                        , ("exit", (\n -> System.Exit.exitWith ExitSuccess))]
