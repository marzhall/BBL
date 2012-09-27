module DatabaseCommands (commandList) where

import Data.Map
import System.Exit
import System.IO
import FieldsParser

intersect                       :: ([Char] -> IO ()) -> IO String -> Map String (Map [Char] Field) -> IO ()
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

printFields                       :: ([Char] -> IO ()) -> IO String -> Map String (Map [Char] Field) -> IO ()
printFields printFunc readFunc db = do
                                    putStrLn "Which table would you like to see the fields of?"
                                    answer <- getLine
                                    if (member answer db) then 
                                       mapM_ printFunc $ keys (db ! answer)
                                    else
                                       printFunc "Not a table."

printFieldInfo                       :: ([Char] -> IO ()) -> IO String -> Map String (Map [Char] Field) -> IO ()
printFieldInfo printFunc readFunc db = do
                                       printFunc "Which table would you like to see a field's info from?"
                                       answer <- getLine
                                       if (member answer db) then do
                                          printFunc "Which field would you like to see the info of?"
                                          fieldToPrint <- getLine
                                          if (member fieldToPrint (db ! answer)) then 
                                             printFunc $ show ((db ! answer) ! fieldToPrint)
                                          else
                                             printFunc ("Not a field of table " ++ answer)
                                       else
                                          printFunc "Not a table."
              
findField                       :: ([Char] -> IO ()) -> IO String -> Map String (Map [Char] Field) -> IO ()
findField printFunc readFunc db = do
                                  putStrLn "What field would you like to search for?"
                                  answer <- getLine
                                  mapM_ printFunc $ keys $ Data.Map.filter (\f -> member answer f) db

commandList                    :: ([Char] -> IO ()) -> IO String -> Map [Char] (Map String (Map [Char] Field) -> IO ())
commandList printFunc readFunc = fromList [  ("exit", (\_ -> System.Exit.exitWith ExitSuccess))
                                           , ("findfield", findField printFunc readFunc)
                                           , ("halp", (\_ -> mapM_ printFunc $ keys (commandList printFunc readFunc))) --eat the db in order to match the other functions
                                           , ("info", printFieldInfo printFunc readFunc)
                                           , ("intersection", intersect printFunc readFunc)
                                           , ("printfields", printFields printFunc readFunc)
                                           , ("printtables", (\database -> mapM_ printFunc $ keys database))]
