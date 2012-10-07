module DatabaseCommands (commandList) where

import Data.Map hiding (map)
import System.Exit
import System.IO
import FieldsParser
import Data.List.Utils

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

--weakIntersect                            :: ([Char] -> IO ()) -> IO String -> [String] -> Map String (Map [Char] Field) -> IO ()
--weakIntersect printFunc readFunc args db =  if length args /= 2 then do 
                                               --putStrLn "What two tables would you like to see the intersection of?"
                                               --printFunc "Table One:"
                                               --mapOne <- getLine
                                               --if (member mapOne db) then do
                                                   --printFunc "Table Two:"
                                                   --mapTwo <- getLine
                                                   --if (member mapTwo db) then
                                                      --mapM_ printFunc (keys (weakIntersection (db ! mapOne) (db ! mapTwo)))
                                                   --else
                                                      --printFunc "The second table does not exist; returning you to main."
                                               --else
                                                  --printFunc "The first table does not exist; returning you to main."
                                            --else do
                                               --if (member (head args) db) then do
                                                   --if (member (args !! 1) db) then
                                                      --mapM_ printFunc (weakIntersection (db ! (head args)) (db ! (args !! 1)))
                                                   --else
                                                      --printFunc "The second table does not exist; returning you to main."
                                               --else
                                                  --printFunc "The first table does not exist; returning you to main."
                                            --where
                                                --weakIntersection table1 table2 = do
                                                                                 --let toMatch = concat (map (wordsWhen (== '-')) (keys table1))
                                                                                 --mapM_ printFunc $ show $ fromList $ zip toMatch (map (\x -> Prelude.filter (contains x) toMatch) (keys table2))

intersect                            :: ([Char] -> IO ()) -> IO String -> [String] -> Map String (Map [Char] Field) -> IO ()
intersect printFunc readFunc args db =  if length args /= 2 then do 
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
                                        else do
                                           if (member (head args) db) then do
                                               if (member (args !! 1) db) then
                                                  mapM_ printFunc (keys (intersection (db ! (head args)) (db ! (args !! 1))))
                                               else
                                                  printFunc "The second table does not exist; returning you to main."
                                           else
                                              printFunc "The first table does not exist; returning you to main."

printFields                            :: ([Char] -> IO ()) -> IO String -> [String] -> Map String (Map [Char] Field) -> IO ()
printFields printFunc readFunc args db = if length args /= 1 then do 
                                            putStrLn "Which table would you like to see the fields of?"
                                            answer <- getLine
                                            if (member answer db) then 
                                               mapM_ printFunc $ keys (db ! answer)
                                            else
                                               printFunc "Not a table."
                                         else do
                                            if (member (head args) db) then 
                                               mapM_ printFunc $ keys (db ! (head args))
                                            else
                                               printFunc "Not a table."

printFieldInfo                            :: ([Char] -> IO ()) -> IO String -> [String] -> Map String (Map [Char] Field) -> IO ()
printFieldInfo printFunc readFunc args db = if length args /= 2 then do 
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
                                            else
                                               if (member (head args) db) then do
                                                  if (member (args !! 1) (db ! (head args))) then 
                                                     printFunc $ show ((db ! (head args)) ! (args !! 1))
                                                  else
                                                     printFunc ((args !! 1) ++ " is not a field of table " ++ (head args))
                                               else
                                                  printFunc ((head args) ++ " is not a table.")
              
findField                            :: ([Char] -> IO ()) -> IO String -> [String] -> Map String (Map [Char] Field) -> IO ()
findField printFunc readFunc args db = if length args /= 1 then do 
                                          putStrLn "What field would you like to search for?"
                                          answer <- getLine
                                          mapM_ printFunc $ keys $ Data.Map.filter (\f -> member answer f) db
                                       else
                                          mapM_ printFunc $ keys $ Data.Map.filter (\f -> member (head args) f) db

commandList                    :: ([Char] -> IO ()) -> IO String -> Map [Char] ([String] -> Map String (Map [Char] Field) -> IO ())
commandList printFunc readFunc = fromList [  ("exit", (\_ _ -> System.Exit.exitWith ExitSuccess))
                                           , ("find", findField printFunc readFunc)
                                           , ("halp", (\_ _ -> mapM_ printFunc $ keys (commandList printFunc readFunc)))
                                           , ("info", printFieldInfo printFunc readFunc)
                                           , ("intersect", intersect printFunc readFunc)
                                           , ("printf", printFields printFunc readFunc)
                                           , ("printt", (\_ database -> mapM_ printFunc $ keys database))]
