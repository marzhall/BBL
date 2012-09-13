module DatabaseCommands (  checkMembership
                         , intersect
                         , printFields
                         , commandList) where
import Data.Map
import System.Exit
import System.IO
import FieldsParser

checkMembership          :: Ord k => k -> Map k a -> Bool
checkMembership item map = if (member item map) == False then do
                              False
                           else 
                              True
                           
intersect    :: (Ord k, Show k) => Map String (Map k Field) -> IO ()
intersect db = do 
               putStrLn "What two tables would you like to see the intersection of?"
               print "Table One:"
               mapOne <- getLine
               if (checkMembership mapOne db) then do
                   print "Table Two:"
                   mapTwo <- getLine
                   if (checkMembership mapTwo db) then
                       print (keys (intersection (db ! mapOne) (db ! mapTwo)))
                   else
                       print "The second table does not exist; returning you to main."
               else
                   print "The first table does not exist; returning you to main."

printFields    :: Show k => Map String (Map k Field) -> IO () 
printFields db = do
                 putStrLn "Which table would you like to see the fields of?"
                 answer <- getLine
                 if checkMembership answer db then 
                    print $ keys (db ! answer)
                 else
                    print "Not a table."

              
--findField    :: Show k => Map String (Map k Field) -> IO () 
findField db = do
               putStrLn "What field would you like to search for?"
               answer <- getLine
               print $ keys $ Data.Map.filter (\f -> member db f) db

commandList :: Map [Char] (Map String (Map String Field) -> IO ())
commandList = fromList [  ("halp", (\n -> print $ keys commandList)) --eat the db in order to match the other functions
                            , ("intersection", intersect)
                            , ("printfields", printFields)
                            , ("printtables", (\n -> print $ keys n))
                            , ("findfield", (findField))
                            , ("exit", (\n -> System.Exit.exitWith ExitSuccess))]
