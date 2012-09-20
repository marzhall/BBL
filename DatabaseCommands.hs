module DatabaseCommands (  commandState 
                         , commandParser) where 
import System.IO
import System.Exit
import FieldsParsec
import Data.Map
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data commandState = {  database :: (Map [Char] (Map String (Map [Char] Field)))
                     , printFunc :: [Char] -> IO ()
                     , readFunc :: IO ()}

commandParser                   :: [char] -> commandState -> IO ()
commandParser command localState = case parse userCommands "command" command of
                                                        Left error -> (printFunc localState) error
                                                        Right result -> (printFunc localState) $ foldl commandExecutor (database commandState) result

commandExecutor database

userCommand :: Parser (String, [[Char]])
userCommand = do
              command <- many1 alphaNum
              spaces
              arguments <- many $ noneOf "\n|"
              return (command, words arguments)

userCommands :: Parser [(String, [[Char]])]
userCommands = do
               list <- many userCommand
               return list
                        
--intersect            :: ([Char] -> IO ()) -> IO String -> Map String (Map [Char] Field) -> IO ())
intersect localState = do 
                       putStrLn "What two tables would you like to see the intersection of?"
                       (printFunc localState) "Table One:"
                       mapOne <- (readFunc localState)
                       if (member mapOne db) then do
                          (printFunc localState) "Table Two:"
                          mapTwo <- (readFunc localState)
                          if (member mapTwo db) then
                               keys $ intersection (db ! mapOne) (db ! mapTwo)
                          else
                              (printFunc localState) "The second table does not exist; returning you to main."
                       else
                          (printFunc localState) "The first table does not exist; returning you to main."

--printFields            :: commandState -> IO ()) 
printFields localState = do
                         (printFunc localState) "Which table would you like to see the fields of?"
                         answer <- (readFunc localState)
                         if (member answer db) then 
                            keys (db ! answer)
                         else
                            (printFunc localState) "Not a table."

--printFieldInfo            :: commandState -> IO ()) 
printFieldInfo localState = do
                            (printFunc localState) "Which table would you like to see a field's info from?"
                            answer <- (readFunc localState)
                            if (member answer db) then do
                               (printFunc localState) "Which field would you like to see the info of?"
                               fieldToPrint <- (readFunc localState)
                               if (member fieldToPrint (db ! answer)) then 
                                  ((db ! answer) ! fieldToPrint)
                               else
                                  (printFunc localState) ("Not a field of table " ++ answer)
                            else
                               (printFunc localState) "Not a table."
              
--findField            :: commandState -> IO ()) 
findField localState = do
                       (printFunc localState) "What field would you like to search for?"
                       answer <- (readFunc localState)
                       keys $ Data.Map.filter (\f -> member answer f) db

--commandList            :: ([Char] -> IO ()) -> IO String -> Map [Char] (Map String (Map [Char] Field) -> IO ())
commandList localState = fromList [  ("exit", (\_ -> System.Exit.exitWith ExitSuccess))
                                           , ("findfield", findField localState)
                                           , ("halp", (\_ -> mapM_ (printFunc localState) $ keys (commandList localState))) --eat the db in order to match the other functions
                                           , ("info", printFieldInfo localState)
                                           , ("intersection", intersect localState)
                                           , ("printfields", printFields localState)
                                           , ("printtables", (\database -> mapM_ (printFunc localState) $ keys database))]
