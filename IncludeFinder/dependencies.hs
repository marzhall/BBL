import ProgressParser
import Data.Map hiding (foldl, filter)
import Data.List
import System.Directory
import Text.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import qualified Data.ByteString.Char8 as B
import System.Environment
import Control.Monad
import System.IO

parseFile          :: String -> IO (Either ParseError [String])
parseFile fileName = catch (readFile fileName >>= (\contents -> return $ parse includes fileName (B.pack contents)))
                           (\_ -> return $ Left $ newErrorMessage (UnExpect ": file cannot be read or found.") (newPos fileName 0 0)) 

prettyPrinter          :: [String] -> String
prettyPrinter fileList = foldl (\aggregator file -> (file ++ "\n") ++ aggregator) "" populatedFileList
    where populatedFileList = (filter ((/=) "") fileList)  

dependencyFold                          :: Map String [String] -> String -> String -> IO (Map String [String])
dependencyFold dependMap child fileName = do
    parsedFile <- parseFile fileName
    case parsedFile of
        Left err -> return $ insertWith (++) fileName ((show err):(child:(dependMap ! child))) dependMap
        Right results -> if results /= [] 
                            then do
                                parents <- mapM (dependencyFold newDB fileName) results
                                return $ unionsWith (++) parents 
                            else
                                return $ newDB
            where newDB = insertWith (++) fileName (child:(dependMap ! child)) dependMap

main = do
    args <- getArgs 
    dependencyDB <- do
        catch (readFile "dependencies.cache" >>= (\contents -> return $ fromList $ read contents))
              (\_ -> do
                  let askUser = (>=) 1 (length args)
                  directory <- if askUser 
                      then do
                          print "What's the directory, boss?" 
                          getLine
                      else return (args !! 1)

                  fileList <- getDirectoryContents directory
                  dependencyFold <- mapM (dependencyFold (fromList [("", [])]) "") fileList
                  let uniquedMap = Data.Map.map nub $ unionsWith (++) dependencyFold
                  writeFile "dependencies.cache" (show $ toList $ Data.Map.map nub $ unionsWith (++) dependencyFold)
                  return uniquedMap)

    forever $ do
        print "What's the file you need, chief?" 
        filename <- getLine
        let result = if (member filename dependencyDB) then
                         (dependencyDB ! filename)
                     else
                         ["Couldn't find this file in the directory we read."]
        writeFile (filename ++ ".dependencies") ((prettyPrinter result) ++ "\n")
