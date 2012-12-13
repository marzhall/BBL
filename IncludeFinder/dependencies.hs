import ProgressParser
import Data.Map as M hiding (foldl, filter)
import Data.List as D
import Data.Foldable (foldlM)
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

dependencyFold                                     :: Map String [String] -> Map String [String] -> String -> String -> Map String [String]
dependencyFold dependMap includeMap child fileName = do
    if member fileName includeMap then
        if includes /= [] 
            then
                unionsWith (++) (D.map (dependencyFold dbWithNewFile includeMap fileName) includes)
        else
            dbWithNewFile
    else
        insertWith (++) fileName (("File not found"):(child:(dependMap ! child))) dependMap
    where includes = (includeMap ! fileName)
          dbWithNewFile = insertWith (++) fileName (child:(dependMap ! child)) dependMap

includeDB          :: String -> IO (String, [String])
includeDB fileName = do
    parsedFile <- parseFile fileName
    case parsedFile of
        Left err -> return $ (fileName, [show err])
        Right results -> return $ (fileName, results)

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
                  topLevelIncludes <- foldlM (\includeMap newfile -> (includeDB newfile) >>= (\newInclude -> return $ M.union (fromList [newInclude]) includeMap)) (fromList [("", [""])]) fileList
                  let dependencyMaps = D.map (dependencyFold (fromList [("", [])]) topLevelIncludes "") fileList
                  let uniquedMap = M.map nub $ unionsWith (++) dependencyMaps
                  writeFile "dependencies.cache" (show $ toList $ M.map nub $ unionsWith (++) dependencyMaps)
                  return uniquedMap)

    forever $ do
        print "What's the file you need, chief?" 
        filename <- getLine
        let result = if (member filename dependencyDB) then
                         (dependencyDB ! filename)
                     else
                         ["Couldn't find this file in the directory we read."]
        writeFile (filename ++ ".dependencies") ((prettyPrinter result) ++ "\n")
