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
import Control.Parallel
import Control.Parallel.Strategies
import Control.Seq
import System.IO
import System.IO.Unsafe (unsafePerformIO) 
import System.Exit

parseFile          :: String -> IO (Either ParseError [String])
parseFile fileName = catch (readFile fileName >>= (\contents -> return $ parse includes fileName (B.pack contents)))
                           (\_ -> return $ Left $ newErrorMessage (UnExpect ": file cannot be read or found.") (newPos fileName 0 0)) 

prettyPrinter          :: [String] -> String
prettyPrinter fileList = foldl (\aggregator file -> (file ++ "\n") ++ aggregator) "" populatedFileList
    where populatedFileList = filter (endsWith ".p") (filter ((/=) "") fileList)  

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

mapConcurrent f = return. parMap rpar (unsafePerformIO . f)

endsWith ending arrayToCheck = foldl (\acc (endSegment, stringSegment) -> acc && (endSegment == stringSegment)) True (zip (reverse ending) (reverse arrayToCheck))

sourced = filter (\file -> endsWith ".p" file || endsWith ".i" file || endsWith ".cls" file)

main = do
    args <- getArgs 
    dependencyDB <- do
        catch (readFile "dependencies.cache" >>= (\contents -> return $ fromList $ read contents))
              (\_ -> do
                  fileList <- getCurrentDirectory >>= getDirectoryContents
                  topLevelIncludes <-  mapConcurrent includeDB (sourced fileList)
                  let dependencyMaps = parMap rpar (dependencyFold (fromList [("", [])]) (fromList topLevelIncludes) "") fileList
                  let uniquedMap = M.map nub $ unionsWith (++) dependencyMaps
                  writeFile "dependencies.cache" (show $ toList $ M.map nub $ unionsWith (++) dependencyMaps)
                  System.Exit.exitWith $ ExitSuccess
                  return uniquedMap)

    forever $ do
        print "What's the file you need, chief?" 
        filename <- getLine
        let result = if (member filename dependencyDB) then
                         (dependencyDB ! filename)
                     else
                         ["Couldn't find this file in the directory we read."]
        writeFile (filename ++ ".dependencies") ((prettyPrinter result) ++ "\n")
