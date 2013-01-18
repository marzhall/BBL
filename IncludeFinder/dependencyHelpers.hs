module DependencyHelpers ( prettyPrinter
                         , dependencyFold
                         , parseFile
                         , includeDB
                         , mapConcurrent) where

import Control.Seq
import Control.Parallel
import Control.Parallel.Strategies
import Text.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import Data.Map as M hiding (foldl, filter)
import Data.List as D
import ProgressParser
import qualified Data.ByteString.Char8 as B
import System.IO.Unsafe (unsafePerformIO) 

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

