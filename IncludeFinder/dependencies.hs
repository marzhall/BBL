import ProgressParser
import DependencyHelpers
import Options
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
import System.Exit
import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import Control.Parallel
import Control.Parallel.Strategies

endsWith ending arrayToCheck = foldl (\acc (endSegment, stringSegment) -> acc && (endSegment == stringSegment)) True (zip (reverse ending) (reverse arrayToCheck))

sourced = filter (\file -> endsWith ".p" file || endsWith ".i" file || endsWith ".cls" file || endsWith ".w")

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
