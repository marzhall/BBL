import ProgressParser
import Text.Parsec
import System.Environment
import Control.Monad
import System.IO
import Data.Tree


--Need a custom tree to do this.
--
--parseIncludeTree                :: [String] -> IO [(String, a)]
parseIncludeTree []             = return []
parseIncludeTree (current:rest) = do
        contents <- readFile current
        case parse includes "" contents of
            Left err -> do more <- parseIncludeTree rest
                           return $  [(current, [err])] : more
            Right results -> 
                        do more <- parseIncludeTree results
                           stillmore  <- parseIncludeTree rest
                           return  $ (current, more) : stillmore

main = do
    args <- getArgs 
    let askUser = (>=) 1 (length args)
    filename <- if askUser 
        then do
                print "What's the filename, boss?" 
                getLine
        else return (args !! 1)

    includeTree <- parseIncludeTree [filename]
    mapM print $ drawTree includeTree

parseOnFrom res [] = return res
parseOnFrom res (current:rest) = do
  contents <- readFile current
  case parse includes "" contents of
    Left err       -> parseOnFrom (res ++ [(current, [err])]) rest
    Right results  -> do more <- parseOnFrom res results
                         parseOnFrom (res ++ more) rest


[(String, [(String, [String])])]
[(String, [(String, [(String, [String])])])]
