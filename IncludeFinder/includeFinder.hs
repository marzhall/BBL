import ProgressParser
import IncludeTree
import Text.Parsec
import System.Environment
import Control.Monad
import System.IO

parseFile          :: String -> IO (Either ParseError [String])
parseFile fileName = readFile fileName >>= (\contents -> return $ parse includes contents contents)

includeTree          :: String -> IO (IncludeTree (Either ParseError String))
includeTree fileName = do
    parsedFile <- parseFile fileName
    case parsedFile of
        Left err -> return $ NoIncludes $ Left err
        Right results -> if results /= [] 
                            then do
                                parsedResults <- mapM includeTree results
                                return $ IncludeFile (Right fileName) parsedResults
                            else
                                return $ NoIncludes (Right fileName)

main = do
    args <- getArgs 
    let askUser = (>=) 1 (length args)
    filename <- if askUser 
        then do
                print "What's the filename, boss?" 
                getLine
        else return (args !! 1)

    includeTree <- includeTree filename
    writeFile (filename ++ ".includes") (printTree includeTree)
