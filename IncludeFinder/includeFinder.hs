import ProgressParser
import Text.Parsec
import System.Environment
import System.IO
import Data.Tree

readFiles                :: [String] -> Either (String, [[String]])
readFiles []             = [[]]
readFiles (lastFile:[])  = case parse current includes of  
readFiles (current:rest) = do
    withFile current ReadMode (\handle -> do
        contents <- hGetContents handle
        case parse includes contents of  
            Left error -> Left error
            Right result -> (current, readFiles rest)

main = do
    args <- getArgs
    let askUser = (>=) (length args) 1
    let filename = ""

    if askUser = true then
    do
       println "What's the filename, boss?" 
       filename <- getArgs
       println "Thanks, champ."
    else
    do
       let filename = (args !! 1)

    includeTree <- readFiles filename
    case includeTree of 
        Left error -> print error
        Right result -> drawTree result

