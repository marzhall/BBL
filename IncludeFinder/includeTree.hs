module IncludeTree (  IncludeTree(..)
                    , addSpaces
                    , printTree) where

import Text.Parsec
import Data.Map hiding (map, filter)

data IncludeTree a = NoIncludes a |
                     IncludeFile a [IncludeTree a]

stripEndlines = filter (/= '\n')

addSpaces     :: String -> Int -> String
addSpaces spaces 0 = spaces
addSpaces spaces indent = addSpaces (' ':spaces) (indent - 1)

childTrees                              :: Int -> IncludeTree (Either ParseError String) -> [Char]
childTrees indent (NoIncludes filename) = "\n" ++ addSpaces "" indent ++ stripEndlines (show filename)
childTrees indent (IncludeFile filename includes) = "\n" ++ addSpaces "" indent
    ++ (stripEndlines $ show filename)
    ++ (concatMap (childTrees (indent + 4)) includes)

printTree (NoIncludes filename) = (show filename)
printTree (IncludeFile filename includes) = stripEndlines (show filename)
    ++ concatMap (childTrees 4) includes
