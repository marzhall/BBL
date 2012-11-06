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

--childTrees                              :: Int -> IncludeTree [Either ParseError String] -> [Char]
childTrees indent (NoIncludes filename) = "\n" ++ addSpaces "" indent ++ stripEndlines (show filename)
childTrees indent (IncludeFile filename includes) = "\n" ++ addSpaces "" indent
    ++ (stripEndlines $ show filename)
    ++ (concatMap (childTrees (indent + 4)) includes)

printTree (NoIncludes filename) = (show filename)
printTree (IncludeFile filename includes) = stripEndlines (show filename)
    ++ concatMap (childTrees 4) includes


--makeMap :: IncludeTree (Either ParseError String) -> Map String [String]
--makeMap (NoIncludes filename) = case filename of
    --Right filename -> fromList [(filename, [])]
    --Left filename -> fromList [(show filename, [])]
--makeMap (IncludeFile filename includes) = case filename of
    --Right filename -> fromListWith (++) [(filename, concatMap makeMapHelper includes)]
    --Left filename -> fromListWith (++) [(show filename, [])]
--
--makeMapHelper :: IncludeTree (Either ParseError String) -> [(String, [String])]
--makeMapHelper (NoIncludes filename) childFile = case filename of
    --Right filename -> [(filename, [childFile])]
    --Left filename -> [(show filename, [childFile])]
--makeMapHelper (IncludeFile filename includes) childFile = case filename of
    --Right filename -> [(childFile, concatMap makeMapHelper includes filename)]
    --Left filename -> [(show filename, childFile)]
