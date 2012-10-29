module IncludeTree (  IncludeTree(..)
                    , addSpaces
                    , printTree) where

import Text.Parsec

data IncludeTree a = NoIncludes a | 
                     IncludeFile a [IncludeTree a] 

addSpaces     :: [Char] -> Int -> [Char]
addSpaces spaces 0 = spaces
addSpaces spaces indent = addSpaces (' ':spaces) (indent - 1)

--childTrees                              :: Int -> IncludeTree [Either ParseError String] -> [Char]
childTrees indent (NoIncludes filename) = "\n" ++ (addSpaces "" indent) ++ (show filename)
childTrees indent (IncludeFile filename includes) = "\n" ++ (addSpaces "" indent) 
    ++ (show filename)
    ++ (concat $ map (childTrees (indent + 4)) includes)

printTree (NoIncludes filename) = (show filename)
printTree (IncludeFile filename includes) = (show filename)
    ++ (concat $ map (childTrees 4) includes )
