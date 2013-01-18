module Options where

import System.Console.GetOpt
import Data.Maybe (fromMaybe)

data Flag = IncludeFiles [String] | SourceDirectories [String]
   deriving Show

options :: [OptDescr Flag]
options = [ Option ['f'] ["files"] (ReqArg IncludeFiles) ".i files to dependency-check"
          , Option ['d'] ["directories"] (ReqArg SourceDirectories) "Directories containing source to search"]
          

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."
