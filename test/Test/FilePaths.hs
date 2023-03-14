{-# HLINT ignore "Use camelCase" #-}

module Test.FilePaths where

import Data.List (isSuffixOf)
import System.Directory (getDirectoryContents)
import Utility

dir_examples :: FilePath
dir_examples = "examples/"

dir_examples_parsing :: FilePath
dir_examples_parsing = dir_examples <> "parsing/"

dir_examples_typing :: FilePath
dir_examples_typing = dir_examples <> "typing/"

getDirectoryFilesBySuffix :: FilePath -> String -> IO [FilePath]
getDirectoryFilesBySuffix dir suf = do
  filter (suf `isSuffixOf`)
    <$> ( (dir <>) <$$> getDirectoryContents dir
        )
