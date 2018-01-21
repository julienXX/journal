{-# LANGUAGE OverloadedStrings #-}
module Journal.Reader where

import           System.Directory (listDirectory)

readPosts :: IO [FilePath]
readPosts = do
  putStrLn "Reading Markdown file from _posts..."
  listDirectory "_posts"
