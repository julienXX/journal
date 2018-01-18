{-# LANGUAGE OverloadedStrings #-}
module Journal.Reader where

import           System.Directory (listDirectory)

readPosts :: IO([FilePath])
readPosts = do
  putStrLn "Read Markdown"
  listDirectory "_posts"
