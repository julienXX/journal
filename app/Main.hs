{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Journal.IndexWriter (buildIndex)
import           Journal.PostWriter  (convertMdToHtml)
import           Journal.Reader      (readPosts)
import           Journal.Setup       (buildSkeleton)

main :: IO ()
main = do
  buildSkeleton
  posts <- readPosts
  buildIndex posts
  mapM_ convertMdToHtml posts
  putStrLn "Finished."
