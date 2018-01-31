{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Journal.Reader (readPosts)
import           Journal.Setup  (buildSkeleton)
import           Journal.Writer (buildIndex, convertMdToHtml)

main :: IO ()
main = do
  buildSkeleton
  posts <- readPosts
  buildIndex posts
  mapM_ convertMdToHtml posts
  putStrLn "Finished."
