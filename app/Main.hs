{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Journal.Reader (readPosts)
import           Journal.Setup  (buildSkeleton)
import           Journal.Writer (convertMdToHtml)

main :: IO ()
main = do
  buildSkeleton
  posts <- readPosts
  mapM_ (\f -> convertMdToHtml f) posts
  putStrLn "Finished."
