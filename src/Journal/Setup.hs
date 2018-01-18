{-# LANGUAGE OverloadedStrings #-}
module Journal.Setup where

import           System.Directory (createDirectoryIfMissing)


buildSkeleton :: IO()
buildSkeleton = do
  let dirs = ["_layout", "_posts", "_site"]
  mapM_ (\d -> createDirectoryIfMissing False d) dirs
