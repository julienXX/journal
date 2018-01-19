{-# LANGUAGE OverloadedStrings #-}
module Journal.Setup where

import           System.Directory (copyFile, createDirectoryIfMissing)


buildSkeleton :: IO()
buildSkeleton = do
  putStrLn "Building skeleton..."
  let dirs = ["_layout", "_posts", "_site/assets/css"]
  mapM_ (\d -> createDirectoryIfMissing True d) dirs
  copyFile "assets/style.css" "_site/assets/css/style.css"
