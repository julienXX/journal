{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.String.Conversions       (cs)
import           Data.Text                     (Text (..))
import           System.Directory              (listDirectory)
import           System.FilePath.Posix
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Markdown                 (def, markdown, msXssProtect)

import           Reader                        (readPosts)
import           Writer                        (convertMdToHtml)

main :: IO ()
main = do
  posts <- readPosts
  mapM_ (\f -> convertMdToHtml f) posts
  putStrLn "Finished."
