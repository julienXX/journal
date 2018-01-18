{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.String.Conversions       (cs)
import           Data.Text                     (Text (..))
import           System.Directory              (listDirectory)
import           System.Environment            (getArgs)
import           System.FilePath.Posix
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Markdown                 (def, markdown, msXssProtect)


main :: IO ()
main = do
  args <- getArgs
  putStrLn "Read Markdown"
  posts <- listDirectory "_posts"
  mapM_ (\f -> convertMdToHtml f) posts
  putStrLn "Finished."


convertMdToHtml :: FilePath -> IO()
convertMdToHtml file = do
  source <- readFile $ "_posts/" ++ file
  let r = renderHtml $ markdown def { msXssProtect = True } $ cs source
  let dest = "_site/" ++ dropExtension file ++ ".html"
  putStrLn $ "Write file " ++ dest
  writeFile dest (cs r)
