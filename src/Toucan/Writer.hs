{-# LANGUAGE OverloadedStrings #-}
module Toucan.Writer where

import           Data.String.Conversions       (cs)
import           System.FilePath.Posix         (dropExtension)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Markdown                 (def, markdown, msXssProtect)


convertMdToHtml :: FilePath -> IO()
convertMdToHtml file = do
  source <- readFile $ "_posts/" ++ file
  let r = renderHtml $ markdown def { msXssProtect = True } $ cs source
  let dest = "_site/" ++ dropExtension file ++ ".html"
  putStrLn $ "Write file " ++ dest
  writeFile dest (cs r)
