{-# LANGUAGE OverloadedStrings #-}
module Journal.Writer where

import           Data.String.Conversions       (cs)
import qualified Data.Text.Lazy                as LT
import           System.FilePath.Posix         (dropExtension)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.EDE
import           Text.Markdown                 (def, markdown, msXssProtect)

convertMdToHtml :: FilePath -> IO()
convertMdToHtml file = do
  source <- readFile $ "_posts/" ++ file
  let renderedMarkdown = renderHtml $ markdown def { msXssProtect = True } $ cs source
  let dest = destPath file
  generatedHtml <- mkHtml "TITLE" renderedMarkdown
  writeFile dest (cs generatedHtml)

destPath :: FilePath -> FilePath
destPath f = "_site/" ++ dropExtension f ++ ".html"

mkHtml :: LT.Text -> LT.Text -> IO(LT.Text)
mkHtml title body = do
    r <- eitherParseFile "_layout/default.ede"
    either error return $ r >>= (`eitherRender` env)
  where
    env = fromPairs
        [ "title" .= title
        , "body"  .= body
        ]
