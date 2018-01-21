{-# LANGUAGE OverloadedStrings #-}
module Journal.Writer where

import           Data.String.Conversions       (cs)
import           Data.Text                     as T
import           Data.Text.Lazy                as LT
import           System.FilePath.Posix         (dropExtension)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.EDE                      (eitherParseFile, eitherRender,
                                                fromPairs, (.=))
import           Text.Markdown                 (def, markdown, msXssProtect)

import           Journal.PostParser

convertMdToHtml :: FilePath -> IO()
convertMdToHtml file = do
  source <- readFile $ "_posts/" ++ file
  let post = parsePost $ cs source
  let postTitle = getTitle post
  let postBody = getBody post
  let renderedMarkdown = renderHtml $ markdown def { msXssProtect = True } $ cs postBody
  let dest = destPath file
  generatedHtml <- mkHtml postTitle renderedMarkdown
  writeFile dest (cs generatedHtml)

destPath :: FilePath -> FilePath
destPath f = "_site/" ++ dropExtension f ++ ".html"

mkHtml :: T.Text -> LT.Text -> IO LT.Text
mkHtml pTitle pBody = do
    r <- eitherParseFile "_layout/default.ede"
    either error return $ r >>= (`eitherRender` values)
  where
    values = fromPairs
        [ "title" .= pTitle
        , "body"  .= pBody
        ]

getTitle :: Either T.Text Post -> T.Text
getTitle (Left _)  = ""
getTitle (Right p) = cs $ title p

getBody :: Either T.Text Post -> T.Text
getBody (Left _)  = ""
getBody (Right p) = cs $ body p
