{-# LANGUAGE OverloadedStrings #-}
module Journal.Writer.Post where

import           Data.String.Conversions       (cs)
import           Data.Text.Lazy                as LT hiding (pack, unpack)
import           System.FilePath.Posix         (dropExtension)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.EDE                      (eitherParseFile, eitherRender,
                                                fromPairs, (.=))
import           Text.Markdown                 (def, markdown, msXssProtect)

import           Journal.Post
import           Journal.Writer.Base           (Body, Date, Tags, Title,
                                                postFromFile)

convertMdToHtml :: FilePath -> IO()
convertMdToHtml file = do
  post <- postFromFile file
  let postTitle = getTitle post
  let postBody = getBody post
  let postDate = getDate post
  let postTags = getTags post
  let renderedMarkdown = renderHtml $ markdown def { msXssProtect = True } $ cs postBody
  let dest = destPath file
  generatedHtml <- mkHtml postTitle postDate postTags renderedMarkdown
  writeFile dest (cs generatedHtml)

destPath :: FilePath -> FilePath
destPath f = "_site/" ++ dropExtension f ++ ".html"

mkHtml :: Title -> Date -> Tags -> Body -> IO LT.Text
mkHtml pTitle pDate pTags pBody = do
    r <- eitherParseFile "_layout/default.ede"
    either error return $ r >>= (`eitherRender` values)
  where
    values = fromPairs
        [ "title" .= pTitle
        , "date"  .= pDate
        , "tags"  .= pTags
        , "body"  .= pBody
        ]
