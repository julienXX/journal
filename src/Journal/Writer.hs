{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module Journal.Writer where

import           Data.Aeson                    (ToJSON, object, toJSON)
import           Data.String.Conversions       (cs)
import           Data.Text                     as T
import           Data.Text.Lazy                as LT hiding (pack, unpack)
import           System.FilePath.Posix         (dropExtension)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.EDE                      (eitherParseFile, eitherRender,
                                                fromPairs, (.=))
import           Text.Markdown                 (def, markdown, msXssProtect)

import           Journal.Post
import           Journal.PostParser

type Title = T.Text
type Date = T.Text
type Description = T.Text
type Tag = T.Text
type Tags = [Tag]
type Body = LT.Text

data IndexEntry = MkIndexEntry { eTitle :: Title, eDate :: Date, eDescription :: Description }
  deriving (Show)

instance ToJSON IndexEntry where
  toJSON i = object [
    "title"       .= eTitle i,
    "date"        .= eDate i,
    "description" .= eDescription i ]

buildIndex :: [FilePath] -> IO ()
buildIndex paths = do
  parsedPosts <- mapM postFromFile paths
  entries <- mapM makeIndexEntry parsedPosts
  generatedIndex <- mkIndex entries
  writeFile indexPath (cs generatedIndex)

postFromFile :: FilePath -> IO ParsedPost
postFromFile file = do
  source <- readFile $ "_posts/" ++ file
  pure $ parsePost $ cs source

makeIndexEntry :: ParsedPost -> IO IndexEntry
makeIndexEntry post = do
  let postTitle = getTitle post
  let postDate = getDate post
  let postDescription = getDescription post
  pure MkIndexEntry { eTitle = postTitle, eDate = postDate, eDescription = postDescription }

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

indexPath :: FilePath
indexPath = "_site/index.html"

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

mkIndex :: [IndexEntry] -> IO LT.Text
mkIndex entries = do
    r <- eitherParseFile "_layout/index.ede"
    either error return $ r >>= (`eitherRender` values)
  where
    values = fromPairs [ "posts" .= entries ]
