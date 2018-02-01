{-# LANGUAGE OverloadedStrings #-}
module Journal.Writer.Index (buildIndex) where

import           Data.Aeson              (ToJSON, object, toJSON)
import           Data.String.Conversions (cs)
import           Data.Text.Lazy          as LT hiding (map, pack, unpack)
import           Journal.Writer.Base     (Date, Description, Title,
                                          postFromFile)
import           Text.EDE                (eitherParseFile, eitherRender,
                                          fromPairs, (.=))

import           Journal.Post

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
  let entries = map makeIndexEntry parsedPosts
  generatedIndex <- mkIndex entries
  writeFile indexPath (cs generatedIndex)

makeIndexEntry :: ParsedPost -> IndexEntry
makeIndexEntry post = do
  let postTitle = getTitle post
  let postDate = getDate post
  let postDescription = getDescription post
  MkIndexEntry { eTitle = postTitle, eDate = postDate, eDescription = postDescription }

indexPath :: FilePath
indexPath = "_site/index.html"

mkIndex :: [IndexEntry] -> IO LT.Text
mkIndex entries = do
    r <- eitherParseFile "_layout/index.ede"
    either error return $ r >>= (`eitherRender` values)
  where
    values = fromPairs [ "posts" .= entries ]
