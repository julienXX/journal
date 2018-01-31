{-# LANGUAGE OverloadedStrings #-}
module Journal.BaseWriter where

import           Data.String.Conversions (cs)
import           Data.Text               as T
import           Data.Text.Lazy          as LT hiding (pack, unpack)

import           Journal.Post
import           Journal.PostParser

type Title = T.Text
type Date = T.Text
type Description = T.Text
type Tag = T.Text
type Tags = [Tag]
type Body = LT.Text

postFromFile :: FilePath -> IO ParsedPost
postFromFile file = do
  source <- readFile $ "_posts/" ++ file
  pure $ parsePost $ cs source
