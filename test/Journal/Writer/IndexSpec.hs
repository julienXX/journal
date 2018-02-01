{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Journal.Writer.IndexSpec where

import           Journal.Post
import           Journal.Writer.Index
import           Test.Hspec

spec :: Spec
spec = describe "IndexWriter" $

  it "builds Index entries from ParsedPost" $ do
    let parsedPost = Right MkPost { title = "title", date = "date", description = "description"
                                  , body = "body", tags = "tags" }
    let indexEntry = MkIndexEntry { title = "title", date = "date", description = "description" }
    makeIndexEntry parsedPost `shouldBe` indexEntry
