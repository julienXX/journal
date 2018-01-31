{-# LANGUAGE OverloadedStrings #-}
module Journal.PostSpec where

import           Journal.Post
import           Test.Hspec

spec :: Spec
spec = describe "Post" $ do

  let parsedPost = Right MkPost { title = "title", date = "date", description = "description", tags = "tag1, tag2", body = "body" }

  it "extracts title from a parsed post" $
    getTitle parsedPost `shouldBe` "title"

  it "extracts date from a parsed post" $
    getDate parsedPost `shouldBe` "date"

  it "extracts description from a parsed post" $
    getDescription parsedPost `shouldBe` "description"

  it "extracts body from a parsed post" $
    getBody parsedPost `shouldBe` "body"

  it "extracts tags from a parsed post" $
    getTags parsedPost `shouldBe` ["tag1","tag2"]
