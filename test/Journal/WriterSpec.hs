{-# LANGUAGE OverloadedStrings #-}
module Journal.WriterSpec where

import           Data.Monoid    ((<>))
import           Journal.Writer
import           Test.Hspec

spec :: Spec
spec = describe "Writer" $ do

  it "converts a .md file path to an .html one" $ do
    destPath "foo.md" `shouldBe` "_site/foo.html"
