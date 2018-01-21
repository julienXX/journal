{-# LANGUAGE OverloadedStrings #-}
module Journal.WriterSpec where

import           Data.Monoid    ((<>))
import           Journal.Writer
import           Test.Hspec

spec :: Spec
spec = describe "Writer" $

  it "converts a .md file path to an .html one" $
    destPath "foo.md" `shouldBe` "_site/foo.html"
