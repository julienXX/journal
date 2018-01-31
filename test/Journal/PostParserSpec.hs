{-# LANGUAGE OverloadedStrings #-}
module Journal.PostParserSpec where

import           Data.Text                       (pack)
import           Text.Parsec

import           Journal.PostParser
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib


spec :: Spec
spec = describe "PostParser" $ do

  context "startSeparator" $ do
    it "parses" $ do
      let testString = pack "---\n"
      let parsed = parse startSeparator "" testString
      parsed `shouldSatisfy` isRight

    it "fails" $ do
      let testString = pack "---"
      let parsed = parse startSeparator "" testString
      parsed `shouldSatisfy` isLeft

  context "endSeparator" $ do
    it "parses" $ do
      let testString = pack "---\n"
      let parsed = parse endSeparator "" testString
      parsed `shouldSatisfy` isRight

    it "parses" $ do
      let testString = pack "\n---\n"
      let parsed = parse endSeparator "" testString
      parsed `shouldSatisfy` isRight

    it "fails" $ do
      let testString = pack "---"
      let parsed = parse endSeparator "" testString
      parsed `shouldSatisfy` isLeft

  context "titleMeta" $ do
    it "parses" $ do
      let testString = pack "title: title\n"
      let parsed = parse titleMeta "" testString
      parsed `shouldBe` Right "title"

    it "parses" $ do
      let testString = pack "\ntitle: title\n"
      let parsed = parse titleMeta "" testString
      parsed `shouldBe` Right "title"

    it "fails" $ do
      let testString = pack "title: title"
      let parsed = parse titleMeta "" testString
      parsed `shouldSatisfy` isLeft

  context "dateMeta" $ do
    it "parses" $ do
      let testString = pack "date: 1234\n"
      let parsed = parse dateMeta "" testString
      parsed `shouldBe` Right "1234"

    it "parses" $ do
      let testString = pack "\ndate: some text date\n"
      let parsed = parse dateMeta "" testString
      parsed `shouldBe` Right "some text date"

    it "fails" $ do
      let testString = pack "date: date with no endline"
      let parsed = parse dateMeta "" testString
      parsed `shouldSatisfy` isLeft

  context "descriptionMeta" $ do
    it "parses" $ do
      let testString = pack "description: description\n"
      let parsed = parse descriptionMeta "" testString
      parsed `shouldBe` Right "description"

    it "parses" $ do
      let testString = pack "\ndescription: description\n"
      let parsed = parse descriptionMeta "" testString
      parsed `shouldBe` Right "description"

    it "fails" $ do
      let testString = pack "description: description with no endline"
      let parsed = parse descriptionMeta "" testString
      parsed `shouldSatisfy` isLeft

  context "tagsMeta" $ do
    it "parses" $ do
      let testString = pack "tags: [tag1, tag2]\n"
      let parsed = parse tagsMeta "" testString
      parsed `shouldBe` Right "[tag1, tag2]"

    it "parses" $ do
      let testString = pack "\ntags: [tag1, tag2]\n"
      let parsed = parse tagsMeta "" testString
      parsed `shouldBe` Right "[tag1, tag2]"

    it "fails" $ do
      let testString = pack "tags: [tag1, tag2]"
      let parsed = parse tagsMeta "" testString
      parsed `shouldSatisfy` isLeft

  context "bodyContent" $ do
    it "parses" $ do
      let testString = pack "anything"
      let parsed = parse bodyContent "" testString
      parsed `shouldBe` Right "anything"

    it "parses" $ do
      let testString = pack ""
      let parsed = parse bodyContent "" testString
      parsed `shouldBe` Right ""

  context "parsePost" $ do
    it "parses" $ do
      let testString = pack "---\ntitle: title\ndate: date\ndescription: description\ntags: tags\n---\nbody"
      let parsed = parsePost testString
      parsed `shouldSatisfy` isRight

    it "fails (title is missing)" $ do
      let testString = pack "---\ndate: date\ndescription: description\ntags: tags\n---\nbody"
      let parsed = parsePost testString
      parsed `shouldSatisfy` isLeft
