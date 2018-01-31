module Journal.PostParser where

import           Data.Functor.Identity as I
import           Data.Text             (Text, pack)
import           Text.Parsec

import           Journal.Post

parsePost :: Text -> ParsedPost
parsePost post =
  case parse postParser "" post
  of
    Left err     -> Left . pack $ show err
    Right result -> Right result

postParser :: ParsecT Text u I.Identity Post
postParser = do
  _ <- startSeparator
  mTitle <- titleMeta
  mDate <- dateMeta
  mDesc <- descriptionMeta
  mTags <- tagsMeta
  _ <- endSeparator
  mBody <- bodyContent
  return MkPost { title = pack mTitle,
                  date = pack mDate,
                  description = pack mDesc,
                  tags = pack mTags,
                  body = pack mBody}

startSeparator :: ParsecT Text u Identity Char
startSeparator = string "---" >> endOfLine

endSeparator :: ParsecT Text u Identity Char
endSeparator = optionMaybe newline >> string "---" >> endOfLine

titleMeta :: ParsecT Text u Identity String
titleMeta = optionMaybe newline >> string "title: " >> spaces >> manyTill anyChar (try (string "\n"))

dateMeta :: ParsecT Text u Identity String
dateMeta = optionMaybe newline >> string "date: " >> spaces >> manyTill anyChar (try (string "\n"))

descriptionMeta :: ParsecT Text u Identity String
descriptionMeta = optionMaybe newline >> string "description: " >> spaces >> manyTill anyChar (try (string "\n"))

tagsMeta :: ParsecT Text u Identity String
tagsMeta = optionMaybe newline >> string "tags: " >> spaces >> manyTill anyChar (try (string "\n"))

bodyContent :: ParsecT Text u Identity String
bodyContent = optionMaybe newline >> manyTill anyChar (try eof)
