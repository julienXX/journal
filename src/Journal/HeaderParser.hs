module Journal.HeaderParser where

import           Data.Functor.Identity as I
import           Data.Text             (Text, pack)
import           Text.Parsec

data Meta = MkMeta { title :: Text, date :: Text, description :: Text }
  deriving (Show)

parseMeta :: Text -> Either Text Meta
parseMeta post =
  case parse metaFromPost "" post
  of
    Left err     -> Left . pack $ show err
    Right result -> Right result

metaFromPost :: ParsecT Text u I.Identity Meta
metaFromPost = do
  _ <- startSeparator
  mTitle <- titleMeta
  mDate <- dateMeta
  mDesc <- descriptionMeta
  _ <- endSeparator
  return MkMeta { title = pack mTitle,
                  date = pack mDate,
                  description = pack mDesc }

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
