module Journal.Post where

import           Data.String.Conversions (cs)
import           Data.Text               (Text, pack, splitOn)

data Post = MkPost { title :: Text, date :: Text, description :: Text, tags :: Text, body :: Text }
  deriving (Show)

type ParsedPost = Either Text Post

getTitle :: ParsedPost -> Text
getTitle (Left _)  = pack ""
getTitle (Right p) = cs $ title p

getDate :: ParsedPost -> Text
getDate (Left _)  = pack ""
getDate (Right p) = cs $ date p

getDescription :: ParsedPost -> Text
getDescription (Left _)  = pack ""
getDescription (Right p) = cs $ description p

getBody :: ParsedPost -> Text
getBody (Left _)  = pack ""
getBody (Right p) = cs $ body p

getTags :: ParsedPost -> [Text]
getTags (Left _)  = [pack ""]
getTags (Right p) = do
  let comma = pack ","
  splitOn comma (tags p)
