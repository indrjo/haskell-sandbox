
module TeXLogParser (listMissings) where

import Text.Parsec
import Text.Parsec.String

-- Read a TeX log to find all the files that are required but not present
-- in your TeX Live. The "TeX log" here is a unique string encompassing all
-- the content of an actual TeX log file.
listMissings :: String -> [String]
listMissings = either (\_ -> []) id . parse notFounds ""

quoteMarks :: String
quoteMarks = "`\'\""

quotedName :: GenParser Char s String
quotedName = filter (/= '\n') <$> many (noneOf quoteMarks)

notFound :: GenParser Char s String
notFound =
  between (oneOf quoteMarks) (oneOf quoteMarks) quotedName
    <* (spaces >> string "not found")

notFounds :: GenParser Char s [String]
notFounds = (eof >> pure [])
        <|> try (notFound >>=
                  \name -> (name :) <$> notFounds)
        <|> (anyChar >> notFounds)
