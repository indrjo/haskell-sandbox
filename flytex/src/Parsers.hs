
module Parsers ( listPackageNames
               , listNotFounds
               ) where

import Text.Parsec
import Text.Parsec.String

-- DESCRIPTION
--
-- This module defines some functions that take a string as argument (the
-- contents of the standard output or some log) and to extract some names.
--

-- From the output of `tlmgr search --global --file PATTERN` list the names
-- the names of the packages that occur. 
listPackageNames :: String -> [String]
listPackageNames = either (\_ -> []) id . parse packageNamesParser ""

-- From "\nNAME:\n" extract "NAME".
packageNameParser :: GenParser Char s String
packageNameParser =
  between newline
          (char ':' >> newline)
          (many1 $ noneOf " :")

-- The parser of the output of `tlmgr search --global --file PATTERN`.
packageNamesParser :: GenParser Char s [String]
packageNamesParser =
      try (packageNameParser >>=
            \name -> (name :) <$> packageNamesParser)
  <|> try (anyChar >> packageNamesParser)
  <|> return []

-- Capture the names the missing files from the log of a TeX command. More
-- precisely, look for pieces of the form 
--
--   "FILE" not found
-- 
-- and isolate FILE.
listNotFounds :: String -> [String]
listNotFounds = either (\_ -> []) id . parse notFoundsParser ""

quoteMarks :: String
quoteMarks = "'\"`"

-- Extract the fragment between two quote marks.
quotedNameParser :: GenParser Char s String
quotedNameParser =
  between (oneOf quoteMarks)
          (oneOf quoteMarks)
          (many1 $ noneOf quoteMarks)

-- From a string complaining of something not found give its name.
notFoundParser :: GenParser Char s String
notFoundParser = quotedNameParser <* (spaces >> string "not found")

-- The parser that isolates the missing files from the log.
notFoundsParser :: GenParser Char s [String]
notFoundsParser =
      try (notFoundParser >>=
            \name -> (name :) <$> notFoundsParser)
  <|> try (anyChar >> notFoundsParser)
  <|> return []

