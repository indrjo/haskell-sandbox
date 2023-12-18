
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
listPackageNames = listCaptures packageNameParser

-- From "\nNAME:\n" extract "NAME".
packageNameParser ::  Parser String
packageNameParser =
  between newline
          (char ':' >> newline)
          (many1 $ noneOf " :")

-- Capture the names the missing files from the log of a TeX command. More
-- precisely, look for pieces of the form 
--
--   "FILE" not found
-- 
-- and isolate FILE.
listNotFounds :: String -> [String]
listNotFounds = listCaptures notFoundParser

quoteMarks :: String
quoteMarks = "'\"`"

-- Extract the fragment between two quote marks.
quotedNameParser :: Parser String
quotedNameParser =
  between (oneOf quoteMarks)
          (oneOf quoteMarks)
          (many1 $ noneOf quoteMarks)

-- From a string complaining of something not found give its name.
notFoundParser :: Parser String
notFoundParser = quotedNameParser <* (spaces >> string "not found")


-- Some helper functions used in this module.

-- While consuming the input string, try a given parser: if it succeeds,
-- collect the match into a list, otherwise move on by one character.
listCapturesParser :: Parser String -> Parser [String]
listCapturesParser p =
      try (p >>=
            \str -> (str :) <$> listCapturesParser p)
  <|> try (anyChar >> listCapturesParser p)
  <|> return []

-- Since `parse (listCapturesParser p) ""` never gives a `Left ParseError`,
-- it safe to expect always the `Right` value.
listCaptures :: Parser String -> String -> [String]
listCaptures p =
  either (\_ -> []) id . parse (listCapturesParser p) ""
