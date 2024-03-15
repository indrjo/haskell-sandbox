
module Parsers ( listPackageNames
               , listNotFounds
               , repoUrl
               ) where

import Text.Parsec
import Text.Parsec.String

-- DESCRIPTION
--
-- This module defines some functions that take a string as argument (the
-- contents of the standard output or some log) and to extract some names
-- with aid of parsers.
--

-- ------------------------------------------------------------------------
-- Parse the output of `tlmgr search --global --file PATTERN`.
-- ------------------------------------------------------------------------

listPackageNames :: String -> [String]
listPackageNames = listCaptures packageNameParser

-- From "\nNAME:\n" extract "NAME".
packageNameParser ::  Parser String
packageNameParser =
  between newline
          (char ':' >> newline)
          (many1 $ noneOf ":")


-- ------------------------------------------------------------------------
-- Extract the names of the missing file from the log of the compilation.
-- ------------------------------------------------------------------------

-- So far the parser looks for pieces having the following pattern
--
--   <quote-mark>FILE<quote-mark> not found
--
-- and isolates just FILE. See `quoteMarks` for a list of quotation marks
-- used to wrap the name of the missing.
listNotFounds :: String -> [String]
listNotFounds = listCaptures notFoundParser

-- From a string complaining of something not found of the fashion shown
-- above isolate the name.
notFoundParser :: Parser String
notFoundParser =
  between (oneOf quoteMarks)
          (oneOf quoteMarks >> spaces >> string "not found")
          (many1 $ noneOf quoteMarks)

quoteMarks :: String
quoteMarks = "'\"`"


-- ------------------------------------------------------------------------
-- Parse `tlmgr option repository` output to extract the url.
-- ------------------------------------------------------------------------

repoUrl :: String -> Either ParseError String
repoUrl = parse repoUrlParser ""

repoUrlParser :: Parser String
repoUrlParser = do
  _ <- string "Default package repository (repository):"
  spaces
  many1 $ noneOf "\n"


-- ------------------------------------------------------------------------
-- Helper functions.
-- ------------------------------------------------------------------------

-- While consuming the input string, try a given parser: if it succeeds,
-- collect the match into a list, otherwise move on by one char and retry
-- until the input is wholly consumed.
listCapturesParser :: Parser String -> Parser [String]
listCapturesParser p =
      try (p >>= \str ->
                   (str :) <$> listCapturesParser p)
  <|> try (anyChar >> listCapturesParser p)
  <|> return []

-- Since `parse (listCapturesParser p) ""` never gives a `Left ParseError`,
-- it safe to expect always the `Right` value.
listCaptures :: Parser String -> String -> [String]
listCaptures p = either (\_ -> []) id . parse (listCapturesParser p) ""

