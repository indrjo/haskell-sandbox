{- cabal:
    build-depends: base, extra, parsec
-}

module CSVParser where

import Data.List.Extra
import Text.Parsec
import Text.Parsec.String

cell :: GenParser Char s String
cell = try quotedCell <|> simpleCell
  where
    simpleCell :: GenParser Char s String
    simpleCell = trim <$> many1 (noneOf ",\n")
    quotedCell :: GenParser Char s String
    quotedCell = do
      spaces
      between (char '"') (char '"') (many1 $ noneOf "\"\n")

cellsInARow :: GenParser Char s [String]
cellsInARow = cell `sepEndBy` char ','

cellsInAFile :: GenParser Char s [[String]]
cellsInAFile = cellsInARow `sepEndBy` char '\n'

fileToCSV = parseFromFile cellsInAFile

