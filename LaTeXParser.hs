{- cabal:
    build-depends: base, extra, parsec
-}

module LaTeXParser where

import Data.List.Extra (trim)
import Text.Parsec
import Text.Parsec.String

data UsePackage = UsePackage [PackageOption] [String]
  deriving (Show)

data PackageOption = UOpt String | KeyVal String [String]
  deriving (Show)

-- Parse "\usepackage[...]{...}" statements.
usepackage :: GenParser Char st UsePackage
usepackage = do
  string "\\usepackage"
  spaces
  opts <- try options <|> return []
  spaces
  pkgs <- try packages <|> return []
  return $ UsePackage opts pkgs

-- Parse options to be passed to the package
options :: GenParser Char st [PackageOption]
options =
    between (char '[') (char ']') $
      (try keyval <|> uopt) `sepEndBy` char ','
  where
    uopt :: GenParser Char s PackageOption
    uopt = UOpt <$> (spaces >> aname)
    keyval :: GenParser Char s PackageOption
    keyval = do
      key <- spaces >> aname
      spaces >> char '=' >> spaces
      val <- try (listBetween '{' '}' ',') <|> (pure <$> aname)
      return $ KeyVal key val
    aname :: GenParser Char s String
    aname = many1 (noneOf "=,] ")

-- Parse package names
packages :: GenParser Char st [String]
packages = listBetween '{' '}' ','

-- Parse a list enclosed by two characters and separated by anothe one.
listBetween :: Char -> Char -> Char -> GenParser Char st [String]
listBetween open close sep =
  between (char open)
          (char close)
          ((trim <$> many (noneOf [close, sep]))
            `sepEndBy` char sep)

