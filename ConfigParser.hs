{- cabal:
  build-depends: base, parsec
-}

{-
This is a small module intended to help parsing simple configuration files,
as the following:

 # This is a comment. Comments are skipped by the parser.  Blank lines are
 # not taken into account as well.

 hello = world # a "key-val" configuration line
 hallo-welt    # a "unary" options

-}

module ConfigParser (ConfigOpt(..), configs) where

import Text.Parsec
import Text.Parsec.String

data ConfigOpt = UOpt String
               | KeyVal String String
  deriving (Show)

beforeSep :: GenParser Char s String
beforeSep = many1 (noneOf " =#\n") 
        <?> "a uopt or a key expected"

sep :: GenParser Char s ()
sep = spaces >> char '=' >> spaces

afterSep :: GenParser Char s String
afterSep = many1 (noneOf " #\n")
       <?> "a value expected"

config :: GenParser Char s ConfigOpt
config = beforeSep >>= \a ->
  try (KeyVal a <$> (sep >> afterSep)) <|> return (UOpt a)

configs :: GenParser Char s [ConfigOpt]
configs = (eof >> return [])
      <|> (space >> configs)
      <|> (char '#' >> many (noneOf "\n") >> configs)
      <|> (config >>= \cfg -> (cfg :) <$> configs)

