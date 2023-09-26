
{- cabal:
    build-depends: base, parsec
    other-modules: Terms
-}

module SExprParser where

import Text.ParserCombinators.Parsec
import Terms (Term(..))


-- INTRODUCING S-EXPRESSIONS **********************************************

{-
  DEFINITION. Given a collection of "atoms" A, we define S-EXPRESSIONS over
  A inductively as follows:

    1. Atoms are themselves S-expressions.
    2. If f1, ..., fn are S-expressions, then so is (f1 ... f2)
    3. Nothing else is a S-expressions.

  In Haskell, we could write something like:

  > data SExpr a = Atom a | Nest [SExpr a]

  but, for our purpouses, the atoms we want are exactly strings.
-}

data SExpr = Atom String | Nest [SExpr]
  deriving (Show)


-- WRINTING A PARSER FOR S-EXPRESSIONS ************************************

-- The parser.
fromStringToSExpr :: String -> Either ParseError SExpr
fromStringToSExpr = parse sexpr ""

{-
  How do we parse a S-expression?

  * First of all, see if it is something like (f1 ... fn).
  * If that is the case, open inspect the stuff inside the brackets. That
    is, for i in {1, ..., n} see what is fi.
  * If f is an atom, that is no brackets, then it is an atom.
-}

atom, nest, sexpr :: GenParser Char st SExpr
atom = Atom <$> many1 (noneOf " ()\t\n\r")
nest = Nest <$> between (char '(') (char ')') (many sexpr)
sexpr = between spaces spaces (nest <|> atom)


-- CONVERT S-EXPRESSIONS INTO OUR TERMS ***********************************

-- The name explains it all, doesn't it?
fromSExprToTerm :: SExpr -> Term
-- There is a source of indecison here: how should an Atom be converted?
fromSExprToTerm (Atom a)  = Const a
fromSExprToTerm (Nest (Atom "lambda":Atom var:[rst])) =
  Lambda var $ fromSExprToTerm rst
fromSExprToTerm (Nest ns) =
  foldl1 Apply $ map fromSExprToTerm ns

