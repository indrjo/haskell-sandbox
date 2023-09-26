
{- cabal:
    build-depends: base
-}

module Terms where

import Data.List ((\\), union)

data Term = Var String
          | Const String
          | Lambda String Term
          | Apply Term Term

{- EXAMPLES
1. Variable written as "x": Var "x"
2. The constant "a": Const "a"
3. Lambdas with more variables:
    Lambda "x" (Lambda "y" ...BODY...)
4. Appying one term other ones:
    Apply (Apply (Const "f") (Const "x")) (Const "y")
-}

{- DOUBTS
1. How do I formalize the identity function?
    `Lambda "x" (Var "x")` or `Lambda "x" (Const "x")`?
   In general, the occurrence of the variable of the lambda how should be
   represented within the body? As a Var or as a Const?
-}

-- There is no practical use of that instance actually...
instance Show Term where
  -- Constants and variables are printed in the ame manner: they loose the
  -- the labels Var and Const. How to invert the process of printing?
  show (Var v) = v
  show (Const c) = c
  -- The greek letter lambda is the char '\x03BB'.
  show (Lambda x body) = "(\x03BB " ++ x ++ " " ++ show body ++ ")"
  -- The application is putting two terms between brackets.
  show (Apply t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

instance Read Term where
  -- Better than this instance is Parsec. I've written a module for that
  -- purpouse. I shall implement nice Errors for parsing too.
  readsPrec _ expr = [(undefined expr, "")]

-- List all free variables. In this context free variables are those do not
-- occur under the Lambda constructor. 
freeVars :: Term -> [String]
freeVars (Var v) = [v]
freeVars (Const _) = []
freeVars (Lambda x t) = freeVars t \\ [x]
freeVars (Apply t1 t2) = freeVars t1 `union` freeVars t2

-- List all the variables in Term.
allVars :: Term -> [String]
allVars (Var v) = [v]
allVars (Const _) = []
allVars (Lambda _ t) = allVars t
allVars (Apply t1 t2) = allVars t1 `union` allVars t2

