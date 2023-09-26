
module Main where

import System.Environment (getArgs)
import TeXLogParser

{-

This program is a tiny CLI: if some parameters are given, only the first is 
taken into account and it should be a TeX log file; otherwise, something
from standard input is expected. Thus the program can be used in two ways:

  $ tex-list-missings some-tex.log

or

  $ some-command | tex-list-missings

-}

main :: IO ()
main = do
  args <- getArgs
  texlog <- case args of
    fname:_ -> readFile fname
    []      -> getContents
  mapM_ putStrLn (listMissings texlog)

