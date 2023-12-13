{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment (getArgs)

import Parsers (listNotFounds)
import TeX 
import Tlmgr

main :: IO ()
main = getArgs >>= \case
  [engine,texf] -> flytex engine texf
  [texf]        -> flytex "pdflatex" texf
  _             -> printUsage

flytex :: FilePath -> FilePath -> IO ()
flytex engine texf =
  makeTeX engine texf >>= \proc_out ->
    tlmgrInstall =<< tlmgrSearch (listNotFounds proc_out)

printUsage :: IO ()
printUsage =
  putStrLn $ "\n USAGE: $ flytex [ENGINE] TEXFILE"
             ++ "\n The default ENGINE is pdflatex\n"
