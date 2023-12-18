{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment (getArgs)

import Parsers (listNotFounds)
import TeX 
import Tlmgr

main :: IO ()
main = getArgs >>= \case
  [engine,texf] -> strelitzia engine texf
  [texf]        -> strelitzia "pdflatex" texf
  _             -> printUsage

strelitzia :: FilePath -> FilePath -> IO ()
strelitzia engine texf =
  makeTeX engine texf >>= \proc_out ->
    tlmgrInstall =<< tlmgrSearch (listNotFounds proc_out)

printUsage :: IO ()
printUsage =
  putStrLn $ "\n USAGE: $ strelitzia [ENGINE] TEXFILE"
             ++ "\n The default ENGINE is pdflatex\n"
