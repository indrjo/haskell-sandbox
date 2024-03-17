{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.FilePath (replaceExtension)
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
strelitzia engine texf = do
  makeTeX engine texf 50 -- to be improved!
  notFounds <- listNotFounds <$> readFile (replaceExtension texf "log")
  if null notFounds
    then putStrLn "no missing file! good..."
    else do
      connected <- contactPackageRepo
      if connected
        then do
          packages <- tlmgrSearch notFounds
          putStrLn $ "packages to be installed: " ++ unwords packages
          tlmgrInstall packages
        else
          putStrLn "cannot reach the package repository!"

printUsage :: IO ()
printUsage =
  putStrLn $ "\n USAGE: $ strelitzia [ENGINE] TEXFILE"
             ++ "\n The default ENGINE is pdflatex\n"
