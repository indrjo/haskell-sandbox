{-# LANGUAGE LambdaCase #-}

module Main where

import System.FilePath (replaceFileName, takeFileName)
import System.Directory (renameFile)
import System.IO.Error (catchIOError)
import System.Environment (getArgs)
import System.Exit (die)

--
-- DESCRIPTION
--
-- In a TeX file, change math delimiters `$` to '\(' or '\)'.
--
-- USAGE
--
--  $ ./THIS-PROGRAM input.tex
--
-- or
--
--  $ ./THIS-PROGRAM < input.tex > output.tex
--

main :: IO ()
main = getArgs >>= \case
  fname:_ -> tryOrDie $ do
    makeFileOld fname
    content <- readFile (old fname)
    writeFile fname (changeMathDelims content)
  _ -> interact changeMathDelims
  
data MathDelim = Open 
               | Close
  deriving (Eq)

instance Show MathDelim where
  show Open  = "\\("
  show Close = "\\)"

-- Stolen from here: 
-- https://www.matematicamente.it/forum/viewtopic.php?f=15&t=222435
changeMathDelims :: String -> String
changeMathDelims = helper Open
  where
    helper :: MathDelim -> String -> String
    helper d ('\\':'$':str') = '\\':'$' : helper d str'
    helper d ('$':str') = 
      let d' = if d == Open then Close else Open in
        show d ++ helper d' str'
    helper d (s:str') = s : helper d str'
    helper _ [] = []

old :: FilePath -> FilePath
old fpath = replaceFileName fpath ("old-" ++ takeFileName fpath)

makeFileOld :: FilePath -> IO ()
makeFileOld fpath = renameFile fpath (old fpath)

tryOrDie :: IO a -> IO a
tryOrDie action = catchIOError action (die . show)

