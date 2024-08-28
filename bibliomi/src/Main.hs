{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (find)
import System.Environment (getArgs)

import Types
import Scrapers

main :: IO ()
main = getArgs >>= \case
  "list":_ -> printSupportedLibraries
  name:_   -> printLibraryCalendar name
  _        -> putStrLn help

printLibraryCalendar :: String -> IO ()
printLibraryCalendar libname =
  listLibrariesScraper >>= \case
    Just libraries ->
      case lookupLibrary libname libraries of
        Nothing      -> putStrLn $ "no library called: " ++ libname
        Just library ->
          calendarScraper library >>= \case
            Just calendar -> prettyPrint calendar
            Nothing       -> putStrLn "no calendar scraped!"
    Nothing -> putStrLn "the list of libraries is empty!"

lookupLibrary :: String -> [Library] -> Maybe Library
lookupLibrary name = find ((== name) . libraryName)

prettyPrint :: LibraryCalendar -> IO ()
prettyPrint = mapM_ (putStrLn . pretty')
  where
    pretty' (Date name, status) = 
      name ++ ": " ++ case status of
        Open from to -> unwords ["open from", unHour from, "to", unHour to]
        Closed       -> "closed!"
        Unknown str  -> "unknown: " ++ str

printSupportedLibraries :: IO ()
printSupportedLibraries = do
  putStrLn "SUPPORTED LIBRARIES:"
  listLibrariesScraper >>= \case
    Just libraries ->
      let libnames = map libraryName libraries
      in mapM_ (putStrLn . ("* " ++)) libnames
    Nothing -> putStrLn "cannot scrape the list of the libraries :/"

help :: String
help = "USAGE:\n\
       \\t$ bibliomi \"NAME\"\n\
       \\n\
       \For the list of the supported libraries:\n\
       \\t$ bibliomi list"

