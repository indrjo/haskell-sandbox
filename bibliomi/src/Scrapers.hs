
module Scrapers ( calendarScraper
                , listLibrariesScraper
                ) where

-- Module providing some web scrapers for the website of public libraries
-- of the city of Milan.
--
--   https://milano.biblioteche.it/...
--

import Data.List.Extra (trim)
import Text.HTML.Scalpel
import Text.Parsec
import Text.Parsec.String

import Types

-- Extract the calendar of a given library. Visit the page
--   
--   https://milano.biblioteche.it/library/NAME/timetable
--
-- and scrape the calendar you can find there.
calendarScraper :: Library -> IO (Maybe LibraryCalendar)
calendarScraper (Library _ url) =
  scrapeURL (timetableURL url) $
    chroot (TagString "table" @: [hasClass "library-timetable"]) $
      chroots (TagString "tr" @: []) $ do
        day:interval:_ <- texts $ TagString "td" @: []
        return (Date day, parseStatus $ trim interval)

-- If the string is "Chiusa" then give Closed, if the string has the form
-- "hh1:mm1 - hh2:mm2" then output Open "hh1:mm1" "hh2:mm2". Unknown is for
-- any other string.
parseStatus :: String -> Status
parseStatus = either (Unknown . show) id . parse parser ""
  where
    parser :: Parser Status
    parser = try (string "Chiusa" >> return Closed)
         <|> try (Open <$> (hour <* string " - ") <*> hour)
         <|> (Unknown <$> getInput)
    hour :: Parser Hour
    hour = do
      hh <- count 2 digit
      _  <- char ':'
      mm <- count 2 digit
      return $ Hour (hh ++ ':' : mm)

-- Scrape the list of all the public libraries in Milan directly from the
-- main page of the public libraries.
listLibrariesScraper :: IO (Maybe [Library])
listLibrariesScraper =
  scrapeURL baseURL $
    chroot (TagString "ul" @: [hasClass "dropdown-menu"]) $
      chroots (TagString "a" @: []) $ do
        name <- text $ TagString "a" @: []
        href <- attr "href" $ TagString "a" @: []
        return $ Library name (baseURL ++ href)

-- The homepage URL.
baseURL :: URL
baseURL = "https://milano.biblioteche.it/"

timetableURL :: URL -> URL
timetableURL url = url ++ "timetable/"

