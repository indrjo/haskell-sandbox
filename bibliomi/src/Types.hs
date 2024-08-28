
module Types where

-- The second field should be something like
--
--   https://milano.biblioteche.it/library/NAME/
--
data Library = Library
  { libraryName :: String
  , libraryURL  :: String
  } deriving (Show)

-- The library status.
data Status = Open Hour Hour
            -- if open, the interval for which is open
            | Closed
            -- closed the whole day
            | Unknown String
            -- who knows? (for parsing purposes see below)
  deriving (Show)

-- A date is just a string. For example: "Fri 02 August".
newtype Date = Date { unDate :: String } 
  deriving (Show)

-- An hour is just a string. For example: "14:00".
newtype Hour = Hour { unHour :: String }
  deriving (Show)

type LibraryCalendar = [(Date, Status)]

