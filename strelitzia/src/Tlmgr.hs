
module Tlmgr (tlmgrInstall, tlmgrSearch) where

import Data.List (intercalate)
import RunShell
import Parsers (listPackageNames)


-- DESCRIPTION
--
-- A minimal TeX Live has *tlmgr* to handle packages: not only you install
-- packages with it, but you can search packages containing a given file!
--

-- ------------------------------------------------------------------------
-- Search for packages.
-- ------------------------------------------------------------------------

-- Take a list of files not found and list the packages containing them.
tlmgrSearch :: [String] -> IO [String]
tlmgrSearch notFounds =
  listPackageNames <$> runShell ("tlmgr search --global --file "
                                 ++ searchPattern notFounds) ""

searchPattern :: [String] -> String
searchPattern []     = ""
searchPattern [name] = "'/" ++ name ++ "'"
searchPattern names  = "'/(" ++ intercalate "|" names ++ ")'"


-- ------------------------------------------------------------------------
-- Install packages.
-- ------------------------------------------------------------------------

tlmgrInstall :: [String] -> IO ()
tlmgrInstall packages =
  callCommand $ "tlmgr install " ++ unwords packages

