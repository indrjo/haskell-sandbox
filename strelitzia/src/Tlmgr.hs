
module Tlmgr (tlmgrInstall, tlmgrSearch) where

import Data.List (intercalate)
import RunShell
import Parsers (listPackageNames)

-- DESCRIPTION
--
-- A minimal TeX Live has *tlmgr* to handle packages: not only you install
-- packages with it, but you can search packages containing a given file!
--

-- Take a list of packages and install them.
tlmgrInstall :: [String] -> IO ()
tlmgrInstall packages =
  callCommand $ "tlmgr install " ++ unwords packages

-- Take a list of files not found and list the packages containing them.
tlmgrSearch :: [String] -> IO [String]
tlmgrSearch notFounds =
  let searchPattern = "'/(" ++ intercalate "|" notFounds ++ ")'" in
    listPackageNames <$>
      runShell ("tlmgr search --global --file " ++ searchPattern) ""

