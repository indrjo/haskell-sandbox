
module TeX (makeTeX) where

import RunShell

-- Generate an appropriate shell command. "max_print_line=1000" is to avoid
-- pasky breaks in the log that cicumvent the parser `notFoundsParser`.
texCommand :: FilePath -> FilePath -> String
texCommand engine texf = unwords ["max_print_line=1000", engine, texf]

-- Run the TeX command generated by the function above. Observe, a string
-- of 50 "\n"'s is sent to stdin, to avoid halts caused by missing files.
-- More "\n"'s?
makeTeX :: FilePath -> FilePath -> IO String
makeTeX engine texf = runShell (texCommand engine texf) (replicate 50 '\n')
