
module RunShell ( runShell
                , callCommand
                , wgetSpider
                ) where

import System.Process (readCreateProcessWithExitCode, shell, callCommand)
import System.Exit (ExitCode(..))

-- Make the host OS run some command, and return all its output as a unique
-- string and the the exit status. The second argument is a string to be
-- passed to the stdin if the command requires user interaction somewhere.
readShellCommand :: String -> String -> IO (ExitCode, String)
readShellCommand comm inpstr =
  let comm' = shell $ unwords [comm, "2>&1"] in do
    (exitcode, allout, _) <- readCreateProcessWithExitCode comm' inpstr
    return (exitcode, allout)

runShell :: String -> String -> IO String
runShell comm inpstr = do
  (_, out) <- readShellCommand comm inpstr
  return out

quote :: String -> String
quote str = "'" ++ str ++ "'"

-- Check if you can reach a given url.
wgetSpider :: String -> IO Bool
wgetSpider url = do
  (exitcode, _) <- readShellCommand ("wget -q --spider " ++ quote url) ""
  return $ exitcode == ExitSuccess

