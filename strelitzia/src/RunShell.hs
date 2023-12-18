
module RunShell (runShell, callCommand) where

import System.Process (readCreateProcessWithExitCode, shell, callCommand)

-- Make the host OS run some command, and return all the output in a unique
-- string. This is achieved by appending "2>&1" at the end of the command.
-- The second argument is a string to be passed to the stdin if the command
-- requires user interaction somewhere.
runShell :: String -> String -> IO String
runShell comm instr = (\(_, out, _) -> out) <$>
  readCreateProcessWithExitCode (shell $ comm ++ " 2>&1") instr

