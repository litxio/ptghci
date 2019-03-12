
module Main where

import Language.Haskell.PtGhci.Prelude
import Language.Haskell.PtGhci.App
import Foreign.C.Types
import System.Environment
import System.FilePath
import System.Process
-- import Paths_ptghci_engine

-- foreign import ccall "run_python_interpreter" runPythonIntepreter :: IO CInt

main = runApp
-- do
--   dataDir <- getDataDir
--   putStrLn dataDir
--   pythonPath <- lookupEnv "PYTHONPATH"
--   let pythonBase = dataDir </> "pybits"
--       pyMain = pythonBase </> "main.py"
--   case pythonPath of
--     Nothing -> setEnv "PYTHONPATH" pythonBase
--     Just path -> setEnv "PYTHONPATH" (path ++ ":" ++ pythonBase)
--   -- runPythonIntepreter
--   createProcess (proc "python3" [pyMain]) {
--     cwd = Nothing,
--     env = Nothing,
--     std_in = Inherit,
--     std_out = Inherit,
--     close_fds = False,
--     create_group = False,
--     delegate_ctlc = True }
--   forever (return ())
