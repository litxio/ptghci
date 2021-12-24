module Main where

import Foreign.C.Types
import System.IO (hFlush)
import System.Environment
import System.FilePath
import System.Process
import Language.Haskell.PtGhci.Prelude
import Language.Haskell.PtGhci.Engine
import Language.Haskell.PtGhci.StartPy
import Language.Haskell.PtGhci.Env
import Language.Haskell.PtGhci.Log
import Language.Haskell.PtGhci.Config
import System.Console.CmdArgs.Verbosity

main :: IO ()
main = do
  config <- getConfig
  env <- mkEnv config
  when (config ^. verbosity >= Just Trace) $ setVerbosity Loud
  sockets <- setupSockets
  sockAddrs <- socketEndpoints sockets
  debug env $ "Listening on sockets: " ++ show sockAddrs

  -- Check if we are running in "engine mode" -- if so we should not start the
  -- Python interpreter, but should print the ZeroMQ sockets we are using to
  -- stdout
  lookupEnv "PTGHCI_ENGINE_MODE" >>= \case
    Just _ -> runEngineMode env sockets
    Nothing -> runPythonInProc env sockets

-- | The normal way we run -- Python interpreter runs prompt-toolkit loop
-- in-process.
runPythonInProc :: Env -> Sockets -> IO ()
runPythonInProc env sockets = do
  sockAddrs <- socketEndpoints sockets

  withAsync (startPythonApp env sockAddrs) $ \pyThread -> do
    (appThread, quitApp) <- runApp env sockets
    res <- waitEither pyThread appThread
    case res of
      Left () -> quitApp
      Right () -> return ()

-- | Don't run the Python intepreter; just start the engine and listen for a
-- connection from an external Python process.  Used for testing.  In the
-- future may be used for Jupyter notebook connections.
runEngineMode :: Env -> Sockets -> IO ()
runEngineMode env sockets = do
  sockAddrs <- socketEndpoints sockets

  -- Print the socket addresses so Python knows how to connect
  print sockAddrs >> hFlush stdout

  (appThread, quitApp) <- runApp env sockets
  wait appThread `finally` quitApp
