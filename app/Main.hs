{-# LANGUAGE LambdaCase #-}

module Main where

import Language.Haskell.PtGhci.Prelude
import Language.Haskell.PtGhci.Engine
import Language.Haskell.PtGhci.StartPy
import Foreign.C.Types
import System.IO (hFlush)
import System.Environment
import System.FilePath
import System.Process

main :: IO ()
main = do
  sockets <- setupSockets

  -- Check if we are running in "engine mode" -- if so we should not start the
  -- Python interpreter, but should print the ZeroMQ sockets we are using to
  -- stdout
  lookupEnv "PTGHCI_ENGINE_MODE" >>= \case
    Just _ -> runEngineMode sockets
    Nothing -> runPythonInProc sockets
  

-- | The normal way we run -- Python interpreter runs prompt-toolkit loop
-- in-process.
runPythonInProc :: Sockets -> IO ()
runPythonInProc sockets = do
  sockAddrs <- socketEndpoints sockets

  (appThread, quitApp) <- runApp sockets
  withAsync (startPythonApp sockAddrs) $ \pyThread -> do
    res <- waitEither pyThread appThread
    case res of
      Left () -> quitApp
      Right () -> return ()

-- | Don't run the Python intepreter; just start the engine and listen for a
-- connection from an external Python process.  Used for testing.  In the
-- future may be used for Jupyter notebook connections.
runEngineMode :: Sockets -> IO ()
runEngineMode sockets = do
  sockAddrs <- socketEndpoints sockets
  
  -- Print the socket addresses so Python knows how to connect
  print sockAddrs >> hFlush stdout

  (appThread, quitApp) <- runApp sockets
  wait appThread `finally` quitApp

