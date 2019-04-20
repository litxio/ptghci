{-# LANGUAGE ForeignFunctionInterface, ExtendedDefaultRules #-}

module Language.Haskell.PtGhci.StartPy where

import Language.Haskell.PtGhci.Prelude
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import System.Environment
import System.Posix.Signals
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.String
import Foreign.Marshal.Array
import Data.Char (ord)
import Paths_ptghci

data PyObject = PyObject
type PyObjectPtr = Ptr PyObject

-- | Starts the Python interpreter and hands over control
startPythonApp :: (String, String, String, String) -> IO ()
startPythonApp (requestAddr, controlAddr, stdoutAddr, stderrAddr) = do
  pythonPath0 <- lookupEnv "PYTHONPATH"
  ourpp <- getDataFileName "pybits"
  let pythonPath = case pythonPath0 of
                     Nothing -> ourpp
                     Just s -> s ++ ":" ++ ourpp
  setEnv "PYTHONPATH" pythonPath
  setEnv "PTGHCI_REQUEST_ADDR" requestAddr
  setEnv "PTGHCI_CONTROL_ADDR" controlAddr
  setEnv "PTGHCI_STDOUT_ADDR" stdoutAddr
  setEnv "PTGHCI_STDERR_ADDR" stderrAddr
  s <- newCString $ unlines [ "import os, sys"
                             ,"from ptghci.app import App"
                             ,"app = App()"
                             ,"app.run()"]

  -- installHandler sigINT (Catch handleInterrupt) Nothing

  -- Run the Python interpreter in an OS thread while having this thread listen
  -- for an AsyncCancelled exception indicating we should exit.
  done <- newEmptyMVar
  withAsyncBound (pyInitialize >> pyRunSimpleString s `finally` finalize done) $
    \_ -> (handle onInterrupt $ takeMVar done)
  where
    -- handleInterrupt = putStrLn "Haskell got unexpected SIGINT" 
    --                     >> pyErrSetInterrupt >> pyErrCheckSignals >> return ()
    
    -- We want to kill the Python interpreter cleanly when we get an
    -- AsyncCanclled exception, so we arrange to handle that exception by
    -- scheduling a Python exception to be thrown
    onInterrupt :: AsyncCancelled -> IO ()
    onInterrupt _ = do
      pendingPtr <- createPendingCallPtr throwPyExit
      gstate <- pyGILStateEnsure
      void $ pyAddPendingCall pendingPtr
      pyGILStateRelease gstate
      
    throwPyExit = do
      eofErr <- peek pyEOFError
      msg <- newCString "Thrown from Haskell"
      pyErrSetString eofErr msg
      return (-1)

    finalize done = pyFinalize >> putMVar done ()

foreign import ccall "Py_Main" pyMain :: CInt -> Ptr (Ptr CInt) -> IO CInt
foreign import ccall "PyRun_SimpleString" pyRunSimpleString :: CString -> IO CInt
foreign import ccall "Py_Initialize" pyInitialize :: IO ()
foreign import ccall "Py_Finalize" pyFinalize :: IO ()
foreign import ccall "PyErr_SetInterrupt" pyErrSetInterrupt :: IO ()
foreign import ccall "PyErr_CheckSignals" pyErrCheckSignals :: IO CInt
foreign import ccall "PyErr_SetString" pyErrSetString :: PyObjectPtr -> CString -> IO ()
foreign import ccall "Py_AddPendingCall" pyAddPendingCall :: FunPtr (IO CInt) -> IO CInt

-- The result of pyGILStateEnsure is actually an enum not int so I am violating
-- an abstraction here.
foreign import ccall "PyGILState_Ensure" pyGILStateEnsure :: IO CInt
foreign import ccall "PyGILState_Release" pyGILStateRelease :: CInt -> IO ()
foreign import ccall "&PyExc_EOFError" pyEOFError :: Ptr PyObjectPtr


foreign import ccall "wrapper" createPendingCallPtr :: IO CInt -> IO (FunPtr (IO CInt))
