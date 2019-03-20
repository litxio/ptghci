module Language.Haskell.PtGhci.Log where

import Language.Haskell.PtGhci.Prelude
import Language.Haskell.PtGhci.Env
import System.IO (hFlush)

getVerbosity :: Env -> Verbosity
getVerbosity env = case env ^. config . verbosity of
                   Nothing -> Info
                   Just l -> l

writeLog :: Env -> Text -> IO ()
writeLog e t = hPutStrLn handle t >> hFlush handle
  where handle = e ^. logHandle

info :: StringConv a Text => Env -> a -> IO ()
info e t = when (getVerbosity e >= Info) $ writeLog e (toS t)

debug :: StringConv a Text => Env -> a -> IO ()
debug e t = when (getVerbosity e >= Debug) $ writeLog e (toS t)

trace :: StringConv a Text => Env -> a -> IO ()
trace e t = when (getVerbosity e >= Trace) $ writeLog e (toS t)
