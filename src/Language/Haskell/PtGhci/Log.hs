module Language.Haskell.PtGhci.Log where

import Language.Haskell.PtGhci.Prelude
import Language.Haskell.PtGhci.Env
import System.IO (hFlush)

getVerboseLevel :: Config -> Verbosity
getVerboseLevel config = case config ^. verbosity of
                        Nothing -> Info
                        Just l -> l

writeLog :: Env -> Text -> IO ()
writeLog e t = case handle of
                 Nothing -> return ()
                 Just h -> hPutStrLn h t >> hFlush h
  where handle = e ^. logHandle

logerr :: StringConv a Text => Env -> a -> IO ()
logerr e t = when (getVerboseLevel (e^.config) >= Error) $ writeLog e (toS t)

info :: StringConv a Text => Env -> a -> IO ()
info e t = when (getVerboseLevel (e^.config) >= Info) $ writeLog e (toS t)

debug :: StringConv a Text => Env -> a -> IO ()
debug e t = when (getVerboseLevel (e^.config) >= Debug) $ writeLog e (toS t)

trace :: StringConv a Text => Env -> a -> IO ()
trace e t = when (getVerboseLevel (e^.config) >= Trace) $ writeLog e (toS t)
