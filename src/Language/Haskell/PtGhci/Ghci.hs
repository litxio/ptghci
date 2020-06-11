module Language.Haskell.PtGhci.Ghci (
  module Language.Haskell.PtGhci.Ghci,
  Stream(..) 
  ) where

import Language.Haskell.PtGhci.Prelude hiding (traceIO, appendFile, unlines)
import Debug.Trace (traceIO)
import System.Process
import Data.Unique
import System.IO hiding (hPutStrLn, putStr, putStrLn, print, hGetLine)
import System.IO.Error
import System.IO.Extra hiding (hPutStrLn, putStr, putStrLn, print, appendFile, hGetLine)
import Control.Concurrent.Extra
import Control.Concurrent.STM
import System.Time.Extra
import GHC.Conc (unsafeIOToSTM)
import Control.Exception.Extra hiding (throwIO)
import Data.List.Extra hiding (map, head)
import Data.Maybe
import Data.IORef
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Console.CmdArgs.Verbosity
import Language.Haskell.Ghcid.Parser
import Language.Haskell.Ghcid.Types
import Language.Haskell.Ghcid.Util

-- | This module consists of logic that relates to communicating with the GHCi
-- process.  Portions of the code in this module are taken verbatim from Neil
-- Mitchell's fabulous ghcid project.  The parts that are messy and/or buggy
-- are mine.

streamLogFile = "streams.log"

data StartupIsFinished = StartupIsFinished deriving (Eq, Show)
instance Exception StartupIsFinished

appendLine :: Handle -> String -> IO ()
appendLine h s = hPutStrLn h (s ++ "\n")

-- | A GHCi session. Created with 'startGhci', closed with 'stopGhci'.
--
--   The interactions with a 'Ghci' session must all occur single-threaded,
--   or an error will be raised. The only exception is 'interrupt', which aborts
--   a running computation, or does nothing if no computation is running.
data Ghci = Ghci
    {ghciProcess :: ProcessHandle
    ,ghciInterrupt :: IO ()
    ,ghciExec :: String -> IO Int
    ,ghciExecCapture :: String -> IO ([String], [String])
    ,ghciUnique :: Unique
    ,ghciLastSeqSeenOut :: TVar Int
    ,ghciLastSeqSeenErr :: TVar Int
    ,ghciCapturingSeq :: TMVar Int
    ,ghciCapturedOut :: TMVar [String]
    ,ghciCapturedErr :: TMVar [String]
    }

withCreateProc proc f = do
    let undo (_, _, _, proc) = ignored $ terminateProcess proc
    bracketOnError (createProcess proc) undo $ \(a,b,c,d) -> f a b c d

startGhciProcess :: CreateProcess -> (Stream -> String -> IO ()) -> IO (Ghci, [Load])
startGhciProcess process echo0 = do
    let proc = process{std_in=CreatePipe, std_out=CreatePipe
                      , std_err=CreatePipe, create_group=True}
    withCreateProc proc $ \(Just inp) (Just out) (Just err) ghciProcess -> do

        logFileHRef <- newIORef Nothing

        let streamLog msg = whenLoud $ do
              logFileH <- readIORef logFileHRef >>= \case
                Nothing -> do
                  h <- openFile streamLogFile AppendMode
                  hSetBuffering h LineBuffering
                  writeIORef logFileHRef $ Just h
                  return h
                Just h -> return h
              appendLine logFileH msg

        hSetBuffering out LineBuffering
        hSetBuffering err LineBuffering
        hSetBuffering inp LineBuffering

        whenLoud $ do pid <- getPid ghciProcess
                      streamLog $ "GHCi process is " ++ show pid
        

        let writeInp x = do
                streamLog $ "%STDIN: " ++ x
                hPutStrLn inp x
            prompt = "%~#IGNORE#-%"

        -- Some programs (e.g. stack) might use stdin before starting ghci (see #57)
        -- Send them an empty line
        hPutStrLn inp ("" :: String)

        -- I'd like the GHCi prompt to go away, but that's not possible, so I set it to a special
        -- string and filter that out.
        -- At various points I need to ensure everything the user is waiting for has completed
        -- So I send messages on stdout/stderr and wait for them to arrive
        --
        -- The lock ensures only one thread talks to ghci at a time.
        isRunning <- newLock
            
        ghciUnique <- newUnique

        nextSeq <- newTVarIO 0
        ghciLastSeqSeenOut <- newTVarIO (-1)
        ghciLastSeqSeenErr <- newTVarIO (-1)
        ghciCapturingSeq <- newEmptyTMVarIO
        let awaitSignal stream i = readTVar (lastSeqSeen stream) >>= check . (>=i)
            lastSeqSeen stream = case stream of
                                   Stdout -> ghciLastSeqSeenOut
                                   Stderr -> ghciLastSeqSeenErr

        let sendSignals seq = do
              writeInp $ "INTERNAL_GHCID.hPutStrLn INTERNAL_GHCID.stdout \""
                          ++ mkSignal seq ++ "\""
              writeInp $ "INTERNAL_GHCID.hPutStrLn INTERNAL_GHCID.stderr \"\\n"
                          ++ mkSignal seq ++ "\""

            -- | write the entry to GHCi and send the signal
        let submitInput :: String -> Int -> IO ()
            submitInput entry seq = do
                  if isGhciCommand entry
                     then streamLog $ entry ++ " looks like a command"
                     else streamLog $ entry ++ " doesn't look like a command"
                  writeInp entry
                  unless (isGhciCommand entry) $ writeInp "let that = it"
                  -- Make sure INTERNAL_GHCID doesn't go out of scope when the
                  -- user types :module or :load
                  when (resetsModules entry) $
                    writeInp "import qualified System.IO as INTERNAL_GHCID"
                  sendSignals seq
                  -- unless (isGhciCommand entry) $ writeInp "let it = __ptghci_it"

        -- Make sure we've seen the latest signal on both streams, and return
        -- the next signal.
        let syncSeq :: STM Int
            syncSeq = do
              s <- readTVar nextSeq
              awaitSignal Stdout (s-1) -- Make sure we're caught up
              awaitSignal Stderr (s-1) -- Make sure we're caught up
              return s

            cycleSeq :: IO ()
            cycleSeq = do
              s <- atomically syncSeq
              sendSignals s
              void $ atomically syncSeq

        -- It seems that, after interrupting GHCi, the next signal I send for
        -- stdout gets swallowed up - I never see it [#16].  To compensate, I
        -- do an extra signal/sync cycle after an interrupt.  This flag
        -- indicates that we should do so before handling the next request.
        justInterrupted <- newIORef False

        let ghciInterrupt = do
              streamLog "Interrupting GHCi"
              interruptProcessGroupOf ghciProcess
              writeIORef justInterrupted True

        let ghciExec :: String -> IO Int  -- Return the signal value
            ghciExec entry =
              withLock isRunning $ do
                  whenM (readIORef justInterrupted) $
                    cycleSeq >> writeIORef justInterrupted False
                  seq <- atomically syncSeq
                   
                  submitInput entry seq
                  atomically $ modifyTVar nextSeq (+1)
                  return seq

        -- Storage for capturing stdout/stderr
        ghciCapturedOut <- newEmptyTMVarIO
        ghciCapturedErr <- newEmptyTMVarIO

        -- | ghciExecCapture works by setting ghciCapturingSeq (which tells
        -- watchStream that we are doing a capture) and then waiting for the
        -- next signal.
        let ghciExecCapture :: String -> IO ([String], [String])
            ghciExecCapture entry =
              -- withLock isInterrupting $ return ()
              withLock isRunning $ do
                  whenM (readIORef justInterrupted) $
                    cycleSeq >> writeIORef justInterrupted False
                  seq <- atomically $ do
                    s <- syncSeq
                    putTMVar ghciCapturingSeq s
                    return s

                  submitInput entry seq
                  atomically $ do
                    awaitSignal Stdout seq
                    awaitSignal Stderr seq
                    tryTakeTMVar ghciCapturingSeq
                    out <- takeTMVar ghciCapturedOut
                    err <- takeTMVar ghciCapturedErr
                    modifyTVar nextSeq (+1)
                    return ( dropTrailingBlank $ dropLeadingBlank out
                           , dropTrailingBlank $ dropLeadingBlank err)
                      where dropLeadingBlank [] = []
                            dropLeadingBlank (s:ss) = if null s then ss
                                                                else s:ss
                            dropTrailingBlank [""] = []
                            dropTrailingBlank [] = []
                            dropTrailingBlank (s:ss) = s:dropTrailingBlank ss



        -- sendSignals results in a lot of "it :: ()" being printed out, when
        -- ':set +t' is active so ignore those.  TODO: Note that this isn't 
        -- strictly right - sometimes the user executes an IO () action and
        -- technically should want to see the "it :: ()".
        let keep line = line /= prompt && line /= "it :: ()"

        let watchStream stream = do
              captureBuffer <- newIORef []
              forever $ do
                let h           = if stream == Stdout then out else err
                    captureDest = if stream == Stdout then ghciCapturedOut 
                                                      else ghciCapturedErr
                el <- tryBool isEOFError $ T.unpack 
                                           . T.replace "\r" ""
                                           . decodeUtf8 <$> BS.hGetLine h
                case el of
                  Left e -> do
                    streamLog $ "Got EOF on " ++ show stream
                    -- when (stream == Stdout) $ do
                    --   mec <- getProcessExitCode ghciProcess
                    --   case mec of
                    --     Nothing -> echo0 stream "After EOF, GHCi process still alive!"
                    --     Just ec -> echo0 stream $ "GHCi died with code " ++ show ec
                    throwIO e

                  Right val' -> do
                    streamLog $ show stream ++ ": " ++ show val'
                    when (keep val') $ do
                      -- traceIO $ "Read line " ++ val' ++ " -- " ++ show stream
                      let (val, mbSig) = stripSignal val'

                      (mbCapSeq, lastSeq) <- atomically $ 
                        (,) <$> tryReadTMVar ghciCapturingSeq
                            <*> readTVar (lastSeqSeen stream)

                      let capture = case mbCapSeq of
                                      Nothing -> False
                                      Just capSeq -> capSeq == lastSeq + 1
                          dupSig = ((<= lastSeq) <$> mbSig) == Just True && null val

                      unless dupSig $ do
                        if capture then do streamLog "(captured)"
                                           modifyIORef captureBuffer (val:)
                                   -- DO send the signal, provided it's not a duplicate
                                   else echo0 stream val'

                        case mbSig of
                          Just finishingSeq -> do
                            streamLog $
                              "Finishing " ++ show finishingSeq ++ " -- " ++ show stream
                            atomically $ writeTVar (lastSeqSeen stream) finishingSeq
                            capRes <- readIORef captureBuffer
                            when capture $ do
                              finishCapture <- if finishingSeq >= fromJust mbCapSeq
                                                  then atomically $ do
                                                      putTMVar captureDest (reverse capRes)
                                                      return True
                                                  else return False

                              when finishCapture $ writeIORef captureBuffer []
                          Nothing -> return ()

        let ghci = Ghci{..}

        -- Now wait for 'GHCi, version' to appear before sending anything real
        stdout <- newIORef []
        stderr <- newIORef []
        let readDuringStartup :: Stream -> IO ()
            readDuringStartup stream = handle (\(e::StartupIsFinished) -> return ()) $ do
                let h = if stream == Stdout then out else err
                    capture = if stream == Stdout then stdout else stderr
                el <- tryBool isEOFError $ T.unpack
                                           . T.replace "\r" ""
                                           . decodeUtf8 <$> BS.hGetLine h
                case el of
                  Left e -> do
                    let msg = "Got EOF from GHCi on " <> show stream <> " during startup"
                    streamLog msg -- >> putErrLn msg
                    throwIO e
                  Right s -> do
                    -- putStrLn $ "Appending  to" ++ show stream ++ ": " ++ s
                    modifyIORef (if stream == Stdout then stdout else stderr) (s:)
                    echo0 stream s
                    unless (stream == Stdout 
                            && any (`isPrefixOf` s) ["GHCi, version ","GHCJSi, version "]) $
                      readDuringStartup stream

        catch (withAsync (readDuringStartup Stdout) $ \outThread ->
                withAsync (readDuringStartup Stderr) $ \errThread -> do
                  wait outThread 
                  errThread `cancelWith` StartupIsFinished
                  void $ waitCatch errThread)
              (\(e :: SomeException) -> do
                putErrLn "-- GHCi startup failed, output follows --"
                readIORef stdout >>= putStrLn . unlines
                readIORef stderr >>= putStrLn . unlines )

        -- Set up the GHCi environment
        writeInp "import qualified System.IO as INTERNAL_GHCID"
        writeInp ":unset +s" 
        writeInp "INTERNAL_GHCID.hSetBuffering INTERNAL_GHCID.stdout INTERNAL_GHCID.LineBuffering"
        writeInp "INTERNAL_GHCID.hSetBuffering INTERNAL_GHCID.stderr INTERNAL_GHCID.LineBuffering"
        -- Put a newline after the prompt, because we are using line buffering
        writeInp $ ":set prompt " ++ "\"" ++ prompt ++ "\\n\""
        writeInp ":set prompt-cont \"\""

        -- failure isn't harmful here, so set flags one-by-one
        forM_ (ghciFlagsRequired ++ ghciFlagsRequiredVersioned) $ \flag ->
            writeInp $ ":set " ++ flag

        async $ watchStream Stdout
        async $ watchStream Stderr

        errMsgs <- readIORef stderr
        outMsgs <- readIORef stdout

        -- Run execCapture once to capture any leftover text sitting on stdout/stderr.
        -- We also use this opportunity to see whether we have started up properly
        res <- try $ execCapture ghci "0" :: IO (Either SomeException ([String], [String]))
        (moreOutput, moreErrs) 
          <- case res of
               Left exc -> do
                 streamLog $ "Critical exception in execCapture: " ++ show exc
                 putErrLn "It looks like GHCi did not start up properly.  The start-up messages were:"
                 putErrLn $ unlines errMsgs
                 putStrLn $ unlines outMsgs
                 die "GHCi did not start up properly"
               Right more -> return more
        -- traverse (echo0 Stdout) moreOutput
        traverse (echo0 Stderr) moreErrs

        let r1 = parseLoad . reverse $ errMsgs ++ outMsgs
        r2 <- if any isLoading r1 then return [] else map (uncurry Loading) <$> showModules ghci

        return (ghci, r1 ++ r2)

-- | Start GHCi by running the given shell command, a helper around 'startGhciProcess'.
startGhci
    :: String -- ^ Shell command
    -> Maybe FilePath -- ^ Working directory
    -> (Stream -> String -> IO ()) -- ^ Output callback
    -> IO (Ghci, [Load])
startGhci cmd directory = startGhciProcess (shell cmd){cwd=directory}

-- | Send a command, get lines of result from stdout and stderr.
-- Must be called single-threaded.
execCapture :: Ghci -> String -> IO ([String], [String])
execCapture g s = ghciExecCapture g s

-- | Execute a command, echoing results over streams
execStream :: Ghci -> String -> IO Int
execStream = ghciExec

-- | Interrupt Ghci, stopping the current computation (if any),
--   but leaving the process open to new input.
interrupt :: Ghci -> IO ()
interrupt = ghciInterrupt


-- | Stop GHCi. Attempts to interrupt and execute @:quit:@, but if that doesn't
-- complete within 20 milliseconds it just terminates the process.
stopGhci :: Ghci -> IO ()
stopGhci ghci = race_ (quit ghci)
                      (sleep 0.02 >> terminateProcess (process ghci))

-- | List the modules currently loaded, with module name and source file.
showModules :: Ghci -> IO [(String,FilePath)]
showModules ghci = parseShowModules . fst <$> execCapture ghci ":show modules"

-- | Return the current working directory, and a list of module import paths
showPaths :: Ghci -> IO (FilePath, [FilePath])
showPaths ghci = parseShowPaths . fst <$> execCapture ghci ":show paths"

-- | Perform a reload, list the messages that reload generated.
reload :: Ghci -> IO [Load]
reload ghci = parseLoad . fst <$> execCapture ghci ":reload"

-- | Send @:quit@ and wait for the process to quit.
quit :: Ghci -> IO ()
quit ghci =  do
  interrupt ghci
  handle (\UnexpectedExit{} -> return ()) $ void $ execStream ghci ":quit"
  -- Be aware that waitForProcess has a race condition, see https://github.com/haskell/process/issues/46.
  -- Therefore just ignore the exception anyway, its probably already terminated.
  void $ waitForProcess $ process ghci


-- | Obtain the progress handle behind a GHCi instance.
-- 
process :: Ghci -> ProcessHandle
process = ghciProcess

signalPrefix, signalSuffix :: String
signalPrefix = "#~PTGHCI~SYNC~"
signalSuffix = "~#"

mkSignal :: Int -> String
mkSignal seq = signalPrefix++show seq++signalSuffix

stripSignal :: String -> (String, Maybe Int)
stripSignal s
  | not (s `startsWith` signalPrefix) = (s, Nothing)
  | [(sig, rest)] <- reads (drop (length signalPrefix) s)
  , rest `startsWith` signalSuffix = (drop (length signalSuffix) rest, Just sig)
startsWith s pre = take (length pre) s == pre

resetsModules entry =
  ":loa" `isPrefixOf` (dropColonBracket entry) 
  || ":mo" `isPrefixOf` (dropColonBracket entry)

isGhciCommand entry = ":" `isPrefixOf` (dropColonBracket entry)

dropColonBracket s = if ":{\n" `isPrefixOf` s then drop 3 s else s
