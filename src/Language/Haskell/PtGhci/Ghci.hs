module Language.Haskell.PtGhci.Ghci (
  module Language.Haskell.PtGhci.Ghci,
  Stream(..) 
  ) where

-- | This module consists of logic that relates to communicating with the GHCi
-- process.  Portions of the code in this module are taken verbatim from Neil
-- Mitchell's fabulous ghcid project.  The parts that are messy and/or buggy
-- are mine.

import Language.Haskell.PtGhci.Prelude hiding (traceIO, appendFile)
import Debug.Trace (traceIO)
import System.Process
import Data.Unique
import System.IO hiding (hPutStrLn, putStr, putStrLn, print, hGetLine)
import System.IO.Error
import System.IO.Extra hiding (hPutStrLn, putStr, putStrLn, print, appendFile, hGetLine)
import Control.Concurrent.Extra
import Control.Concurrent.STM
import System.Time.Extra
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

streamLogFile = "streams.log"

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

        logFileH <- openFile streamLogFile AppendMode
        hSetBuffering logFileH LineBuffering

        hSetBuffering out LineBuffering
        hSetBuffering err LineBuffering
        hSetBuffering inp LineBuffering

        whenLoud $ do pid <- getPid ghciProcess
                      appendLine logFileH $ "GHCi process is " ++ show pid
        

        let writeInp x = do
                whenLoud $ appendLine logFileH $ "%STDIN: " ++ x
                hPutStrLn inp x
            prompt = "%~#IGNORE#-%"

        -- Some programs (e.g. stack) might use stdin before starting ghci (see #57)
        -- Send them an empty line
        hPutStrLn inp ("" :: String)

        -- I'd like the GHCi prompt to go away, but that's not possible, so I set it to a special
        -- string and filter that out.
        let ghcid_prefix = "#~GHCID-START~#"
            removePrefix = dropPrefixRepeatedly ghcid_prefix

        -- At various points I need to ensure everything the user is waiting for has completed
        -- So I send messages on stdout/stderr and wait for them to arrive
        syncCount <- newVar 0
        let syncReplay = do
                i <- readVar syncCount
                -- useful to avoid overloaded strings by showing the ['a','b','c'] form, see #109
                let showStr xs = "[" ++ intercalate "," (map show xs) ++ "]"
                let msg = "#~GHCID-FINISH-" ++ show i ++ "~#"
                writeInp $ "INTERNAL_GHCID.putStrLn " ++ showStr msg ++ "\n" ++
                        "INTERNAL_GHCID.hPutStrLn INTERNAL_GHCID.stderr " ++ showStr msg
                return $ isInfixOf msg
        let syncFresh = do
                modifyVar_ syncCount $ return . succ
                syncReplay

        -- Consume from a stream until EOF (return Nothing) or some predicate returns Just
        let consume :: Stream -> (String -> IO (Maybe a)) -> IO (Maybe a)
            consume name finish = do
                let h = if name == Stdout then out else err
                fix $ \rec -> do
                    el <- tryBool isEOFError $ T.unpack . decodeUtf8 <$> BS.hGetLine h
                    case el of
                        Left err -> print err >>
                                      return Nothing
                        Right l -> do
                            -- whenLoud $ appendLine logFileH $ "%" ++ upper (show name) ++ ": " ++ l
                            res <- finish $ removePrefix l
                            case res of
                                Nothing -> rec
                                Just a -> return $ Just a

        let consume2 :: String -> (Stream -> String -> IO (Maybe a)) -> IO (a,a)
            consume2 msg finish = do
                -- fetch the operations in different threads as hGetLine may block
                -- and can't be aborted by async exceptions, see #154
                res1 <- onceFork $ consume Stdout (finish Stdout)
                res2 <- onceFork $ consume Stderr (finish Stderr)
                res1 <- res1
                res2 <- res2
                case liftM2 (,) res1 res2 of
                    Nothing -> case cmdspec process of
                        ShellCommand cmd -> throwIO $ UnexpectedExit cmd msg
                        RawCommand exe args -> throwIO $ UnexpectedExit (unwords (exe:args)) msg
                    Just v -> return v

        -- is anyone running running an exec statement, ensure only one person talks to ghci at a time
        isRunning <- newLock
            
        ghciUnique <- newUnique

        ghciCapturedOut <- newEmptyTMVarIO
        ghciCapturedErr <- newEmptyTMVarIO

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

        let ghciExec :: String -> IO Int  -- Return the signal value
            ghciExec command =
              withLock isRunning $ do
                  seq <- atomically $ do
                    s <- readTVar nextSeq
                    awaitSignal Stdout (s-1) -- Make sure we're caught up
                    awaitSignal Stderr (s-1) -- Make sure we're caught up
                    return s
                   
                  writeInp command
                  -- writeInp "let that = it"
                  sendSignals seq
                  -- writeInp "let it = ___ptghci_it"
                  -- traceIO "Waiting for prompt..."
                  atomically $ modifyTVar nextSeq (+1)
                  return seq


        let ghciExecCapture :: String -> IO ([String], [String])  -- Return the signal value
            ghciExecCapture command =
              -- withLock isInterrupting $ return ()
              withLock isRunning $ do
                  seq <- atomically $ do
                    s <- readTVar nextSeq
                    awaitSignal Stdout (s-1) -- Make sure we're caught up
                    awaitSignal Stderr (s-1) -- Make sure we're caught up
                    putTMVar ghciCapturingSeq s
                    return s

                  writeInp command
                  -- writeInp "let ___ptghci_it = it"
                  sendSignals seq
                  -- writeInp "let it = ___ptghci_it"
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
                                                                else (s:ss)
                            dropTrailingBlank ("":[]) = []
                            dropTrailingBlank (s:ss) = s:dropTrailingBlank ss


        let ghciInterrupt = do
              whenLoud $ appendLine logFileH "Interrupting GHCi"
              interruptProcessGroupOf ghciProcess

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
                el <- tryBool isEOFError $ T.unpack . decodeUtf8 <$> BS.hGetLine h
                case el of
                  Left e -> do
                    whenLoud $ appendLine logFileH $ "Got EOF on " ++ show stream
                    -- when (stream == Stdout) $ do
                    --   mec <- getProcessExitCode ghciProcess
                    --   case mec of
                    --     Nothing -> echo0 stream "After EOF, GHCi process still alive!"
                    --     Just ec -> echo0 stream $ "GHCi died with code " ++ show ec
                    throwIO e

                  Right val' -> do
                    whenLoud (appendLine logFileH $ show stream ++ ": " ++ show val') 
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
                        if capture then modifyIORef captureBuffer (val:)
                                   -- DO send the signal, provided it's not a duplicate
                                   else echo0 stream val'

                        case mbSig of
                          Just finishingSeq -> do
                            whenLoud $ appendLine logFileH $
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
        sync <- newIORef $ const False
        consume2 "" $ \strm s -> do
            stop <- readIORef sync
            if stop s then
                return $ Just ()
            else do
                -- there may be some initial prompts on stdout before I set the prompt properly
                s <- return $ maybe s (removePrefix . snd) $ stripInfix ghcid_prefix s
                whenLoud $ appendLine logFileH $ "%STDOUT2: " ++ s
                modifyIORef (if strm == Stdout then stdout else stderr) (s:)
                when (any (`isPrefixOf` s) ["GHCi, version ","GHCJSi, version "]) $ do
                    -- the thing before me may have done its own Haskell compiling
                    writeIORef stdout [s]
                    writeIORef stderr [s]
                    writeInp "import qualified System.IO as INTERNAL_GHCID"
                    writeInp ":unset +s" 
                    -- Put a newline after the prompt, because we are using line buffering
                    writeInp $ ":set prompt " ++ "\"" ++ prompt ++ "\\n\""

                    -- failure isn't harmful, so do them one-by-one
                    forM_ (ghciFlagsRequired ++ ghciFlagsRequiredVersioned) $ \flag ->
                        writeInp $ ":set " ++ flag
                    writeIORef sync =<< syncFresh
                return Nothing
        r1 <- parseLoad . reverse <$> ((++) <$> readIORef stderr <*> readIORef stdout)

        async $ watchStream Stdout
        async $ watchStream Stderr

        return (ghci, r1)

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
execCapture = ghciExecCapture

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
