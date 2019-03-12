{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Language.Haskell.PtGhci.App where

import Language.Haskell.PtGhci.Prelude hiding (Rep)

import Control.Monad
import Control.Exception (finally, bracket, catch, SomeException, try,
                          displayException, AsyncException(..), throw, uninterruptibleMask_)
import Text.Printf
import Language.Haskell.Exts.Parser (parseStmt, parseDecl, parseModule,
                                     ParseResult(..)) 
import Language.Haskell.Exts.Pretty (prettyPrintStyleMode, style, defaultMode,
                                     PPLayout(..), PPHsMode(..))
import GHC.Base as Base 
import Data.IORef
import Data.Maybe
import System.ZMQ4
import System.Environment (getArgs)
import Data.Aeson
import qualified Debug.Trace as Debug
import GHC.Generics hiding (Rep)
import Control.Concurrent.Async
import Language.Haskell.Ghcid hiding (Error)
import Data.Text (Text, pack, unpack)
import Data.List ((!!))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.Posix.Process (joinProcessGroup)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Text.Regex.PCRE.Heavy
import Formatting
import System.Console.CmdArgs.Verbosity
import Language.Haskell.PtGhci.PtgRequest as PtgRequest
import Language.Haskell.PtGhci.PtgResponse as PtgResponse
import Language.Haskell.PtGhci.Doc
import Language.Haskell.PtGhci.Exception
import Language.Haskell.PtGhci.Env
import Language.Haskell.PtGhci.Log


data Sockets = Sockets 
  { requestSock :: Socket Rep
  , controlSock :: Socket Pair
  , stdoutSock :: Socket Pub
  , stderrSock :: Socket Pub 
  }

setupSockets :: Env
             -> Base.String 
             -> Base.String 
             -> Base.String 
             -> Base.String 
             -> IO Sockets
setupSockets c reqPort controlPort stdoutPort stderrPort = do
  info c $ format ("Request/control sockets: "%string%", "%string
                   %", "%string%", "%string)
                   reqPort controlPort stdoutPort stderrPort
  ctx <- context
  requester <- socket ctx Rep
  controlSock <- socket ctx Pair
  stdoutSock <- socket ctx Pub
  stderrSock <- socket ctx Pub
  connect requester $ "tcp://127.0.0.1:" ++ reqPort
  connect controlSock $ "tcp://127.0.0.1:" ++ controlPort
  connect stdoutSock $ "tcp://127.0.0.1:" ++ stdoutPort
  connect stderrSock $ "tcp://127.0.0.1:" ++ stderrPort
  return $ Sockets requester controlSock stdoutSock stderrSock

runApp :: IO ()
runApp = do
  -- putStrLn "Connected"
  -- send requester [] "Hello"
  --

  args <- getArgs
  config <- if length args > 4
               then loadConfig (args !! 4) 
               else return defaultConfig
  when (config ^. verbosity >= Just Trace) $ setVerbosity Loud

  -- This keeps interrupt signal from the parent (python) process from messing
  -- up our executions. TODO this won't work on Windows -- do I need it?
  joinProcessGroup 0

  bracket (startGhci "stack ghci" Nothing (\_ _ -> return ()))
          (stop . fst)
          $ \(ghci, loadMsgs) -> do
            env <- mkEnv config ghci
            -- When verbosity is Trace, make ghcid loud too

            sockets <- case (,,,) <$> head args
                                  <*> (args !? 1) 
                                  <*> (args !? 2) 
                                  <*> (args !? 3) of
                Just (reqPort, controlPort, stdoutPort, stderrPort) ->
                  setupSockets env reqPort controlPort stdoutPort stderrPort
                Nothing -> throw $ ConfigurationError
                                  "Expected two connection ports to be given on\
                                    \ command line"

            exec ghci ":set -fdiagnostics-color=always"
            -- exec ghci ":set -fno-it"
            exec ghci ":set prompt-cont #~GHCID-START~#"
            -- exec ghci ":set +m"
            withAsync (awaitInterrupt (controlSock sockets) ghci) 
             $ \intThread -> forever $ loop env sockets loadMsgs

  where
    loop env sockets@Sockets{..} loadMsgs = do
      request <- receive requestSock
      let req = decode (BSL.fromStrict request) :: Maybe PtgRequest
      debug env ("Got request " <> show req :: Text)
   
      case req of
        Nothing -> sendResponse env requestSock 
                    $ ExecResponse False
                    $ "Request not understood" <> decodeUtf8 request
        Just msg ->
          case msg of
            RequestExec code ->
              withAsync (runMultiline env code)
                        $ \a2 -> do
                          result <- wait a2
                          let response = ExecResponse True (T.unlines result)
                          sendResponse env requestSock response

            RequestExecStream code ->
              withAsync (runMultilineStream env sockets code)
                        $ \a2 -> do
                          result <- wait a2
                          let response = ExecResponse True ""
                          sendResponse env requestSock response

            RequestLoadMessages ->
              let response = LoadMessagesResponse True loadMsgs
               in sendResponse env requestSock response

            RequestType identifier -> do
              result <- runLine env $ ":t " <> identifier
              -- putStrLn $ "First line of result: " <> stripAnsi (fromJust (head result))
              let response = ExecResponse (not $ checkForError result)
                                          (T.strip $ T.unlines $ dropBlankLines result)
              sendResponse env requestSock response

            RequestOpenDoc identifier -> do
              result <- try $ findDocForIdentifier env identifier
              let response = 
                    case result of
                      Right path -> ExecResponse True $ pack path
                      Left (ex :: DocException) ->
                        ExecResponse False $ showDocException ex
              sendResponse env requestSock response

            RequestOpenSource identifier -> do
              result <- try $ findDocSourceForIdentifier env identifier
              let response = 
                    case result of
                      Right path -> ExecResponse True $ pack path
                      Left (ex :: DocException) ->
                        ExecResponse False $ showDocException ex
              sendResponse env requestSock response

sendResponse :: Sender a => Env -> Socket a -> PtgResponse -> IO ()
sendResponse env sock msg = do
  debug env ("Sending response: " <> show msg :: Text)
  send sock [] $ BSL.toStrict $ encode msg

onUserInterrupt UserInterrupt = return $ Left ()
onUserInterrupt e = throw e

-- ghcid imports several modules under the name "INTERNAL_GHCID".  Showing
-- this import name will just be confusing to the end user, so strip it out
-- whenever it appears in a message.
stripInternalGhcid :: Text -> Text
stripInternalGhcid = T.replace "INTERNAL_GHCID." ""

runLine :: Env -> Text -> IO [Text]
runLine env cmd = fmap (stripInternalGhcid . T.pack)
                      <$> exec (_ghci env) (T.unpack cmd)

runMultiline :: Env -> Text -> IO [Text]
runMultiline env cmd = fmap (stripInternalGhcid . T.pack) 
                          <$> exec (_ghci env) (":{\n"++T.unpack cmd++"\n:}\n")

runMultilineStream :: Env -> Sockets -> Text -> IO ()
runMultilineStream env Sockets{..} cmd
  = execStream (_ghci env) (":{\n"++T.unpack cmd++"\n:}\n") callback
  where
  callback stream val =
      case stream of
        -- TODO -- double conversion inefficient
        Stdout -> send stdoutSock [] $ toS (stripInternalGhcid $ toS val)
        Stderr -> send stderrSock [] $ toS (stripInternalGhcid $ toS val)


checkForError :: [Text] -> Bool
checkForError = checkForError' . dropBlankLines
  where
    checkForError' (l:ls) = stripAnsi l =~ [re|^<(\w|\s)+>:(\d+:\d+:)? error:.*|]
    checkForError' _ = False

-- Ahhhhhhhhhh!!  This comes from https://github.com/chalk/ansi-regex
ansiRe :: Regex
ansiRe = [re|[\e\x9B][[\]()#;?]*(?:(?:(?:[a-zA-Z\d]*(?:;[a-zA-Z\d]*)*)?\x07)|(?:(?:\d{1,4}(?:;\d{0,4})*)?[\dA-PR-TZcf-ntqry=><~]))|]

stripAnsi :: Text -> Text
stripAnsi t = gsub ansiRe (""::Text) (T.strip t)

dropBlankLines :: [Text] -> [Text]
dropBlankLines = filter ((/= "") . T.strip)

-- | Remove leading lines like "it :: ()".  This happens when +t is on because
-- of the inner workings of ghcid.
eatIt :: [Text] -> [Text]
eatIt = dropWhile (== "it :: ()") 

awaitInterrupt :: Receiver a => Socket a -> Ghci -> IO ()
awaitInterrupt sock ghci = forever $ do
  request <- receive sock
  putText "Handling interrupt"
  let reqStr = unpack $ decodeUtf8 request
  when (reqStr == "Interrupt") (interrupt ghci)

stop ghci = do
  stopGhci ghci
  putText "**Engine stopping**"
