
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
import GHC.Generics hiding (Rep)
import Control.Concurrent.Async
import Language.Haskell.Ghcid hiding (Error)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.Posix.Process (joinProcessGroup)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Formatting
import Language.Haskell.PtGhci.PtgRequest as PtgRequest
import Language.Haskell.PtGhci.PtgResponse as PtgResponse
import Language.Haskell.PtGhci.Doc
import Language.Haskell.PtGhci.Env
import Language.Haskell.PtGhci.Log


data PtgException = ConfigurationError Text
  deriving Show
instance Exception PtgException

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

  -- This keeps interrupt signal from the parent (python) process from messing
  -- up our executions. TODO this won't work on Windows -- do I need it?
  joinProcessGroup 0

  bracket (startGhci "stack ghci" Nothing (\_ _ -> return ()))
          (stop . fst)
          $ \(ghci, loadMsgs) -> do
            let env = Env defaultConfig {_webBrowser = Just "firefox"} ghci
            args <- getArgs

            sockets <- case (,,,) <$> head args
                                  <*> (args !? 1) 
                                  <*> (args !? 2) 
                                  <*> (args !? 3) of
                Just (reqPort, controlPort, stdoutPort, stderrPort) ->
                  setupSockets env reqPort controlPort stdoutPort stderrPort
                Nothing -> throw $ ConfigurationError
                                  "Expected two connection ports to be given on command line"

            exec ghci ":set -fdiagnostics-color=always"
            exec ghci ":set -fno-it"
            exec ghci ":set prompt-cont #~GHCID-START~#"
            -- exec ghci ":set +m"
            withAsync (awaitInterrupt (controlSock sockets) ghci) 
             $ \intThread -> forever $ loop env sockets loadMsgs

  where
    loop env sockets@Sockets{..} loadMsgs = do
      request <- receive requestSock
      let req = decode (BSL.fromStrict request) :: Maybe PtgRequest
   
      case req of
        Nothing -> sendResponse requestSock 
                    $ ExecResponse False
                    $ "Request not understood" <> decodeUtf8 request
        Just msg ->
          case msg of
            RequestExec code ->
              withAsync (runMultiline env code)
                        $ \a2 -> do
                          result <- wait a2
                          print  (show $ head result :: Text)
                          let response = ExecResponse True (T.unlines result)
                          sendResponse requestSock response

            RequestExecStream code ->
              withAsync (runMultilineStream env sockets code)
                        $ \a2 -> do
                          result <- wait a2
                          let response = ExecResponse True ""
                          sendResponse requestSock response

            RequestLoadMessages ->
              let response = LoadMessagesResponse True loadMsgs
               in sendResponse requestSock response

            RequestOpenDoc identifier -> do
              result <- try $ findDocForIdentifier env identifier
              let response = 
                    case result of
                      Right path -> ExecResponse True $ pack path
                      Left (ex :: DocException) ->
                        ExecResponse False $ showDocException ex
              sendResponse requestSock response

            RequestOpenSource identifier -> do
              result <- try $ findDocSourceForIdentifier env identifier
              let response = 
                    case result of
                      Right path -> ExecResponse True $ pack path
                      Left (ex :: DocException) ->
                        ExecResponse False $ showDocException ex
              sendResponse requestSock response

sendResponse :: Sender a => Socket a -> PtgResponse -> IO ()
sendResponse sock msg = send sock [] $ BSL.toStrict $ encode msg

onUserInterrupt UserInterrupt = return $ Left ()
onUserInterrupt e = throw e

-- runAndType :: Env -> Text -> IO ([Text], Maybe Text)
-- runAndType env cmd = do
--   resp <- T.unlines <$> runMultiline env (":t " <> cmd)
--   let typeName = if isErrorResponse resp
--                     then Nothing
--                     else last $ T.splitOn " :: " resp
--   (,typeName) <$> runMultiline env cmd

runMultiline :: Env -> Text -> IO [Text]
runMultiline env cmd = eatIt . fmap T.pack <$> exec (_ghci env) (":{\n"++T.unpack cmd++"\n:}\n")

-- | Remove leading lines like "it :: ()".  This happens when +t is on because
-- of the inner workings of ghcid.
eatIt :: [Text] -> [Text]
eatIt = dropWhile (== "it :: ()") 

runMultilineStream :: Env -> Sockets -> Text -> IO ()
runMultilineStream env Sockets{..} cmd 
  = execStream (_ghci env) (":{\n"++T.unpack cmd++"\n:}\n") callback
  where
    callback stream val =
      case stream of
        Stdout -> send stdoutSock [] $ toS val
        Stderr -> send stderrSock [] $ toS val

awaitInterrupt :: Receiver a => Socket a -> Ghci -> IO ()
awaitInterrupt sock ghci = forever $ do
  request <- receive sock
  -- putText "Handling interrupt"
  let reqStr = unpack $ decodeUtf8 request
  when (reqStr == "Interrupt") (interrupt ghci)

stop ghci = do
  stopGhci ghci
  putText "**Engine stopping**"
