{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.PtGhci.Env where
  
import Language.Haskell.PtGhci.Prelude
import Language.Haskell.PtGhci.Ghci
import GHC.Generics
import Lens.Micro.TH
import Data.Aeson
import Control.Exception (throw)
import Text.Printf
import qualified Data.Aeson.Types    as A (Options (..), Parser)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Language.Haskell.PtGhci.Exception

data Verbosity = Critical | Error | Warn | Info | Debug | Trace
  deriving (Enum, Eq, Ord, Show, Generic)
instance FromJSON Verbosity

data Config = Config
  { _verbosity :: Maybe Verbosity
  , _logFile :: Maybe FilePath
  , _webBrowser :: Maybe Text }
  deriving (Show, Generic)

instance FromJSON Config where
    parseJSON = dropOneAndParse

-- | Drop the first character (leading underscore) from the field names
dropOneAndParse :: (Generic a, GFromJSON Zero (Rep a)) => Value -> A.Parser a
dropOneAndParse = genericParseJSON opts
  where
    opts = defaultOptions {A.fieldLabelModifier = drop 1}

makeLenses ''Config

data Env = Env
  { _config :: Config
  , _ghci :: Ghci
  , _logHandle :: Handle
  }
makeLenses ''Env

mkEnv :: Config -> Ghci -> IO Env
mkEnv config ghci = do
  lhandle <- case (_logFile config) of
               Nothing -> return stdout
               Just path -> openFile path WriteMode
  return $ Env config ghci lhandle

defaultConfig = Config (Just Debug) (Just "log.txt") Nothing

loadConfig :: FilePath -> IO Config
loadConfig path = do
  eitherConfig <- decodeFileEither path
  case eitherConfig of
    Left ex -> throw $ ConfigurationError $ toS $
      (printf "Error parsing configuration file %s: %s"
             path (prettyPrintParseException ex) :: String)
    Right c -> return c
