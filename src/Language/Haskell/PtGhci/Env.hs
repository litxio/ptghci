{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.PtGhci.Env where
  
import Language.Haskell.PtGhci.Prelude
import GHC.Generics
import Language.Haskell.Ghcid hiding (Error)
import Lens.Micro.TH

data Verbosity = Critical | Error | Warn | Info | Debug 
  deriving (Enum, Eq, Ord, Show, Generic)

data Config = Config
  { _verbosity :: Verbosity
  , _webBrowser :: Maybe Text }
  deriving (Show, Generic)
makeLenses ''Config

data Env = Env
  { _config :: Config
  , _ghci :: Ghci }
makeLenses ''Env

defaultConfig = Config Info Nothing
