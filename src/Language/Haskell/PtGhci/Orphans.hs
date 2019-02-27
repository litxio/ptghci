
module Language.Haskell.PtGhci.Orphans where

import Language.Haskell.PtGhci.Prelude

import Language.Haskell.Ghcid
import Data.Aeson
import GHC.Generics

deriving instance Generic Severity
instance ToJSON Severity

deriving instance Generic Load
instance ToJSON Load

