
module Language.Haskell.PtGhci.Exception where

import Language.Haskell.PtGhci.Prelude
import Data.Text (Text, pack, unpack)

data PtgException = ConfigurationError Text
  deriving Show
instance Exception PtgException
