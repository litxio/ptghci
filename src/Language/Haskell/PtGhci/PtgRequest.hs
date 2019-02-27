

module Language.Haskell.PtGhci.PtgRequest where

import Language.Haskell.PtGhci.Prelude

import GHC.Generics
import Data.Aeson
import Data.Text

data PtgRequest = RequestExec { content :: Text }
                  | RequestExecStream { content :: Text }
                  | RequestLoadMessages
                  | RequestOpenDoc { identifier :: Text }
                  | RequestOpenSource { identifier :: Text }
  deriving (Show, Generic)

instance ToJSON PtgRequest
instance FromJSON PtgRequest
