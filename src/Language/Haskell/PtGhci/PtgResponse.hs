
module Language.Haskell.PtGhci.PtgResponse where

import Language.Haskell.PtGhci.Prelude

import GHC.Generics
import Data.Aeson
import Data.Text
import Language.Haskell.Ghcid
import Language.Haskell.PtGhci.Orphans

data PtgResponse = ExecResponse
                      { success :: Bool
                      , content :: Text }
                   | LoadMessagesResponse
                      { success :: Bool
                      , messages :: [Load] }
  deriving (Show, Generic)

instance ToJSON PtgResponse

