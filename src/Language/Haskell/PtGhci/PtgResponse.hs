
module Language.Haskell.PtGhci.PtgResponse where

import Language.Haskell.PtGhci.Prelude

import GHC.Generics
import Data.Aeson
import Data.Text
import Language.Haskell.Ghcid.Types
import Language.Haskell.PtGhci.Orphans

data PtgResponse = ExecCaptureResponse
                      { success :: Bool
                      , content :: Text }
                   | ExecStreamResponse
                      { success :: Bool
                      , errorMessage :: Maybe Text
                      , syncVal :: Int }
                   | LoadMessagesResponse
                      { success :: Bool
                      , messages :: [Load] }
                   | CompletionResponse
                      { success :: Bool
                      , startChars :: Text
                      , candidates :: [Text] }
  deriving (Show, Generic)

instance ToJSON PtgResponse

