{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.PtGhci.Env 
  ( module Language.Haskell.PtGhci.Config
  , module Language.Haskell.PtGhci.Env
  ) where
  
import Language.Haskell.PtGhci.Prelude
import GHC.Generics
import Lens.Micro.TH
import Data.Aeson
import Data.IORef
import qualified Data.Aeson.Types    as A (Options (..), Parser)
import Language.Haskell.PtGhci.Exception
import Language.Haskell.PtGhci.Config

data Env = Env
  { _config :: Config
  , _logHandle :: Maybe Handle
  , _lineCounter :: IORef Int
  }
makeLenses ''Env

mkEnv :: Config -> IO Env
mkEnv config = do
  lhandle <- case _logFile config of
               Nothing -> return Nothing
               Just path -> Just <$> openFile path WriteMode
  counter <- newIORef 0
  return $ Env config lhandle counter

