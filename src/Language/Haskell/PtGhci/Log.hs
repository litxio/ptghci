module Language.Haskell.PtGhci.Log where

import Language.Haskell.PtGhci.Prelude
import Language.Haskell.PtGhci.Env
import Language.Haskell.PtGhci.Monad


info :: StringConv a Text => Env -> a -> IO ()
info e t = when (e ^. config . verbosity >= Info) $ putText (toS t)
