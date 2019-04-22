module Language.Haskell.PtGhci.WebBrowser where

import Language.Haskell.PtGhci.Prelude
import Language.Haskell.PtGhci.Env
import System.Exit (ExitCode(..))
import System.Process (rawSystem)
import Data.Text (unpack)
import qualified Web.Browser as WB

openBrowser :: Env -> Text -> IO Bool
openBrowser env url = do
  case env ^. config . webBrowser of
    Nothing -> liftIO $ WB.openBrowser (unpack url)
    Just browser -> liftIO $ exitCodeToBool `fmap` rawSystem executable argv
      where (executable, argv) = ("sh", ["-c", unpack browser++" \"$0\" 2>&1 > /dev/null",
                                         unpack url])
            exitCodeToBool ExitSuccess     = True
            exitCodeToBool (ExitFailure _) = False
