module Language.Haskell.PtGhci.Prelude
  ( module P,
    module MicroLens,
    module Language.Haskell.PtGhci.Prelude,
    String,
    StringConv,
    toS,
    -- , unwords
  )
where

-- import Prelude (unwords)
import Control.Exception (throw)
import qualified Data.Text as T
import GHC.Base (String)
import Lens.Micro as MicroLens
import Protolude as P hiding (to, toS, trace, (%))
import qualified Protolude as P
import Protolude.Conv (StringConv, toS)
import Text.Printf

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (a : as) = Just as

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) (x : ls) 0 = Just x
(!?) (x : ls) i = ls !? (i - 1)

-- tryP :: (Exception e) => Ptg a -> Ptg (Either e a)
-- tryP (Ptg action) = do
--   r <- ask
--   runReaderT r action
--   return $ Ptg $
