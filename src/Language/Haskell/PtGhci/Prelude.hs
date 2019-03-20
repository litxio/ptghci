
module Language.Haskell.PtGhci.Prelude 
  ( module P
  , module MicroLens
  , module Language.Haskell.PtGhci.Prelude 
  , String
) where 

import Protolude as P hiding ((%), to, trace)
import Lens.Micro as MicroLens
import qualified Protolude as P
import qualified Data.Text as T
import Text.Printf
import Control.Exception (throw)
import GHC.Base (String)

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (a:as) = Just as

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) (x:ls) 0 = Just x
(!?) (x:ls) i = ls !? (i-1)

-- tryP :: (Exception e) => Ptg a -> Ptg (Either e a)
-- tryP (Ptg action) = do
--   r <- ask
--   runReaderT r action
--   return $ Ptg $ 
