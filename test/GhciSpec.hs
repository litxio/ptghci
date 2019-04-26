module GhciSpec where

import Language.Haskell.PtGhci.Prelude
import Data.List (lines)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec
import Language.Haskell.PtGhci.Ghci
import System.IO.Silently

spec :: Spec
spec = do
  (ghci, loadMsgs) <- runIO $ startGhci "stack ghci" Nothing (\_ _ -> return ())

  describe "Ghci module" $ do
    it "executes and captures -- an expression value" $ do
      (out, err) <- execCapture ghci "120+3"
      out `shouldBe` ["123"]
      err `shouldBe` []

    it "executes and captures -- something printed" $ do
      (out, err) <- execCapture ghci "Prelude.print 234"
      out `shouldBe` ["234"]
      err `shouldBe` []

    --  TODO capturing doesn't seem to work
    -- it "executes and streams -- something printed" $ do
    --   (out, _) <- capture $ execStream ghci "print 234"
    --   lines out `shouldBe` ["234"]

