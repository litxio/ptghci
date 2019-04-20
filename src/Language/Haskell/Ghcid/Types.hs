{-# LANGUAGE DeriveDataTypeable #-}

-- Copyright Neil Mitchell 2014-2018.
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
-- 
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
-- 
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
-- 
--     * Neither the name of Neil Mitchell nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


-- | The types types that we use in Ghcid
module Language.Haskell.Ghcid.Types(
    GhciError(..),
    Stream(..),
    Load(..), Severity(..),
    isMessage, isLoading, isLoadConfig
    ) where

import Prelude
import Data.Data
import Control.Exception.Base (Exception)

-- | GHCi shut down
data GhciError = UnexpectedExit String String
    deriving (Show,Eq,Ord,Typeable,Data)

-- | Make GhciError an exception
instance Exception GhciError

-- | The stream Ghci is talking over.
data Stream = Stdout | Stderr
    deriving (Show,Eq,Ord,Bounded,Enum,Read,Typeable,Data)

-- | Severity of messages
data Severity = Warning | Error
    deriving (Show,Eq,Ord,Bounded,Enum,Read,Typeable,Data)

-- | Load messages
data Load
    = -- | A module/file was being loaded.
      Loading
        {loadModule :: String -- ^ The module that was being loaded, @Foo.Bar@.
        ,loadFile :: FilePath -- ^ The file that was being loaded, @Foo/Bar.hs@.
        }
    | -- | An error/warning was emitted.
      Message
        {loadSeverity :: Severity -- ^ The severity of the message, either 'Warning' or 'Error'.
        ,loadFile :: FilePath -- ^ The file the error relates to, @Foo/Bar.hs@.
        ,loadFilePos :: (Int,Int) -- ^ The position in the file, @(line,col)@, both 1-based. Uses @(0,0)@ for no position information.
        ,loadFilePosEnd :: (Int, Int) -- ^ The end position in the file, @(line,col)@, both 1-based. If not present will be the same as 'loadFilePos'.
        ,loadMessage :: [String] -- ^ The message, split into separate lines, may contain ANSI Escape codes.
        }
    | -- | A config file was loaded, usually a .ghci file (GHC 8.2 and above only)
      LoadConfig
        {loadFile :: FilePath -- ^ The file that was being loaded, @.ghci@.
        }
    deriving (Show, Eq, Ord)

-- | Is a 'Load' a 'Message'?
isMessage :: Load -> Bool
isMessage Message{} = True
isMessage _ = False

-- | Is a 'Load' a 'Loading'?
isLoading :: Load -> Bool
isLoading Loading{} = True
isLoading _ = False

-- | Is a 'Load' a 'LoadConfig'?
isLoadConfig :: Load -> Bool
isLoadConfig LoadConfig{} = True
isLoadConfig _ = False
