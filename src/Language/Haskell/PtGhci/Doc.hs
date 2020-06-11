{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Language.Haskell.PtGhci.Doc where

import Language.Haskell.PtGhci.Prelude hiding (option, try, many, some, replace, lines)

import Debug.Trace (trace)
import Data.Void
import Data.Maybe (catMaybes, listToMaybe)
import Data.List hiding (take, takeWhile)
import Text.Regex.PCRE.Heavy
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text, pack, unpack)
import System.Process
import Data.Char (isUpper)
import Text.Printf
import System.IO.Error (userError)
import Control.Applicative hiding (many, some)
import System.FilePath
import System.Directory
import Control.Exception (Exception, throw)
import qualified Control.Exception as Exception
import qualified Data.HashMap.Strict as HM
import Text.Megaparsec hiding (match)
import Text.Megaparsec.Char
import Control.Monad.Combinators
import Language.Haskell.PtGhci.Ghci
import Language.Haskell.PtGhci.WebBrowser
import Language.Haskell.PtGhci.Env

data DocException = DocNotFoundException Text
                  | BadIdentifier Text
                  deriving Show
instance Exception DocException

showDocException :: DocException -> Text
showDocException (DocNotFoundException s) = s
showDocException (BadIdentifier s) = s

data Module = Module 
  { _modPackage :: Text
  , _modName :: Text } deriving (Eq, Show)

data VersionQualifiedPackage = VQP 
  { _vqpPackage :: Text
  , _vqpVersion :: Text } deriving (Eq, Show)

-- | Find the local HTML file for the given identifier in scope in ghci,
-- and open it in the Web browser.
openDocForIdentifier :: Env -> Ghci -> Text -> IO Text
openDocForIdentifier env ghci name = do
  url <- pack <$> findDocForIdentifier env ghci name
  T.putStrLn $ "Opening " <> url
  openBrowser env url
  return url
    -- FIXME this needs to be more sophsticated to handle data constructors
    -- and probably other things.


findDocForIdentifier :: Env -> Ghci -> Text -> IO FilePath
findDocForIdentifier env ghci name = do
  mod <- findModuleForIdentifier ghci name
  possibleBasePaths <- docBasePathCandidates mod
  let possiblePaths :: [FilePath]
      possiblePaths = do -- List monad, yay
        basePath <- possibleBasePaths
        fileName <- docFileNameCandidates mod
        return $ basePath </> unpack (_modPackage mod) </> fileName
  res <- Exception.try $ foldr1 (<|>) (checkExists <$> possiblePaths)
         :: IO (Either IOException FilePath)
  case res of
    Right fullPath -> return $ "file://"++fullPath++anchor
    Left _ -> throw $ DocNotFoundException $ pack
                  $ "Couldn't find documentation file in any of the following locations: "
                    ++ show possiblePaths
  where
    anchor :: FilePath
    anchor
      | isType baseName = unpack $ "#t:"<>baseName
      | otherwise       = unpack $ "#v:"<>baseName
    baseName = last (T.split (=='.') name)


openDocSourceForIdentifier :: Env -> Ghci -> Text -> IO Text
openDocSourceForIdentifier env ghci name = do
  url <- pack <$> findDocSourceForIdentifier env ghci name
  T.putStrLn $ "Opening " <> url
  openBrowser env url
  return url
    -- FIXME this needs to be more sophsticated to handle data constructors
    -- and probably other things.


findDocSourceForIdentifier :: Env -> Ghci -> Text -> IO FilePath
findDocSourceForIdentifier env ghci name = do
  mod <- findModuleForIdentifier ghci name
  possibleBasePaths <- docBasePathCandidates mod
  let possiblePaths :: [FilePath]
      possiblePaths = do -- List monad, yay
        basePath <- possibleBasePaths
        fileName <- docFileNameCandidates mod
        return $ basePath </> unpack (_modPackage mod) </> "src" </> fileName
  res <- Exception.try $ foldr1 (<|>) (checkExists <$> possiblePaths)
         :: IO (Either IOException FilePath)
  case res of
    Right fullPath -> return $ "file://"++fullPath++anchor
    Left _ -> throw $ DocNotFoundException $ pack
                  $ "Couldn't find source code in any of the following locations: "
                    ++ show possiblePaths
  where
    anchor = unpack $ "#" <> baseName
    baseName = last (T.split (=='.') name)


docBasePathCandidates :: Module -> IO [FilePath]
docBasePathCandidates Module{..} = do
  stackPaths <- getStackPaths
  return $ catMaybes [ HM.lookup "local-doc-root" stackPaths
                     , HM.lookup "snapshot-doc-root" stackPaths]
                  
docFileNameCandidates :: Module -> [FilePath]
docFileNameCandidates Module{..} = [ unpack _modName <> ".html"
                                   , replace '.' '-' (unpack _modName) <> ".html"]


checkExists :: FilePath -> IO FilePath
checkExists path = do
  putStrLn $ "Checking " ++ path
  exists <- doesPathExist path
  if exists then return path
            -- We throw IOError here so we can use IO's <|> instance
            else throw $ userError $ printf "%s doesn't exist" path

replace a b s = [if x == a then b else x | x <- s]
-- | Find the local HTML file for the given identifier in scope in ghci.
-- This works by running @:info name@.  If :info doesn't provide the package,
-- we try to find it by calling findPackageInGhcPkg.
findDocBasePath :: Module -> IO FilePath
findDocBasePath Module{..} = do
  paths <- getStackPaths
  res <- Exception.try (maybeFindDoc "local-doc-root" paths _modPackage _modName
              <|> maybeFindDoc "snapshot-doc-root" paths _modPackage _modName)
                :: IO (Either DocException FilePath)
  case res of
    Right path -> return path
    Left ex -> throw $ DocNotFoundException
                      $ pack $ printf "Can't find doc for %s-%s" _modPackage _modName
  where
    -- We throw IOError here so we can use IO's <|> instance
    maybeFindDoc :: Text
                 -> HM.HashMap Text FilePath
                 -> Text
                 -> Text
                 -> IO FilePath
    maybeFindDoc key stackPaths pkg modName =
      case HM.lookup key stackPaths of
        Nothing -> throw $ userError $ printf "Couldn't get %s from stack" key
        Just path -> do
          exists <- doesPathExist path
          if exists then return path
                    else throw $ userError $ printf "%s doesn't exist" path


-- | Is the given identifier a type?  We just check whether the first char
-- is uppercase.  FIXME this doesn't work for data constructors and probably
-- other things.
isType :: Text -> Bool
isType = isUpper . T.head

getStackPaths :: IO (HM.HashMap Text FilePath)
getStackPaths = do
  allDirs <- lines <$> readProcess "stack" ["path"] ""
  return $ HM.fromList $ catMaybes $ splitLine <$> allDirs
  where
    splitLine l = case scan [re|(.+): (.+)|] (pack l) of
                    [(_, [key, path])] -> Just (key, unpack path)
                    _ -> Nothing

-- | Get the module name (and package, if shown) for an identifier by calling
-- GHCi's :info
-- There are a few possibilities we need to deal with:
--  - The package may or may not be shown, depending on whether the module
--    is imported.
findModuleForIdentifier :: Ghci -> Text -> IO Module
findModuleForIdentifier ghci [] = throw $ BadIdentifier "Empty identifier"
findModuleForIdentifier ghci name = do
  infoLines <- fst <$> (execCapture ghci $ ":info " ++ unpack name)
  let firstMatch = find ("-- Defined in" `isInfixOf`) infoLines
  case firstMatch >>= eitherToMaybe . parse moduleParser "" . pack of
    Just (Just pkg, modName) -> return $ Module pkg modName
    Just (Nothing, modName) -> (`Module` modName) <$> findPackageInGhcPkg modName
    Nothing -> throw $ DocNotFoundException
                  $ pack $ printf "Couldn't get information for identifier '%s'" name
  where
    safeHead [] = Nothing
    safeHead (x:_) = Just x
    eitherToMaybe (Left _) = Nothing
    eitherToMaybe (Right v) = Just v

findPackageInGhcPkg :: Text -> IO Text
findPackageInGhcPkg modName = do
  T.putStrLn $ "Finding package for " <> modName
  ghcPkgLines <- lines <$> readProcess "stack" ["exec", "--", "ghc-pkg",
                                                "find-module", unpack modName,
                                                "--simple-output"] ""
  getPkg $ pack <$> ghcPkgLines
  where
    getPkg :: [Text] -> IO Text
    getPkg [] = throw $ DocNotFoundException "Couldn't find package (tried ghc-pkg)"
    getPkg (l:ls) = case parse packageParser "" l of
                      Right pkg -> return pkg
                      Left _ -> getPkg ls

stackOpenDoc :: Text -> IO ()
stackOpenDoc packageName = callProcess "stack" ["haddock", "--open",
                                                unpack packageName]

getLoadedPackages :: Ghci -> IO [VersionQualifiedPackage]
getLoadedPackages ghci = do
  showResult <- fst <$> execCapture ghci ":show packages"
  let matches = scan r . pack <$> showResult
  return $ catMaybes $ mkVQPackage <$> matches
  where
    r = [re|\s-package-id ([a-zA-Z0-9-_]+)-((\d+|\.)+)|]
    mkVQPackage [] = Nothing
    mkVQPackage [(_, [p1,p2])] = Just $ VQP p1 p2

-- extractModule stripClass s = buildMod $ match r s []
--   where
--     r = [re|\s* -- Defined in (‘|')(([\w\d_]+-)+([\d\.]+:))?(([A-Z][\w\d]*)(’|')

type Parser = Parsec Void Text

maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

type PackageStr = Text
type ModuleStr = Text

packageParser :: Parser PackageStr
packageParser = do
  -- Need  the try here because of the potential overlap with the first version
  -- part
  pkgName <- T.intercalate "-" <$> (try wordWithAlpha `endBy1` char '-')
  
  version <- many $ digitChar <|> char '.'

  return $ T.concat [pkgName, "-", pack version]

wordWithAlpha :: Parser Text
wordWithAlpha = do  -- With at least one alphabetical char
  front <- many $ digitChar <|> char '_'
  l <- letterChar
  rest <- many $ alphaNumChar <|> char '_'
  return $ T.concat [pack front, T.singleton l, pack rest]

capitalizedWord :: Parser Text
capitalizedWord = do
  caps <- some upperChar
  rest <- many $ alphaNumChar <|> char '_'
  return $ pack caps <> pack rest


moduleParser :: Parser (Maybe PackageStr, ModuleStr)
moduleParser = do
  manyTill anySingle (string "-- Defined in ") >> punctuationChar
  pkgName <- option Nothing $ Just <$> try (packageParser <* char ':')
  dotSep <- capitalizedWord `sepBy1` char '.'
  let moduleName = T.intercalate "." dotSep
  punctuationChar
  return (pkgName, moduleName)

