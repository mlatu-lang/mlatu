{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu
-- Description : Compiler pipeline
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu
  ( Enter.fragmentFromSource,
    Prelude (..),
    compile,
    runMlatu,
    tokenize,
    compilePrelude,
    compileWithPrelude,
  )
where

import Data.FileEmbed (embedDir, embedFile)
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Enter qualified as Enter
import Mlatu.Informer (M, runMlatu)
import Mlatu.Name (GeneralName, Qualified)
import Mlatu.Tokenize (tokenize)
import Relude

common :: [(FilePath, ByteString)]
common = $(embedDir "./std/common")

foundation :: (FilePath, ByteString)
foundation = ("./std/foundation.mlt", $(embedFile "./std/foundation.mlt"))

-- | This is a simple wrapper for the compiler pipeline. It adds a list of
-- program fragments to the dictionary from a list of source paths. At each
-- stage, errors and warnings (\"reports\") are accumulated, and reported to the
-- programmer at the next checkpoint; see "Mlatu.Monad" for details.
compile ::
  -- | List of permissions to grant to @main@.
  [GeneralName] ->
  -- | Override the default name of @main@.
  Maybe Qualified ->
  -- | List of source file paths.
  [FilePath] ->
  Maybe Dictionary ->
  -- | Resulting dictionary.
  M Dictionary
compile mainPermissions mainName paths mDict = do
  -- Source files must be encoded in UTF-8.

  sources <- liftIO $ traverse (fmap decodeUtf8 . readFileBS) paths
  parsed <-
    mconcat
      <$> zipWithM
        (Enter.fragmentFromSource mainPermissions mainName 1)
        paths
        sources
  -- dictionary <-
  Enter.fragment parsed (fromMaybe Dictionary.empty mDict)

compileWithPrelude :: Prelude -> [GeneralName] -> Maybe Qualified -> [FilePath] -> M Dictionary
compileWithPrelude prelude mainPermissions mainName paths = do
  commonDictionary <- compilePrelude prelude mainPermissions mainName
  compile mainPermissions mainName paths (Just commonDictionary)

compilePrelude :: Prelude -> [GeneralName] -> Maybe Qualified -> M Dictionary
compilePrelude prelude mainPermissions mainName = do
  parsed <-
    mconcat
      <$> zipWithM
        (Enter.fragmentFromSource mainPermissions mainName 1)
        preludePaths
        (decodeUtf8 <$> preludeSources)
  -- dictionary <-
  Enter.fragment parsed Dictionary.empty
  where
    (preludePaths, preludeSources) = case prelude of
      Foundation -> ([fst foundation], [snd foundation])
      Common -> (fst foundation : (fst <$> common), snd foundation : (snd <$> common))

data Prelude
  = Foundation
  | Common
