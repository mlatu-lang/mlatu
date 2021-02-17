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
    compile,
    runMlatu,
    tokenize,
    compileCommon,
  )
where

import Data.FileEmbed (embedDir)
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Enter qualified as Enter
import Mlatu.Monad (K, runMlatu)
import Mlatu.Name (GeneralName, Qualified)
import Mlatu.Tokenize (tokenize)
import Relude

common :: [(FilePath, ByteString)]
common = $(embedDir "./common")

commonPaths :: [FilePath]
commonPaths = map fst common

commonSources :: [Text]
commonSources = map (decodeUtf8 . snd) common

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
  K Dictionary
compile mainPermissions mainName paths mDict = do
  -- Source files must be encoded in UTF-8.

  sources <- liftIO $ mapM (fmap decodeUtf8 . readFileBS) paths
  parsed <-
    mconcat
      <$> zipWithM
        (Enter.fragmentFromSource mainPermissions mainName 1)
        paths
        sources
  -- dictionary <-
  Enter.fragment parsed (fromMaybe Dictionary.empty mDict)

compileCommon :: [GeneralName] -> Maybe Qualified -> K Dictionary
compileCommon mainPermissions mainName = do
  parsed <-
    mconcat
      <$> zipWithM
        (Enter.fragmentFromSource mainPermissions mainName 1)
        commonPaths
        commonSources
  -- dictionary <-
  Enter.fragment parsed Dictionary.empty
