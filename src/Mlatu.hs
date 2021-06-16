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
    compilePrelude,
    compileWithPrelude,
  )
where

import Data.FileEmbed (embedDir)
import Mlatu.Base.Name (GeneralName, Qualified)
import Mlatu.Front.Tokenize (tokenize)
import Mlatu.Informer (M, runMlatu)
import Mlatu.Middle.Dictionary (Dictionary)
import Mlatu.Middle.Dictionary qualified as Dictionary
import Mlatu.Middle.Enter qualified as Enter

prelude :: [(FilePath, ByteString)]
prelude = $(embedDir "./std")

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

compileWithPrelude :: [GeneralName] -> Maybe Qualified -> [FilePath] -> M Dictionary
compileWithPrelude mainPermissions mainName paths = do
  commonDictionary <- compilePrelude mainPermissions mainName
  compile mainPermissions mainName paths (Just commonDictionary)

compilePrelude :: [GeneralName] -> Maybe Qualified -> M Dictionary
compilePrelude mainPermissions mainName = do
  parsed <-
    mconcat
      <$> zipWithM
        (Enter.fragmentFromSource mainPermissions mainName 1)
        preludePaths
        (decodeUtf8 <$> preludeSources)
  Enter.fragment parsed Dictionary.empty
  where
    (preludePaths, preludeSources) = unzip prelude
