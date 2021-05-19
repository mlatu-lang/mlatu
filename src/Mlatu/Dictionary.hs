{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Dictionary
-- Description : Program database
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Dictionary
  ( Dictionary (..),
    empty,
    signatures,
    typeNames,
    wordNames,
    lookupWord,
    lookupType,
    lookupTrait,
    lookupMetadata,
    wordEntries,
    insertWord,
    insertTrait,
    insertType,
    insertMetadata,
  )
where

import Data.Map.Strict qualified as Map
import Mlatu.Entry (MetadataEntry, TraitEntry, TypeEntry, WordEntry)
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Category qualified as Category
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Instantiated qualified as Instantiated
import Mlatu.Name
  ( Qualified,
  )
import Mlatu.Pretty qualified as Pretty
import Mlatu.Signature (Signature)
import Optics
import Prettyprinter (Doc, vsep)
import Relude hiding (empty, fromList, toList)

-- | A key-value store mapping an 'Instantiated' name to a dictionary 'Entry'.
data Dictionary = Dictionary
  { _wordEntries :: Map Instantiated WordEntry,
    _typeEntries :: Map Instantiated TypeEntry,
    _metadataEntries :: Map Instantiated MetadataEntry,
    _traitEntries :: Map Instantiated TraitEntry
  }
  deriving (Show)

makeLenses ''Dictionary

empty :: Dictionary
empty =
  Dictionary
    { _wordEntries = Map.empty,
      _typeEntries = Map.empty,
      _metadataEntries = Map.empty,
      _traitEntries = Map.empty
    }

insert :: ((Map Instantiated a -> Map Instantiated a) -> Dictionary -> Dictionary) -> Instantiated -> a -> Dictionary -> Dictionary
insert o name entry = o (Map.insert name entry)

insertWord :: Instantiated -> WordEntry -> Dictionary -> Dictionary
insertWord = insert $ over wordEntries

insertMetadata :: Instantiated -> MetadataEntry -> Dictionary -> Dictionary
insertMetadata = insert $ over metadataEntries

insertType :: Instantiated -> TypeEntry -> Dictionary -> Dictionary
insertType = insert $ over typeEntries

insertTrait :: Instantiated -> TraitEntry -> Dictionary -> Dictionary
insertTrait = insert $ over traitEntries

lookupWord :: Instantiated -> Dictionary -> Maybe WordEntry
lookupWord = lookup $ view wordEntries

lookupType :: Instantiated -> Dictionary -> Maybe TypeEntry
lookupType = lookup $ view typeEntries

lookupTrait :: Instantiated -> Dictionary -> Maybe TraitEntry
lookupTrait = lookup $ view traitEntries

lookupMetadata :: Instantiated -> Dictionary -> Maybe MetadataEntry
lookupMetadata = lookup $ view metadataEntries

lookup :: (Dictionary -> Map Instantiated a) -> Instantiated -> Dictionary -> Maybe a
lookup o name dictionary = Map.lookup name (o dictionary)

-- | All type signatures (for words or traits) in the dictionary.
signatures :: Dictionary -> [(Qualified, Signature)]
signatures dict =
  mapMaybe getWordSignature (Map.toList (view wordEntries dict))
    ++ mapMaybe getTraitSignature (Map.toList (view traitEntries dict))
  where
    getWordSignature (Instantiated name [], Entry.WordEntry _ _ _ _ (Just signature) _) =
      Just (name, signature)
    getWordSignature _ = Nothing

    getTraitSignature (Instantiated name [], Entry.TraitEntry _ signature) =
      Just (name, signature)
    getTraitSignature _ = Nothing

-- | All type names (for data types or permissions) in the dictionary.
typeNames :: Dictionary -> [Qualified]
typeNames dict =
  mapMaybe typeWordName (Map.toList (view wordEntries dict))
    ++ (Instantiated.name <$> Map.keys (view typeEntries dict))
  where
    typeWordName (Instantiated name _, Entry.WordEntry Category.Permission _ _ _ _ _) =
      Just name
    typeWordName _ = Nothing

-- | All word names (for words or traits) in the dictionary.
wordNames :: Dictionary -> [Qualified]
wordNames dict =
  Instantiated.name
    <$> (Map.keys (view wordEntries dict) ++ Map.keys (view traitEntries dict))
