{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Dictionary
-- Description : Program database
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Middle.Dictionary
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
import Mlatu.Base.Name
  ( Qualified,
  )
import Mlatu.Front.Definition (Category (..))
import Mlatu.Front.Signature (Signature)
import Mlatu.Middle.Entry (MetadataEntry, TraitEntry, TypeEntry, WordEntry)
import Mlatu.Middle.Entry qualified as Entry
import Mlatu.Middle.Instantiated (Instantiated (Instantiated))
import Mlatu.Middle.Instantiated qualified as Instantiated
import Prelude hiding (empty)

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

insertEntry :: Optic' A_Lens is Dictionary (Map Instantiated a) -> Instantiated -> a -> Dictionary -> Dictionary
insertEntry l name entry = over l (Map.insert name entry)

insertWord :: Instantiated -> WordEntry -> Dictionary -> Dictionary
insertWord = insertEntry wordEntries

insertMetadata :: Instantiated -> MetadataEntry -> Dictionary -> Dictionary
insertMetadata = insertEntry metadataEntries

insertType :: Instantiated -> TypeEntry -> Dictionary -> Dictionary
insertType = insertEntry typeEntries

insertTrait :: Instantiated -> TraitEntry -> Dictionary -> Dictionary
insertTrait = insertEntry traitEntries

lookupWord :: Instantiated -> Dictionary -> Maybe WordEntry
lookupWord = lookupEntry wordEntries

lookupType :: Instantiated -> Dictionary -> Maybe TypeEntry
lookupType = lookupEntry typeEntries

lookupTrait :: Instantiated -> Dictionary -> Maybe TraitEntry
lookupTrait = lookupEntry traitEntries

lookupMetadata :: Instantiated -> Dictionary -> Maybe MetadataEntry
lookupMetadata = lookupEntry metadataEntries

lookupEntry :: Optic' A_Lens is Dictionary (Map Instantiated a) -> Instantiated -> Dictionary -> Maybe a
lookupEntry l name dictionary = Map.lookup name (view l dictionary)

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
    typeWordName (Instantiated name _, Entry.WordEntry PermissionWord _ _ _ _ _) =
      Just name
    typeWordName _ = Nothing

-- | All word names (for words or traits) in the dictionary.
wordNames :: Dictionary -> [Qualified]
wordNames dict =
  Instantiated.name
    <$> (Map.keys (view wordEntries dict) ++ Map.keys (view traitEntries dict))
