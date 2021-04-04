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
    entries,
    empty,
    difference,
    insert,
    lookup,
    member,
    signatures,
    toList,
    typeNames,
    wordNames,
    printDictionary,
  )
where

import Data.Map.Strict qualified as Map
import Mlatu.Entry (Entry)
import Mlatu.Entry qualified as Entry
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Name
  ( GeneralName (UnqualifiedName),
    Qualified (Qualified),
    Unqualified (Unqualified),
    isOperatorName,
    qualifierFromName,
  )
import Mlatu.Pretty (printInstantiated)
import Mlatu.Signature (Signature)
import Optics
import Prettyprinter (Doc, vsep)
import Relude hiding (empty, fromList, toList)

-- | A key-value store mapping an 'Instantiated' name to a dictionary 'Entry'.
newtype Dictionary = Dictionary
  { _entries :: Map Instantiated Entry
  }
  deriving (Show)

makeLenses ''Dictionary

empty :: Dictionary
empty =
  Dictionary
    { _entries = Map.empty
    }

difference :: Dictionary -> Dictionary -> Dictionary
difference a b = Dictionary {_entries = Map.difference (view entries a) (view entries b)}

-- | Directly inserts into the dictionary. This is somewhat unsafe, as it can
-- lead to an invalid dictionary state.
insert :: Instantiated -> Entry -> Dictionary -> Dictionary
insert name entry = over entries (Map.insert name entry)

lookup :: Instantiated -> Dictionary -> Maybe Entry
lookup name dictionary = Map.lookup name (view entries dictionary)
{-# INLINEABLE lookup #-}

-- | Whether a name is present in the dictionary.
member :: Instantiated -> Dictionary -> Bool
member = (. view entries) . Map.member

-- | All type signatures (for words or traits) in the dictionary.
signatures :: Dictionary -> [(Qualified, Signature)]
signatures = mapMaybe getSignature . toList
  where
    getSignature :: (Instantiated, Entry) -> Maybe (Qualified, Signature)
    getSignature (Instantiated name [], Entry.Word _ _ (Just signature) _) =
      Just (name, signature)
    getSignature (Instantiated name [], Entry.Constructor _ _ signature _) =
      Just (name, signature)
    getSignature (Instantiated name [], Entry.ClassMethod _ signature) =
      Just (name, signature)
    getSignature _ = Nothing

toList :: Dictionary -> [(Instantiated, Entry)]
toList = Map.toList . view entries

-- | All type names (for data types or permissions) in the dictionary.
typeNames :: Dictionary -> [Qualified]
typeNames = mapMaybe typeName . toList
  where
    typeName (Instantiated name _, Entry.Type {}) = Just name
    typeName _ = Nothing

-- | All word names (for words or type classes) in the dictionary.
wordNames :: Dictionary -> [Qualified]
wordNames = mapMaybe wordName . toList
  where
    wordName (Instantiated name [], Entry.Word {}) = Just name
    -- TODO: Figure out how to get mangled names out of this...
    wordName (Instantiated name _, Entry.ClassMethod {}) = Just name
    wordName (Instantiated name _, Entry.Constructor {}) = Just name
    wordName _ = Nothing

printDictionary :: Dictionary -> Doc a
printDictionary = vsep . fmap printInstantiated . sort . Map.keys . view entries
