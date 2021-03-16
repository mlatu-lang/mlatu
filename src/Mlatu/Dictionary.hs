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
    insert,
    lookup,
    member,
    operatorMetadata,
    signatures,
    toList,
    typeNames,
    wordNames,
    printDictionary,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Mlatu.Entry (Entry)
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Category qualified as Category
import Mlatu.Informer (Informer (..))
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Literal (IntegerLiteral (IntegerLiteral))
import Mlatu.Name
  ( GeneralName (UnqualifiedName),
    Qualified (Qualified),
    Unqualified (Unqualified),
    isOperatorName,
    qualifierFromName,
  )
import Mlatu.Operator (Operator (Operator))
import Mlatu.Operator qualified as Operator
import Mlatu.Pretty (printInstantiated)
import Mlatu.Report qualified as Report
import Mlatu.Signature (Signature)
import Mlatu.Term qualified as Term
import Prettyprinter (Doc, vsep)
import Relude hiding (empty, fromList, toList)
import Optics

-- | A key-value store mapping an 'Instantiated' name to a dictionary 'Entry'.
newtype Dictionary = Dictionary
  { _entries :: HashMap Instantiated Entry
  }
  deriving (Show)

makeLenses ''Dictionary

empty :: Dictionary
empty =
  Dictionary
    { _entries = HashMap.empty
    }

-- | Directly inserts into the dictionary. This is somewhat unsafe, as it can
-- lead to an invalid dictionary state.
insert :: Instantiated -> Entry -> Dictionary -> Dictionary
insert name entry = over entries (HashMap.insert name entry)

lookup :: Instantiated -> Dictionary -> Maybe Entry
lookup name dictionary = HashMap.lookup name (view entries dictionary)
{-# INLINEABLE lookup #-}

-- | Whether a name is present in the dictionary.
member :: Instantiated -> Dictionary -> Bool
member = (. view entries) . HashMap.member

-- | Compiles all operator metadata for infix desugaring.
operatorMetadata ::
  (Informer m) => Dictionary -> m (HashMap Qualified Operator)
operatorMetadata dictionary =
  HashMap.fromList
    <$> mapM
      getMetadata
      (filter isOperatorName $ wordNames dictionary)
  where
    getMetadata :: (Informer m) => Qualified -> m (Qualified, Operator)
    getMetadata name =
      let key = Qualified (qualifierFromName name) (Unqualified "operator")
       in case lookup (Instantiated key []) dictionary of
            -- TODO: Report invalid metadata.
            -- TODO: Avoid redundant decomposition.
            Just (Entry.Metadata _ term)
              -- Just associativity.
              | [Term.Word _ _ (UnqualifiedName (Unqualified assoc)) _ _] <-
                  Term.decompose term,
                Just associativity <- associativityFromName assoc ->
                yield associativity defaultPrecedence
              -- Just precedence.
              | [Term.Push _ (Term.Integer (IntegerLiteral prec _)) _] <-
                  Term.decompose term,
                validPrecedence prec ->
                yield defaultAssociativity $
                  Operator.Precedence $ fromInteger prec
              -- Associativity and precedence.
              | [ Term.Word _ _ (UnqualifiedName (Unqualified assoc)) _ _,
                  Term.Push _ (Term.Integer (IntegerLiteral prec _)) _
                  ] <-
                  Term.decompose term,
                Just associativity <- associativityFromName assoc,
                validPrecedence prec ->
                yield associativity $
                  Operator.Precedence $ fromInteger prec
              | otherwise -> do
                report $
                  Report.makeWarning $
                    Report.InvalidOperatorMetadata
                      (Term.origin term)
                      name
                      term

                yield defaultAssociativity defaultPrecedence
            _noMetadata -> yield defaultAssociativity defaultPrecedence
      where
        associativityFromName "left" = Just Operator.Leftward
        associativityFromName "right" = Just Operator.Rightward
        associativityFromName _ = Nothing

        validPrecedence = liftA2 (&&) (>= 0) (<= 9)

        defaultPrecedence = Operator.Precedence 6
        defaultAssociativity = Operator.Nonassociative

        yield associativity precedence =
          return
            ( name,
              Operator
                { Operator.associativity = associativity,
                  Operator.name = name,
                  Operator.precedence = precedence
                }
            )

-- | All type signatures (for words or traits) in the dictionary.
signatures :: Dictionary -> [(Qualified, Signature)]
signatures = mapMaybe getSignature . toList
  where
    getSignature :: (Instantiated, Entry) -> Maybe (Qualified, Signature)
    getSignature (Instantiated name [], Entry.Word _ _ _ _ (Just signature) _) =
      Just (name, signature)
    getSignature (Instantiated name [], Entry.Trait _ signature) =
      Just (name, signature)
    getSignature _ = Nothing

toList :: Dictionary -> [(Instantiated, Entry)]
toList = HashMap.toList . view entries

-- | All type names (for data types or permissions) in the dictionary.
typeNames :: Dictionary -> [Qualified]
typeNames = mapMaybe typeName . toList
  where
    typeName (Instantiated name _, Entry.Word Category.Permission _ _ _ _ _) =
      Just name
    typeName (Instantiated name _, Entry.Type {}) = Just name
    typeName _ = Nothing

-- | All word names (for words or traits) in the dictionary.
wordNames :: Dictionary -> [Qualified]
wordNames = mapMaybe wordName . toList
  where
    wordName (Instantiated name [], Entry.Word {}) = Just name
    -- TODO: Figure out how to get mangled names out of this...
    wordName (Instantiated name _, Entry.Trait {}) = Just name
    wordName _ = Nothing

printDictionary :: Dictionary -> Doc a
printDictionary = vsep . map printInstantiated . HashMap.keys . view entries
