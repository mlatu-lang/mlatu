-- |
-- Module      : Mlatu.Dictionary
-- Description : Program database
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Dictionary
  ( Dictionary,
    empty,
    insert,
    lookup,
    member,
    operatorMetadata,
    signatures,
    toList,
    typeNames,
    wordNames,
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
import Mlatu.Report qualified as Report
import Mlatu.Signature (Signature)
import Mlatu.Term qualified as Term
import Relude hiding (empty, fromList, toList)
import Text.PrettyPrint.HughesPJClass (Pretty (..))

-- | A key-value store mapping an 'Instantiated' name to a dictionary 'Entry'.
newtype Dictionary = Dictionary
  { entries :: HashMap Instantiated Entry
  }
  deriving (Show)

instance Pretty Dictionary where
  pPrint (Dictionary entries) = pPrint (HashMap.keys entries)

empty :: Dictionary
empty =
  Dictionary
    { entries = HashMap.empty
    }

-- | Directly inserts into the dictionary. This is somewhat unsafe, as it can
-- lead to an invalid dictionary state.
insert :: Instantiated -> Entry -> Dictionary -> Dictionary
insert name entry dictionary =
  dictionary
    { entries = HashMap.insert name entry $ entries dictionary
    }

lookup :: Instantiated -> Dictionary -> Maybe Entry
lookup = (. entries) . HashMap.lookup

-- | Whether a name is present in the dictionary.
member :: Instantiated -> Dictionary -> Bool
member = (. entries) . HashMap.member

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
       in case HashMap.lookup (Instantiated key []) $ entries dictionary of
            -- TODO: Report invalid metadata.
            -- TODO: Avoid redundant decomposition.
            Just (Entry.Metadata _ term)
              -- Just associativity.
              | [Term.Word _ _ (UnqualifiedName (Unqualified assoc)) _ _] <-
                  Term.decompose term,
                Just associativity <- associativityFromName assoc ->
                yield associativity defaultPrecedence
              -- Just precedence.
              | [Term.Push _ (Term.Integer (IntegerLiteral prec _base _bits)) _] <-
                  Term.decompose term,
                validPrecedence prec ->
                yield defaultAssociativity $
                  Operator.Precedence $ fromInteger prec
              -- Associativity and precedence.
              | [ Term.Word _ _ (UnqualifiedName (Unqualified assoc)) _ _,
                  Term.Push _ (Term.Integer (IntegerLiteral prec _base _bits)) _
                  ] <-
                  Term.decompose term,
                Just associativity <- associativityFromName assoc,
                validPrecedence prec ->
                yield associativity $
                  Operator.Precedence $ fromInteger prec
              | otherwise -> do
                report $
                  Report.makeError $
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
signatures = mapMaybe getSignature . HashMap.toList . entries
  where
    getSignature :: (Instantiated, Entry) -> Maybe (Qualified, Signature)
    getSignature (Instantiated name [], Entry.Word _ _ _ _ (Just signature) _) =
      Just (name, signature)
    getSignature (Instantiated name [], Entry.Trait _ signature) =
      Just (name, signature)
    getSignature _ = Nothing

toList :: Dictionary -> [(Instantiated, Entry)]
toList = HashMap.toList . entries

-- | All type names (for data types or permissions) in the dictionary.
typeNames :: Dictionary -> [Qualified]
typeNames = mapMaybe typeName . HashMap.toList . entries
  where
    typeName (Instantiated name _, Entry.Word Category.Permission _ _ _ _ _) =
      Just name
    typeName (Instantiated name _, Entry.Type {}) = Just name
    typeName _ = Nothing

-- | All word names (for words or traits) in the dictionary.
wordNames :: Dictionary -> [Qualified]
wordNames = mapMaybe wordName . HashMap.toList . entries
  where
    wordName (Instantiated name [], Entry.Word {}) = Just name
    -- TODO: Figure out how to get mangled names out of this...
    wordName (Instantiated name _, Entry.Trait {}) = Just name
    wordName _ = Nothing
