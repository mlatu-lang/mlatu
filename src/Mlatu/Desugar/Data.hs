-- |
-- Module      : Mlatu.Desugar.Data
-- Description : Desugaring data type constructors
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Desugar.Data
  ( desugar,
  )
where

import Mlatu.DataConstructor (DataConstructor)
import Mlatu.DataConstructor qualified as DataConstructor
import Mlatu.Definition (Definition (Definition))
import Mlatu.Definition qualified as Definition
import Mlatu.Entry.Category qualified as Category
import Mlatu.Entry.Merge qualified as Merge
import Mlatu.Entry.Parameter (Parameter (Parameter))
import Mlatu.Entry.Parent qualified as Parent
import Mlatu.Fragment (Fragment)
import Mlatu.Fragment qualified as Fragment
import Mlatu.Name (ConstructorIndex (..), GeneralName (..), Qualified (..))
import Mlatu.Operator qualified as Operator
import Mlatu.Signature qualified as Signature
import Mlatu.Term (Term (..))
import Mlatu.TypeDefinition (TypeDefinition)
import Mlatu.TypeDefinition qualified as TypeDefinition
import Relude

-- | Desugars data type constructors into word definitions, e.g.:
--
-- > type Optional<T>:
-- >   case none
-- >   case some (T)
-- >
-- > // =>
-- >
-- > define none<T> (-> Optional<T>) { ... }
-- > define some<T> (T -> Optional<T>) { ... }
desugar :: Fragment () -> Fragment ()
desugar fragment =
  fragment
    { Fragment.definitions =
        Fragment.definitions fragment
          ++ concatMap desugarTypeDefinition (Fragment.types fragment)
    }

desugarTypeDefinition :: TypeDefinition -> [Definition ()]
desugarTypeDefinition definition =
  zipWith (desugarConstructor definition) [0 ..] $
    TypeDefinition.constructors definition

desugarConstructor :: TypeDefinition -> Int -> DataConstructor -> Definition ()
desugarConstructor definition index constructor =
  Definition
    { Definition.body =
        New
          ()
          (ConstructorIndex index)
          (length $ DataConstructor.fields constructor)
          $ DataConstructor.origin constructor,
      Definition.category = Category.Constructor,
      Definition.fixity = Operator.Postfix,
      Definition.inferSignature = False,
      Definition.merge = Merge.Deny,
      Definition.name =
        Qualified qualifier $
          DataConstructor.name constructor,
      Definition.origin = origin,
      Definition.parent =
        Just $
          Parent.Type $
            TypeDefinition.name definition,
      Definition.signature = constructorSignature
    }
  where
    resultSignature =
      foldl'
        (\a b -> Signature.Application a b origin)
        ( Signature.Variable (QualifiedName $ TypeDefinition.name definition) $
            TypeDefinition.origin definition
        )
        $ map
          ( \(Parameter parameterOrigin parameter _kind) ->
              Signature.Variable (UnqualifiedName parameter) parameterOrigin
          )
          $ TypeDefinition.parameters definition
    constructorSignature =
      Signature.Quantified
        (TypeDefinition.parameters definition)
        ( Signature.Function
            (DataConstructor.fields constructor)
            [resultSignature]
            []
            origin
        )
        origin
    origin = DataConstructor.origin constructor
    qualifier = qualifierName $ TypeDefinition.name definition
