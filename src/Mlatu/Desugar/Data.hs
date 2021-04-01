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
import Mlatu.Definition (ConstructorDefinition (..))
import Mlatu.Definition qualified as Definition
import Mlatu.Entry.Parameter (Parameter (Parameter))
import Mlatu.Fragment (Fragment)
import Mlatu.Fragment qualified as Fragment
import Mlatu.Name (ConstructorIndex (..), GeneralName (..), Qualified (..))
import Mlatu.Signature qualified as Signature
import Mlatu.TypeDefinition (TypeDefinition)
import Mlatu.TypeDefinition qualified as TypeDefinition
import Optics
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
desugar fragment = over Fragment.constructorDefinitions (\defs -> defs ++ concatMap desugarTypeDefinition (view Fragment.types fragment)) fragment

desugarTypeDefinition :: TypeDefinition -> [ConstructorDefinition ()]
desugarTypeDefinition definition =
  zipWith (desugarConstructor definition) [0 ..] $
    view TypeDefinition.constructors definition

desugarConstructor :: TypeDefinition -> Int -> DataConstructor -> ConstructorDefinition ()
desugarConstructor definition index constructor =
  ConstructorDefinition
    { Definition._constructorBody = (ConstructorIndex index, length $ view DataConstructor.fields constructor),
      Definition._constructorName =
        Qualified qualifier $
          view DataConstructor.name constructor,
      Definition._constructorOrigin = origin,
      Definition._constructorParent = view TypeDefinition.name definition,
      Definition._constructorSignature = constructorSignature
    }
  where
    resultSignature =
      foldl'
        (\a b -> Signature.Application a b origin)
        ( Signature.Variable (QualifiedName $ view TypeDefinition.name definition) $
            view TypeDefinition.origin definition
        )
        $ ( \(Parameter parameterOrigin parameter _kind _) ->
              Signature.Variable (UnqualifiedName parameter) parameterOrigin
          )
          <$> view TypeDefinition.parameters definition
    constructorSignature =
      Signature.Quantified
        (view TypeDefinition.parameters definition)
        ( Signature.Function
            (view DataConstructor.fields constructor)
            [resultSignature]
            origin
        )
        origin
    origin = view DataConstructor.origin constructor
    qualifier = qualifierName $ view TypeDefinition.name definition
