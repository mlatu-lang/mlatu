-- |
-- Module      : Mlatu.Desugar.Data
-- Description : Desugaring data type constructors
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Front.DesugarTypes
  ( desugar,
  )
where

import Data.List (zipWith3)
import Mlatu.Base.Name (ConstructorIndex (..), GeneralName (..), Qualified (..), Unqualified (..))
import Mlatu.Base.Origin (Origin)
import Mlatu.Front.CodataDefinition (CodataDefinition)
import Mlatu.Front.CodataDefinition qualified as Codata
import Mlatu.Front.DataDefinition (DataDefinition)
import Mlatu.Front.DataDefinition qualified as Data
import Mlatu.Front.Definition (Category (..), Definition (..), Merge (..), Parent (..))
import Mlatu.Front.Definition qualified as Definition
import Mlatu.Front.Fragment (Fragment)
import Mlatu.Front.Fragment qualified as Fragment
import Mlatu.Front.Parameter (Parameter (Parameter))
import Mlatu.Front.Signature (Signature)
import Mlatu.Front.Signature qualified as Signature
import Mlatu.Front.Term (Specialness (..), Term (..), compose)

type Desugar a = State Int a

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
  over
    Fragment.definitions
    ( <>
        evalState
          ( do
              defs <- traverse desugarCodataDefinition (view Fragment.codataDefinitions fragment)
              defs' <- traverse desugarDataDefinition (view Fragment.dataDefinitions fragment)
              pure (asum (defs <> defs'))
          )
          0
    )
    fragment

desugarCodataDefinition :: CodataDefinition -> Desugar [Definition ()]
desugarCodataDefinition definition = do
  c <- constructor
  let list = [0 .. (length (view Codata.deconstructors definition) - 1)]
  let ds = zipWith3 id (desugarDeconstructor <$> view Codata.deconstructors definition) list (reverse list)
  pure (c : ds)
  where
    desugarDeconstructor :: (Unqualified, [Signature], [Signature], Origin) -> Int -> Int -> Definition ()
    desugarDeconstructor (name, pre, post, origin) lo hi =
      Definition
        { Definition._body =
            Match
              origin
              ()
              [ ( origin,
                  QualifiedName constructorName,
                  compose
                    origin
                    ()
                    ( replicate hi (Word origin () "drop" [])
                        <> replicate lo (Word origin () "nip" [])
                    )
                )
              ]
              (origin, Left ()),
          Definition._category = DeconstructorWord,
          Definition._inferSignature = False,
          Definition._merge = DenyMerge,
          Definition._name = Qualified (qualifierName $ view Codata.name definition) name,
          Definition._origin = origin,
          Definition._parent = Just $ TypeParent $ view Codata.name definition,
          Definition._signature =
            Signature.Quantified
              (view Codata.parameters definition)
              (Signature.Function pre post [] origin)
              origin
        }
    constructorName =
      Qualified
        (qualifierName (view Codata.name definition))
        (unqualifiedName (view Codata.name definition))
    constructor = makeDefinition $ \index ->
      Definition
        { Definition._body =
            New
              (view Codata.origin definition)
              ()
              (ConstructorIndex index)
              (length $ view Codata.deconstructors definition)
              NonSpecial,
          Definition._category = DeconstructorWord,
          Definition._inferSignature = False,
          Definition._merge = DenyMerge,
          Definition._name = constructorName,
          Definition._origin = origin,
          Definition._parent = Just $ TypeParent $ view Codata.name definition,
          Definition._signature = constructorSignature
        }
    resultSignature =
      foldl'
        (\a b -> Signature.Application a b origin)
        ( Signature.Variable (QualifiedName $ view Codata.name definition) $
            view Codata.origin definition
        )
        $ ( \(Parameter parameterOrigin parameter _kind) ->
              Signature.Variable (UnqualifiedName parameter) parameterOrigin
          )
          <$> view Codata.parameters definition
    constructorSignature =
      Signature.Quantified
        (view Codata.parameters definition)
        ( Signature.Function
            (asum (view _3 <$> view Codata.deconstructors definition))
            [resultSignature]
            []
            origin
        )
        origin
    origin = view Codata.origin definition

desugarDataDefinition :: DataDefinition -> Desugar [Definition ()]
desugarDataDefinition definition =
  traverse desugarConstructor $ view Data.constructors definition
  where
    desugarConstructor :: (Unqualified, [Signature], [Signature], Origin) -> Desugar (Definition ())
    desugarConstructor (name, input, output, origin) = makeDefinition $ \index ->
      Definition
        { Definition._body =
            New
              origin
              ()
              (ConstructorIndex index)
              (length input)
              ( case unqualifiedName (view Data.name definition) of
                  "nat" -> NatLike
                  "list" -> ListLike
                  _ -> NonSpecial
              ),
          Definition._category = ConstructorWord,
          Definition._inferSignature = False,
          Definition._merge = DenyMerge,
          Definition._name = Qualified qualifier name,
          Definition._origin = origin,
          Definition._parent = Just $ TypeParent $ view Data.name definition,
          Definition._signature = constructorSignature
        }
      where
        constructorSignature =
          Signature.Quantified
            (view Data.parameters definition)
            ( Signature.Function
                input
                output
                []
                origin
            )
            origin
        qualifier = qualifierName $ view Data.name definition

makeDefinition :: (Int -> Definition ()) -> Desugar (Definition ())
makeDefinition f = do
  index <- get
  modify (+ 1)
  pure (f index)
