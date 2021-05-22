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

import Data.List (zipWith3)
import Mlatu.CodataDefinition (CodataDefinition)
import Mlatu.CodataDefinition qualified as Codata
import Mlatu.DataDefinition (DataDefinition)
import Mlatu.DataDefinition qualified as Data
import Mlatu.Definition (Definition (Definition))
import Mlatu.Definition qualified as Definition
import Mlatu.Entry.Category qualified as Category
import Mlatu.Entry.Merge qualified as Merge
import Mlatu.Entry.Parameter (Parameter (Parameter))
import Mlatu.Entry.Parent qualified as Parent
import Mlatu.Fragment (Fragment)
import Mlatu.Fragment qualified as Fragment
import Mlatu.Name (ConstructorIndex (..), GeneralName (..), Qualified (..), Unqualified (..))
import Mlatu.Signature qualified as Signature
import Mlatu.Term (Case (..), Else (..), Specialness (..), Term (..), compose)
import Optics
import Relude

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
    desugarDeconstructor (name, sig, origin) = \lo hi ->
      Definition
        { Definition._body =
            Match
              ()
              [ Case
                  (QualifiedName constructorName)
                  ( compose
                      ()
                      origin
                      ( replicate hi (Word () "drop" [] origin)
                          <> replicate lo (Word () "nip" [] origin)
                      )
                  )
                  origin
              ]
              (DefaultElse () origin)
              origin,
          Definition._category = Category.Deconstructor,
          Definition._inferSignature = False,
          Definition._merge = Merge.Deny,
          Definition._name = Qualified (qualifierName $ view Codata.name definition) name,
          Definition._origin = origin,
          Definition._parent = Just $ Parent.Type $ view Codata.name definition,
          Definition._signature =
            Signature.Quantified
              (view Codata.parameters definition)
              (Signature.Function [resultSignature] [sig] [] origin)
              origin
        }
    constructorName =
      Qualified
        (qualifierName (view Codata.name definition))
        ("mk-" <> unqualifiedName (view Codata.name definition))
    constructor = makeDefinition $ \index ->
      Definition
        { Definition._body =
            New
              ()
              (ConstructorIndex index)
              (length $ view Codata.deconstructors definition)
              NonSpecial
              $ view Codata.origin definition,
          Definition._category = Category.Deconstructor,
          Definition._inferSignature = False,
          Definition._merge = Merge.Deny,
          Definition._name = constructorName,
          Definition._origin = origin,
          Definition._parent = Just $ Parent.Type $ view Codata.name definition,
          Definition._signature = constructorSignature
        }
    resultSignature =
      foldl'
        (\a b -> Signature.Application a b origin)
        ( Signature.Variable (QualifiedName $ view Codata.name definition) $
            view Codata.origin definition
        )
        $ ( \(Parameter parameterOrigin parameter _kind _) ->
              Signature.Variable (UnqualifiedName parameter) parameterOrigin
          )
          <$> view Codata.parameters definition
    constructorSignature =
      Signature.Quantified
        (view Codata.parameters definition)
        ( Signature.Function
            (view _2 <$> view Codata.deconstructors definition)
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
    desugarConstructor constructor = makeDefinition $ \index ->
      Definition
        { Definition._body =
            New
              ()
              (ConstructorIndex index)
              (length $ view _2 constructor)
              ( case unqualifiedName (view Data.name definition) of
                  "nat" -> NatLike
                  "list" -> ListLike
                  _ -> NonSpecial
              )
              $ view _3 constructor,
          Definition._category = Category.Constructor,
          Definition._inferSignature = False,
          Definition._merge = Merge.Deny,
          Definition._name = Qualified qualifier $ view _1 constructor,
          Definition._origin = origin,
          Definition._parent = Just $ Parent.Type $ view Data.name definition,
          Definition._signature = constructorSignature
        }
      where
        resultSignature =
          foldl'
            (\a b -> Signature.Application a b origin)
            ( Signature.Variable (QualifiedName $ view Data.name definition) $
                view Data.origin definition
            )
            $ ( \(Parameter parameterOrigin parameter _kind _) ->
                  Signature.Variable (UnqualifiedName parameter) parameterOrigin
              )
              <$> view Data.parameters definition
        constructorSignature =
          Signature.Quantified
            (view Data.parameters definition)
            ( Signature.Function
                (view _2 constructor)
                [resultSignature]
                []
                origin
            )
            origin
        origin = view _3 constructor
        qualifier = qualifierName $ view Data.name definition

makeDefinition :: (Int -> Definition ()) -> Desugar (Definition ())
makeDefinition f = do
  index <- get
  modify (+ 1)
  pure (f index)
