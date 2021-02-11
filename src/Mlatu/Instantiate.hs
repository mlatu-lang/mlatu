-- |
-- Module      : Mlatu.Instantiate
-- Description : Instantiating generic types
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Instantiate
  ( prenex,
    term,
    typ,
  )
where

import Mlatu.Informer (Informer (..))
import Mlatu.Kind (Kind)
import Mlatu.Monad (K)
import Mlatu.Name (Unqualified)
import Mlatu.Origin (Origin)
import Mlatu.Pretty qualified as Pretty
import Mlatu.Report qualified as Report
import Mlatu.Substitute qualified as Substitute
import Mlatu.Term (Term (..))
import Mlatu.Type (Type (..), TypeId, Var (..))
import Mlatu.TypeEnv (TypeEnv, freshTypeId)
import Mlatu.Zonk qualified as Zonk
import Relude hiding (Type)
import Text.PrettyPrint qualified as Pretty

-- | To instantiate a type scheme, we simply replace all quantified variables
-- with fresh ones and remove the quantifier, returning the types with which the
-- variables were instantiated, in order. Because type identifiers are globally
-- unique, we know a fresh type variable will never be erroneously captured.
typ ::
  TypeEnv ->
  Origin ->
  Unqualified ->
  TypeId ->
  Kind ->
  Type ->
  K (Type, Type, TypeEnv)
typ tenv0 origin name x k t = do
  ia <- freshTypeId tenv0
  let a = TypeVar origin $ Var name ia k
  replaced <- Substitute.typ tenv0 x a t
  return (replaced, a, tenv0)

-- | When generating an instantiation of a generic definition, we only want to
-- instantiate the rank-1 quantifiers; all other quantifiers are irrelevant.
prenex :: TypeEnv -> Type -> K (Type, [Type], TypeEnv)
prenex tenv0 q@(Forall origin (Var name x k) t) =
  while origin (Pretty.hsep ["instantiating", Pretty.quote q]) $ do
    (t', a, tenv1) <- typ tenv0 origin name x k t
    (t'', as, tenv2) <- prenex tenv1 t'
    return (t'', a : as, tenv2)
prenex tenv0 t = return (t, [], tenv0)

-- | Instantiates a generic expression with the given type arguments.
term :: TypeEnv -> Term Type -> [Type] -> K (Term Type)
term tenv t args = foldlM go t args
  where
    go (Generic _name x expr _origin) arg = Substitute.term tenv x arg expr
    go _ _ = do
      report $ Report.TypeArgumentCountMismatch t $ map (Zonk.typ tenv) args
      halt
