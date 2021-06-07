-- |
-- Module      : Mlatu.Resolve
-- Description : Name resolution
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Resolve
  ( definition,
    generalName,
    run,
    signature,
  )
where

import Data.List (elemIndex)
import Data.Set qualified as Set
import Mlatu.Definition (Definition)
import Mlatu.Definition qualified as Definition
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Entry.Parameter (Parameter (Parameter))
import Mlatu.Informer (M, ice, reportCannotResolveName)
import Mlatu.Informer qualified as Report
import Mlatu.Name
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Mlatu.Signature qualified as Signature
import Mlatu.Term (Term (..), Value (..))
import Mlatu.Term qualified as Term
import Mlatu.Vocabulary
import Optics
import Relude hiding (Compose)
import Relude.Unsafe qualified as Unsafe

type Resolved a = StateT [Unqualified] M a

-- | Name resolution is responsible for rewriting unqualified calls to
-- definitions into fully qualified calls.
run :: Resolved a -> M a
run = flip evalStateT []

definition :: Dictionary -> Definition () -> Resolved (Definition ())
definition dictionary def = do
  -- FIXME: reportDuplicate dictionary def
  let vocabulary = qualifierName $ view Definition.name def
  body <- term dictionary vocabulary $ view Definition.body def
  sig <- signature dictionary vocabulary $ view Definition.signature def
  pure
    (set Definition.body body (set Definition.signature sig def))

term :: Dictionary -> Qualifier -> Term () -> Resolved (Term ())
term dictionary vocabulary = recur
  where
    recur :: Term () -> Resolved (Term ())
    recur (Coercion a b (Term.AnyCoercion sig)) =
      Coercion a b <$> (Term.AnyCoercion <$> signature dictionary vocabulary sig)
    recur unresolved@Coercion {} = pure unresolved
    recur (Compose _ a b) = Compose () <$> recur a <*> recur b
    recur Generic {} =
      ice
        "Mlatu.Resolve.term - generic expression should not appear before name resolution"
    recur (Group a) = Group <$> recur a
    recur (Lambda origin _ name _ t) = withLocal name $ Lambda origin () name () <$> recur t
    recur (Match origin _ cases else_) =
      Match origin () <$> traverse resolveCase cases <*> resolveElse else_
      where
        resolveCase (co, name, t) = do
          resolved <- definitionName dictionary vocabulary name co
          (co,resolved,) <$> recur t

        resolveElse (o, Left a) = pure (o, Left a)
        resolveElse (o, Right t) = (\x -> (o, Right x)) <$> recur t
    recur unresolved@New {} = pure unresolved
    recur unresolved@NewClosure {} = pure unresolved
    recur (Push origin _ v) =
      Push origin () <$> value dictionary vocabulary v
    recur (Word origin _ name params) =
      Word origin () <$> definitionName dictionary vocabulary name origin <*> pure params

value :: Dictionary -> Qualifier -> Value () -> Resolved (Value ())
value _ _ Capture {} = ice "Mlatu.Resolve.value - closure should not appear before name resolution"
value _ _ v@Character {} = pure v
value _ _ Closed {} = ice "Mlatu.Resolve.value - closed name should not appear before name resolution"
value _ _ Local {} = ice "Mlatu.Resolve.value - local name should not appear before name resolution"
-- FIXME: Maybe should be a GeneralName and require resolution.
value _ _ v@Name {} = pure v
value dictionary vocabulary (Quotation t) = Quotation <$> term dictionary vocabulary t
value _ _ v@Text {} = pure v

signature :: Dictionary -> Qualifier -> Signature -> Resolved Signature
signature dictionary vocabulary = go
  where
    go :: Signature -> Resolved Signature
    go (Signature.Application a b origin) =
      Signature.Application
        <$> go a <*> go b <*> pure origin
    go sig@Signature.Bottom {} = pure sig
    go (Signature.Grouped a origin) = Signature.Grouped <$> go a <*> pure origin
    go (Signature.Function as bs es origin) =
      Signature.Function
        <$> traverse go as
        <*> traverse go bs
        <*> zipWithM (typeName dictionary vocabulary) es (repeat origin)
        <*> pure origin
    go (Signature.Quantified vars a origin) =
      Signature.Quantified vars
        <$> foldr (withLocal . (\(Parameter _ name _) -> name)) (go a) vars
        <*> pure origin
    go (Signature.Variable name origin) =
      Signature.Variable
        <$> typeName dictionary vocabulary name origin <*> pure origin
    go (Signature.StackFunction r as s bs es origin) =
      Signature.StackFunction r
        <$> traverse go as
        <*> pure s
        <*> traverse go bs
        <*> zipWithM (typeName dictionary vocabulary) es (repeat origin)
        <*> pure origin
    go sig@Signature.Type {} = pure sig

definitionName,
  typeName ::
    Dictionary -> Qualifier -> GeneralName -> Origin -> Resolved GeneralName
definitionName dictionary =
  generalName Report.WordName resolveLocal isDefined
  where
    isDefined = flip Set.member defined
    defined = Set.fromList $ Dictionary.wordNames dictionary
    resolveLocal _ index = pure $ LocalName index
typeName dictionary =
  generalName Report.TypeName resolveLocal isDefined
  where
    isDefined = flip Set.member defined
    defined = Set.fromList $ Dictionary.typeNames dictionary
    resolveLocal l _ = pure $ UnqualifiedName l

generalName ::
  Report.NameCategory ->
  (Unqualified -> LocalIndex -> Resolved GeneralName) ->
  (Qualified -> Bool) ->
  Qualifier ->
  GeneralName ->
  Origin ->
  Resolved GeneralName
generalName category resolveLocal isDefined vocabulary name origin =
  case name of
    -- An unqualified name may refer to a local, a name in the current vocabulary,
    -- or a name in the global scope, respectively.

    UnqualifiedName unqualified -> do
      mLocalIndex <- gets (elemIndex unqualified)
      case mLocalIndex of
        Just index -> resolveLocal unqualified (LocalIndex index)
        Nothing -> do
          let qualified = Qualified vocabulary unqualified
          if isDefined qualified
            then pure (QualifiedName qualified)
            else do
              let global = Global unqualified
              if isDefined global
                then pure (QualifiedName global)
                else do
                  lift $ reportCannotResolveName origin category name
                  pure name

    -- A qualified name may refer to an intrinsic or a definition.

    QualifiedName qualified ->
      if isDefined qualified
        then pure name
        else do
          let qualified' = case (vocabulary, qualifierName qualified) of
                (Qualifier _root1 prefix, Qualifier _root2 suffix) ->
                  Qualified (Qualifier Absolute (prefix ++ suffix)) $
                    unqualifiedName qualified
          if isDefined qualified'
            then pure $ QualifiedName qualified'
            else do
              lift $ reportCannotResolveName origin category name
              pure name
    LocalName {} -> ice "Mlatu.Resolve.generalName - local name should not appear before name resolution"

withLocal :: Unqualified -> Resolved a -> Resolved a
withLocal name action = do
  modify (name :)
  result <- action
  modify (Unsafe.fromJust . viaNonEmpty tail)
  pure result
