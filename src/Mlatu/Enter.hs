-- |
-- Module      : Mlatu.Enter
-- Description : Inserting entries into the dictionary
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Enter
  ( fragment,
    fragmentFromSource,
    resolveAndDesugar,
  )
where

import Data.Map.Strict qualified as Map
import Mlatu.Declaration (Declaration)
import Mlatu.Declaration qualified as Declaration
import Mlatu.Definition (Definition)
import Mlatu.Definition qualified as Definition
import Mlatu.Desugar.Infix qualified as Infix
import Mlatu.Desugar.Quotations qualified as Quotations
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Category qualified as Category
import Mlatu.Entry.Merge qualified as Merge
import Mlatu.Fragment (Fragment)
import Mlatu.Fragment qualified as Fragment
import Mlatu.Ice (ice)
import Mlatu.Infer (mangleInstance, typecheck)
import Mlatu.Informer (errorCheckpoint, report)
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Metadata (Metadata)
import Mlatu.Metadata qualified as Metadata
import Mlatu.Monad (M)
import Mlatu.Name
  ( GeneralName (QualifiedName),
    Qualified (Qualified, qualifierName),
    Unqualified,
    qualifierFromName,
  )
import Mlatu.Parse qualified as Parse
import Mlatu.Pretty (printInstantiated, printQualified)
import Mlatu.Quantify qualified as Quantify
import Mlatu.Report qualified as Report
import Mlatu.Resolve qualified as Resolve
import Mlatu.Scope (scope)
import Mlatu.Signature qualified as Signature
import Mlatu.Term (Term)
import Mlatu.Term qualified as Term
import Mlatu.Tokenize (tokenize)
import Mlatu.TypeAlias (TypeAlias)
import Mlatu.TypeAlias qualified as TypeAlias
import Mlatu.TypeDefinition (TypeDefinition)
import Mlatu.TypeDefinition qualified as TypeDefinition
import Mlatu.Vocabulary qualified as Vocabulary
import Optics
import Prettyprinter (dquotes, hsep)
import Relude

-- | Enters a program fragment into a dictionary.
fragment :: Fragment () -> Dictionary -> M Dictionary
fragment f =
  foldlMx declareTypeAlias (view Fragment.aliases f)
    >=> foldlMx declareType (view Fragment.types f)
    -- We enter declarations of all traits and intrinsics.
    >=> foldlMx enterDeclaration (view Fragment.declarations f)
    -- Then declare all permissions.
    >=> foldlMx
      declareWord
      ( filter ((== Category.Permission) . view Definition.category) $
          view Fragment.definitions f
      )
    -- With everything type-level declared, we can resolve type signatures.
    >=> foldlMx
      resolveSignature
      (view Declaration.name <$> view Fragment.declarations f)
    -- And declare regular words.
    >=> foldlMx
      declareWord
      ( filter ((/= Category.Permission) . view Definition.category) $
          view Fragment.definitions f
      )
    -- Then resolve their signatures.
    >=> foldlMx
      resolveSignature
      ( fmap (view Definition.name) (view Fragment.definitions f)
          ++ fmap (view Declaration.name) (view Fragment.declarations f)
      )
    -- Add their metadata (esp. for operator metadata).
    >=> foldlMx addMetadata (view Fragment.metadata f)
    -- And finally enter their definitions.
    >=> foldlMx defineWord (view Fragment.definitions f)
  where
    foldlMx :: (Foldable f, Monad m) => (b -> a -> m b) -> f a -> b -> m b
    foldlMx = flip . foldlM

declareTypeAlias :: Dictionary -> TypeAlias -> M Dictionary
declareTypeAlias dictionary a =
  pure $
    Dictionary.insertTypeAlias
      (Instantiated (Qualified Vocabulary.global (view TypeAlias.name a)) [])
      (Entry.TypeAliasEntry (view TypeAlias.origin a) (view TypeAlias.alias a))
      dictionary

enterDeclaration :: Dictionary -> Declaration -> M Dictionary
enterDeclaration dictionary declaration = do
  let name = view Declaration.name declaration
      signature = view Declaration.signature declaration
      origin = view Declaration.origin declaration
  pure $ case view Declaration.category declaration of
    Declaration.Intrinsic ->
      case Dictionary.lookupWord (Instantiated name []) dictionary of
        -- TODO: Check signatures.
        Just _existing -> dictionary
        _ ->
          Dictionary.insertWord
            (Instantiated name [])
            ( Entry.WordEntry
                Category.Word
                Merge.Deny
                origin
                Nothing
                (Just signature)
                Nothing
            )
            dictionary
    Declaration.Trait -> case Dictionary.lookupTrait (Instantiated name []) dictionary of
      Just _existing -> dictionary
      Nothing -> Dictionary.insertTrait (Instantiated name []) (Entry.TraitEntry origin signature) dictionary

-- declare type, declare & define constructors
declareType :: Dictionary -> TypeDefinition -> M Dictionary
declareType dictionary typ =
  let name = view TypeDefinition.name typ
   in case Dictionary.lookupType (Instantiated name []) dictionary of
        Just (Entry.TypeEntry _origin parameters _ctors)
          | parameters == view TypeDefinition.parameters typ ->
            pure dictionary
        _ -> do
          let entry =
                Entry.TypeEntry
                  (view TypeDefinition.origin typ)
                  (view TypeDefinition.parameters typ)
                  (view TypeDefinition.constructors typ)
          pure $ Dictionary.insertType (Instantiated name []) entry dictionary

declareWord ::
  Dictionary -> Definition () -> M Dictionary
declareWord dictionary definition =
  let name = view Definition.name definition
      signature = view Definition.signature definition
   in case Dictionary.lookupWord (Instantiated name []) dictionary of
        -- Already declared with the same signature.
        Just (Entry.WordEntry _ _ originalOrigin _ mSignature _)
          | view Definition.inferSignature definition || mSignature == Just signature ->
            pure dictionary
          | otherwise ->
            do
              report $
                Report.makeError $
                  Report.WordRedeclaration
                    (Signature.origin signature)
                    name
                    signature
                    originalOrigin
                    mSignature
              pure dictionary
        _ -> case Dictionary.lookupTrait (Instantiated name []) dictionary of
          -- Already declared or defined as a trait.
          Just (Entry.TraitEntry _origin traitSignature)
            -- TODO: Better error reporting when a non-instance matches a trait.
            | view Definition.category definition == Category.Instance ->
              do
                let qualifier = qualifierName name
                resolvedSignature <-
                  Resolve.run $
                    Resolve.signature
                      dictionary
                      qualifier
                      $ view Definition.signature definition
                mangledName <-
                  mangleInstance
                    dictionary
                    (view Definition.name definition)
                    resolvedSignature
                    traitSignature
                let entry =
                      Entry.WordEntry
                        (view Definition.category definition)
                        (view Definition.merge definition)
                        (view Definition.origin definition)
                        (view Definition.parent definition)
                        (Just resolvedSignature)
                        Nothing
                pure $ Dictionary.insertWord mangledName entry dictionary
          _ -> do
            let entry =
                  Entry.WordEntry
                    (view Definition.category definition)
                    (view Definition.merge definition)
                    (view Definition.origin definition)
                    (view Definition.parent definition)
                    (Just signature)
                    Nothing
            pure $ Dictionary.insertWord (Instantiated name []) entry dictionary

addMetadata :: Dictionary -> Metadata -> M Dictionary
addMetadata dictionary0 metadata =
  foldlM addField dictionary0 $ Map.toList $ view Metadata.fields metadata
  where
    QualifiedName qualified = view Metadata.name metadata
    origin = view Metadata.origin metadata
    qualifier = qualifierFromName qualified

    addField :: Dictionary -> (Unqualified, Term ()) -> M Dictionary
    addField dictionary (unqualified, term) = do
      let name = Qualified qualifier unqualified
      pure $ case Dictionary.lookupMetadata (Instantiated name []) dictionary of
        Just {} -> dictionary -- TODO: Report duplicates or merge?
        Nothing ->
          Dictionary.insertMetadata
            (Instantiated name [])
            (Entry.MetadataEntry origin term)
            dictionary

resolveSignature :: Dictionary -> Qualified -> M Dictionary
resolveSignature dictionary name = do
  let qualifier = qualifierName name
  case Dictionary.lookupWord (Instantiated name []) dictionary of
    Just (Entry.WordEntry category merge origin parent (Just signature) body) -> do
      signature' <- Resolve.run $ Resolve.signature dictionary qualifier signature
      let entry = Entry.WordEntry category merge origin parent (Just signature') body
      pure $ Dictionary.insertWord (Instantiated name []) entry dictionary
    _ -> case Dictionary.lookupTrait (Instantiated name []) dictionary of
      Just (Entry.TraitEntry origin signature) -> do
        signature' <- Resolve.run $ Resolve.signature dictionary qualifier signature
        let entry = Entry.TraitEntry origin signature'
        pure $ Dictionary.insertTrait (Instantiated name []) entry dictionary
      _noResolution -> pure dictionary

-- typecheck and define user-defined words
-- desugaring of operators has to take place here
defineWord ::
  Dictionary ->
  Definition () ->
  M Dictionary
defineWord dictionary definition = do
  let name = view Definition.name definition
  resolved <- resolveAndDesugar dictionary definition
  errorCheckpoint
  let resolvedSignature = view Definition.signature resolved
  -- Note that we use the resolved signature here.
  (typecheckedBody, typ) <-
    typecheck
      dictionary
      ( if view Definition.inferSignature definition
          then Nothing
          else Just resolvedSignature
      )
      $ view Definition.body resolved
  errorCheckpoint
  case Dictionary.lookupTrait (Instantiated name []) dictionary of
    -- Already declared or defined as a trait.
    Just (Entry.TraitEntry _origin traitSignature)
      | view Definition.category definition == Category.Instance ->
        do
          mangledName <-
            mangleInstance
              dictionary
              name
              resolvedSignature
              traitSignature
          -- Should this use the mangled name?
          (flattenedBody, dictionary') <-
            Quotations.desugar
              dictionary
              (qualifierFromName name)
              $ Quantify.term typ typecheckedBody
          let entry =
                Entry.WordEntry
                  (view Definition.category definition)
                  (view Definition.merge definition)
                  (view Definition.origin definition)
                  (view Definition.parent definition)
                  (Just resolvedSignature)
                  (Just flattenedBody)
          pure $ Dictionary.insertWord mangledName entry dictionary'
    -- Previously declared with same signature, but not defined.
    _ -> case Dictionary.lookupWord (Instantiated name []) dictionary of
      Just (Entry.WordEntry category merge origin' parent signature' Nothing)
        | maybe True (resolvedSignature ==) signature' ->
          Quotations.desugar dictionary (qualifierFromName name) (Quantify.term typ typecheckedBody)
            >>= \(flattenedBody, dictionary') ->
              pure $
                Dictionary.insertWord
                  (Instantiated name [])
                  ( Entry.WordEntry
                      category
                      merge
                      origin'
                      parent
                      (Just $ if view Definition.inferSignature definition then Signature.Type typ else resolvedSignature)
                      $ Just flattenedBody
                  )
                  dictionary'
      -- Already defined as concatenable.
      Just (Entry.WordEntry category merge@Merge.Compose origin' parent mSignature body)
        | view Definition.inferSignature definition
            || Just resolvedSignature == mSignature -> do
          composedBody <- case body of
            Just existing -> do
              let strippedBody = Term.stripMetadata existing
              pure $ Term.Compose () strippedBody $ view Definition.body resolved
            Nothing -> pure $ view Definition.body resolved
          (composed, composedType) <-
            typecheck
              dictionary
              ( if view Definition.inferSignature definition
                  then Nothing
                  else Just resolvedSignature
              )
              composedBody
          (flattenedBody, dictionary') <-
            Quotations.desugar
              dictionary
              (qualifierFromName name)
              $ Quantify.term composedType composed
          let entry =
                Entry.WordEntry
                  category
                  merge
                  origin'
                  parent
                  ( if view Definition.inferSignature definition
                      then Nothing -- Just (Signature.Type composedType)
                      else mSignature
                  )
                  $ Just flattenedBody
          pure $ Dictionary.insertWord (Instantiated name []) entry dictionary'
      -- Already defined, not concatenable.
      Just (Entry.WordEntry _ Merge.Deny originalOrigin _ (Just _sig) _) -> do
        report $
          Report.makeError $
            Report.WordRedefinition
              (view Definition.origin definition)
              name
              originalOrigin

        pure dictionary
      -- Not previously declared as word.
      _nonDeclared ->
        ice $
          show $
            hsep
              [ "Mlatu.Enter.defineWord - defining word",
                dquotes $ printQualified name,
                "not previously declared"
              ]

-- | Parses a source file into a program fragment.
fragmentFromSource ::
  -- | List of permissions granted to @main@.
  [GeneralName] ->
  -- | Override name of @main@.
  Maybe Qualified ->
  -- | Initial source line (e.g. for REPL offset).
  Int ->
  -- | Source file path for error reporting.
  FilePath ->
  -- | Source itself.
  Text ->
  -- | Parsed program fragment.
  M (Fragment ())
fragmentFromSource mainPermissions mainName line path source = do
  -- Sources are lexed into a stream of tokens.

  tokenized <- tokenize line path source
  errorCheckpoint

  -- We then parse the token stream as a series of top-level program elements.
  -- Datatype definitions are desugared into regular definitions, so that name
  -- resolution can find their names.

  parsed <- Parse.fragment line path mainPermissions mainName tokenized

  errorCheckpoint

  pure parsed

resolveAndDesugar :: Dictionary -> Definition () -> M (Definition ())
resolveAndDesugar dictionary definition = do
  -- Name resolution rewrites unqualified names into fully qualified names, so
  -- that it's evident from a name which program element it refers to.

  -- needs dictionary for declared names
  resolved <- Resolve.run $ Resolve.definition dictionary definition

  errorCheckpoint

  -- After names have been resolved, the precedences of operators are known, so
  -- infix operators can be desugared into postfix syntax.

  -- needs dictionary for operator metadata
  postfix <- Infix.desugar resolved

  errorCheckpoint

  -- In addition, now that we know which names refer to local variables,
  -- quotations can be rewritten into closures that explicitly capture the
  -- variables they use from the enclosing scope.

  pure $ over Definition.body scope postfix
