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

import Control.Lens ((^.))
import Control.Lens.Setter (over)
import Data.HashMap.Strict qualified as HashMap
import Mlatu.Bracket (bracket)
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
import Mlatu.Infer (mangleInstance, typecheck)
import Mlatu.Informer (checkpoint, report)
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Metadata (Metadata)
import Mlatu.Metadata qualified as Metadata
import Mlatu.Monad (K)
import Mlatu.Name
  ( GeneralName (QualifiedName),
    Qualified (Qualified),
    Unqualified,
    qualifierFromName,
    qualifierName,
  )
import Mlatu.Parse qualified as Parse
import Mlatu.Pretty qualified as Pretty
import Mlatu.Quantify qualified as Quantify
import Mlatu.Report qualified as Report
import Mlatu.Resolve qualified as Resolve
import Mlatu.Scope (scope)
import Mlatu.Signature qualified as Signature
import Mlatu.Term (Term)
import Mlatu.Term qualified as Term
import Mlatu.Tokenize (tokenize)
import Mlatu.TypeDefinition (TypeDefinition)
import Mlatu.TypeDefinition qualified as TypeDefinition
import Relude
import Text.PrettyPrint qualified as Pretty

-- | Enters a program fragment into a dictionary.
fragment :: Fragment () -> Dictionary -> K Dictionary
fragment f =
  -- TODO: Link constructors to parent type.
  foldlMx declareType (f ^. Fragment.types)
    -- We enter declarations of all traits and intrinsics.
    >=> foldlMx enterDeclaration (f ^. Fragment.declarations)
    -- Then declare all permissions.
    >=> foldlMx
      declareWord
      ( filter (\def -> def ^. Definition.category == Category.Permission) $
          f ^. Fragment.definitions
      )
    -- With everything type-level declared, we can resolve type signatures.
    >=> foldlMx
      resolveSignature
      (map Declaration.name (f ^. Fragment.declarations))
    -- And declare regular words.
    >=> foldlMx
      declareWord
      ( filter (\def -> def ^. Definition.category /= Category.Permission) $
          f ^. Fragment.definitions
      )
    -- Then resolve their signatures.
    >=> foldlMx
      resolveSignature
      ( map (^. Definition.name) (f ^. Fragment.definitions)
          ++ map Declaration.name (f ^. Fragment.declarations)
      )
    -- Add their metadata (esp. for operator metadata).
    >=> foldlMx addMetadata (f ^. Fragment.metadata)
    -- And finally enter their definitions.
    >=> foldlMx defineWord (f ^. Fragment.definitions)
  where
    foldlMx :: (Foldable f, Monad m) => (b -> a -> m b) -> f a -> b -> m b
    foldlMx = flip . foldlM

enterDeclaration :: Dictionary -> Declaration -> K Dictionary
enterDeclaration dictionary declaration = do
  let name = Declaration.name declaration
      signature = Declaration.signature declaration
      origin = Declaration.origin declaration
  case Dictionary.lookup (Instantiated name []) dictionary of
    -- TODO: Check signatures.
    Just _existing -> return dictionary
    Nothing -> case Declaration.category declaration of
      Declaration.Intrinsic -> do
        let entry =
              Entry.Word
                Category.Word
                Merge.Deny
                origin
                -- FIXME: Does a declaration ever need a parent?
                Nothing
                (Just signature)
                Nothing
        return $ Dictionary.insert (Instantiated name []) entry dictionary
      Declaration.Trait -> do
        let entry = Entry.Trait origin signature
        return $ Dictionary.insert (Instantiated name []) entry dictionary

-- declare type, declare & define constructors
declareType :: Dictionary -> TypeDefinition -> K Dictionary
declareType dictionary typ =
  let name = typ ^. TypeDefinition.name
   in case Dictionary.lookup (Instantiated name []) dictionary of
        -- Not previously declared.
        Nothing -> do
          let entry =
                Entry.Type
                  (typ ^. TypeDefinition.origin)
                  (typ ^. TypeDefinition.parameters)
                  (typ ^. TypeDefinition.constructors)
          return $ Dictionary.insert (Instantiated name []) entry dictionary
        -- Previously declared with the same parameters.
        Just (Entry.Type _origin parameters _ctors)
          | parameters == typ ^. TypeDefinition.parameters ->
            return dictionary
        -- Already declared or defined differently.
        Just {} ->
          error $
            toText $
              Pretty.render $
                Pretty.hsep
                  [ "type",
                    Pretty.quote name,
                    "already declared or defined differently"
                  ]

declareWord ::
  Dictionary -> Definition () -> K Dictionary
declareWord dictionary definition =
  let name = definition ^. Definition.name
      signature = definition ^. Definition.signature
   in case Dictionary.lookup (Instantiated name []) dictionary of
        -- Not previously declared or defined.
        Nothing -> do
          let entry =
                Entry.Word
                  (definition ^. Definition.category)
                  (definition ^. Definition.merge)
                  (definition ^. Definition.origin)
                  (definition ^. Definition.parent)
                  (Just signature)
                  Nothing
          return $ Dictionary.insert (Instantiated name []) entry dictionary
        -- Already declared with the same signature.
        Just (Entry.Word _ _ originalOrigin _ mSignature _)
          | definition ^. Definition.inferSignature || mSignature == Just signature ->
            return dictionary
          | otherwise ->
            do
              report $
                Report.WordRedeclaration
                  (Signature.origin signature)
                  name
                  signature
                  originalOrigin
                  mSignature
              return dictionary
        -- Already declared or defined as a trait.
        Just (Entry.Trait _origin traitSignature)
          -- TODO: Better error reporting when a non-instance matches a trait.
          | definition ^. Definition.category == Category.Instance ->
            do
              let qualifier = name ^. qualifierName
              resolvedSignature <-
                Resolve.run $
                  Resolve.signature
                    dictionary
                    qualifier
                    $ definition ^. Definition.signature
              mangledName <-
                mangleInstance
                  dictionary
                  (definition ^. Definition.name)
                  resolvedSignature
                  traitSignature
              let entry =
                    Entry.Word
                      (definition ^. Definition.category)
                      (definition ^. Definition.merge)
                      (definition ^. Definition.origin)
                      (definition ^. Definition.parent)
                      (Just resolvedSignature)
                      Nothing
              return $ Dictionary.insert mangledName entry dictionary
        -- Already declared or defined with a different signature.
        Just {} ->
          error $
            toText $
              Pretty.render $
                Pretty.hsep
                  [ "word",
                    Pretty.quote name,
                    "already declared or defined without signature or as a non-word"
                  ]

addMetadata :: Dictionary -> Metadata -> K Dictionary
addMetadata dictionary0 metadata =
  foldlM addField dictionary0 $ HashMap.toList $ metadata ^. Metadata.fields
  where
    QualifiedName qualified = metadata ^. Metadata.name
    origin = metadata ^. Metadata.origin
    qualifier = qualifierFromName qualified

    addField :: Dictionary -> (Unqualified, Term ()) -> K Dictionary
    addField dictionary (unqualified, term) = do
      let name = Qualified qualifier unqualified
      case Dictionary.lookup (Instantiated name []) dictionary of
        Just {} -> return dictionary -- TODO: Report duplicates or merge?
        Nothing ->
          return $
            Dictionary.insert
              (Instantiated name [])
              (Entry.Metadata origin term)
              dictionary

resolveSignature :: Dictionary -> Qualified -> K Dictionary
resolveSignature dictionary name = do
  let qualifier = name ^. qualifierName
  case Dictionary.lookup (Instantiated name []) dictionary of
    Just (Entry.Word category merge origin parent (Just signature) body) -> do
      signature' <- Resolve.run $ Resolve.signature dictionary qualifier signature
      let entry = Entry.Word category merge origin parent (Just signature') body
      return $ Dictionary.insert (Instantiated name []) entry dictionary
    Just (Entry.Trait origin signature) -> do
      signature' <- Resolve.run $ Resolve.signature dictionary qualifier signature
      let entry = Entry.Trait origin signature'
      return $ Dictionary.insert (Instantiated name []) entry dictionary
    _noResolution -> return dictionary

-- typecheck and define user-defined words
-- desugaring of operators has to take place here
defineWord ::
  Dictionary ->
  Definition () ->
  K Dictionary
defineWord dictionary definition = do
  let name = definition ^. Definition.name
  resolved <- resolveAndDesugar dictionary definition
  checkpoint
  let resolvedSignature = resolved ^. Definition.signature
  -- Note that we use the resolved signature here.
  (typecheckedBody, typ) <-
    typecheck
      dictionary
      ( if definition ^. Definition.inferSignature
          then Nothing
          else Just resolvedSignature
      )
      $ resolved ^. Definition.body
  checkpoint
  case Dictionary.lookup (Instantiated name []) dictionary of
    -- Already declared or defined as a trait.
    Just (Entry.Trait _origin traitSignature)
      | definition ^. Definition.category == Category.Instance ->
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
                Entry.Word
                  (definition ^. Definition.category)
                  (definition ^. Definition.merge)
                  (definition ^. Definition.origin)
                  (definition ^. Definition.parent)
                  (Just resolvedSignature)
                  (Just flattenedBody)
          return $ Dictionary.insert mangledName entry dictionary'
    -- Previously declared with same signature, but not defined.
    Just (Entry.Word category merge origin' parent signature' Nothing)
      | maybe True (resolvedSignature ==) signature' -> do
        (flattenedBody, dictionary') <-
          Quotations.desugar
            dictionary
            (qualifierFromName name)
            $ Quantify.term typ typecheckedBody
        let entry =
              Entry.Word
                category
                merge
                origin'
                parent
                ( Just $
                    if definition ^. Definition.inferSignature
                      then Signature.Type typ
                      else resolvedSignature
                )
                $ Just flattenedBody
        return $ Dictionary.insert (Instantiated name []) entry dictionary'
    -- Already defined as concatenable.
    Just
      ( Entry.Word
          category
          merge@Merge.Compose
          origin'
          parent
          mSignature
          body
        )
        | definition ^. Definition.inferSignature
            || Just resolvedSignature == mSignature -> do
          composedBody <- case body of
            Just existing -> do
              let strippedBody = Term.stripMetadata existing
              return $ Term.Compose () strippedBody $ resolved ^. Definition.body
            Nothing -> return $ resolved ^. Definition.body
          (composed, composedType) <-
            typecheck
              dictionary
              ( if definition ^. Definition.inferSignature
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
                Entry.Word
                  category
                  merge
                  origin'
                  parent
                  ( if definition ^. Definition.inferSignature
                      then Nothing -- Just (Signature.Type composedType)
                      else mSignature
                  )
                  $ Just flattenedBody
          return $ Dictionary.insert (Instantiated name []) entry dictionary'
    -- Already defined, not concatenable.
    Just (Entry.Word _ Merge.Deny originalOrigin _ (Just _sig) _) -> do
      report $
        Report.WordRedefinition
          (definition ^. Definition.origin)
          name
          originalOrigin
      return dictionary
    -- Not previously declared as word.
    _nonDeclared ->
      error $
        toText $
          Pretty.render $
            Pretty.hsep
              [ "defining word",
                Pretty.quote name,
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
  K (Fragment ())
fragmentFromSource mainPermissions mainName line path source = do
  -- Sources are lexed into a stream of tokens.

  tokenized <- tokenize line path source
  checkpoint

  -- The layout rule is applied to desugar indentation-based syntax, so that the
  -- parser can find the ends of blocks without checking the indentation of
  -- tokens.

  bracketed <- bracket path tokenized

  -- We then parse the token stream as a series of top-level program elements.
  -- Datatype definitions are desugared into regular definitions, so that name
  -- resolution can find their names.

  parsed <- Parse.fragment line path mainPermissions mainName bracketed
  checkpoint

  return parsed

resolveAndDesugar :: Dictionary -> Definition () -> K (Definition ())
resolveAndDesugar dictionary definition = do
  -- Name resolution rewrites unqualified names into fully qualified names, so
  -- that it's evident from a name which program element it refers to.

  -- needs dictionary for declared names
  resolved <- Resolve.run $ Resolve.definition dictionary definition
  checkpoint

  -- After names have been resolved, the precedences of operators are known, so
  -- infix operators can be desugared into postfix syntax.

  -- needs dictionary for operator metadata
  postfix <- Infix.desugar dictionary resolved
  checkpoint

  -- In addition, now that we know which names refer to local variables,
  -- quotations can be rewritten into closures that explicitly capture the
  -- variables they use from the enclosing scope.

  return $ over Definition.body scope postfix
