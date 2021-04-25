{-# LANGUAGE RecursiveDo #-}

-- |
-- Module      : Mlatu.Infer
-- Description : Type inference
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Infer
  ( inferType0,
    mangleInstance,
    typeFromSignature,
    typeKind,
    typecheck,
  )
where

-- mport Data.Foldable (foldrM)

import Data.Foldable (foldrM)
import Data.List (partition)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Mlatu.DataConstructor (DataConstructor)
import Mlatu.DataConstructor qualified as DataConstructor
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Parameter (Parameter (Parameter))
import Mlatu.Entry.Parent qualified as Parent
import Mlatu.Ice (ice)
import Mlatu.Informer (Informer (..), errorCheckpoint)
import Mlatu.InstanceCheck (instanceCheck)
import Mlatu.Instantiate qualified as Instantiate
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Kind (Kind (..))
import Mlatu.Monad (M)
import Mlatu.Name (ClosureIndex (..), GeneralName (..), LocalIndex (..), Qualified (..), Unqualified (..))
import Mlatu.Origin (Origin)
import Mlatu.Pretty qualified as Pretty
import Mlatu.Regeneralize (regeneralize)
import Mlatu.Report qualified as Report
import Mlatu.Signature (Signature)
import Mlatu.Signature qualified as Signature
import Mlatu.Term (Case (..), Else (..), Term (..), Value (..), defaultElseBody)
import Mlatu.Term qualified as Term
import Mlatu.Type (Constructor (..), Type (..), Var (..))
import Mlatu.Type qualified as Type
import Mlatu.TypeEnv (TypeEnv, freshTypeId)
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Unify qualified as Unify
import Mlatu.Vocabulary qualified as Vocabulary
import Mlatu.Zonk qualified as Zonk
import Optics
import Prettyprinter (Doc, dquotes, hsep)
import Relude hiding (Compose, Type)
import Relude.Unsafe qualified as Unsafe

-- | Type inference takes a program fragment and produces a program with every
-- term annotated with its inferred type. It's polymorphic in the annotation
-- type of its input so that it can't depend on those annotations.
typecheck ::
  (Show a) =>
  -- | Current dictionary, for context.
  Dictionary ->
  -- | Optional signature to check inferred type against.
  Maybe Signature ->
  -- | Term to infer.
  Term a ->
  -- | Type-annotated term and its inferred type.
  M (Term Type, Type)
typecheck dictionary mDeclaredSignature term = do
  let tenv0 = TypeEnv.empty
  declaredType <- traverse (typeFromSignature tenv0) mDeclaredSignature
  declaredTypes <-
    traverse
      (\(name, signature) -> (,) name <$> typeFromSignature tenv0 signature)
      $ Dictionary.signatures dictionary
  let tenv1 = over TypeEnv.sigs (Map.union (Map.fromList declaredTypes)) tenv0
  inferType0 dictionary tenv1 declaredType term

-- | Mangles an instance name according to its trait signature.
mangleInstance ::
  Dictionary -> Qualified -> Signature -> Signature -> M Instantiated
mangleInstance dictionary name instanceSignature traitSignature = do
  let tenv0 = TypeEnv.empty
  instanceType <- typeFromSignature tenv0 instanceSignature
  traitType <- typeFromSignature tenv0 traitSignature
  instanceCheck "trait" traitType "instance" instanceType
  (traitType', args, tenv1) <- Instantiate.prenex tenv0 traitType
  tenv2 <- Unify.typ tenv1 instanceType traitType'
  args' <- starKinded dictionary $ Zonk.typ tenv2 <$> args
  pure $ Instantiated name args'

-- | Since type variables can be generalized if they do not depend on the
-- initial state of the typing environment, the type of a single definition is
-- inferred in an empty environment so that it can be trivially generalized. It
-- is then regeneralized to increase stack polymorphism.
inferType0 ::
  (Show a) =>
  -- | Current dictionary, for context.
  Dictionary ->
  -- | Current typing environment.
  TypeEnv ->
  -- | Optional type to check inferred type against.
  Maybe Type ->
  -- | Term to infer.
  Term a ->
  -- | Type-annotated term and its inferred type.
  M (Term Type, Type)
inferType0 dictionary tenv mDeclared term = do
  rec (term', t, tenvFinal) <- inferType dictionary tenvFinal' tenv term
      tenvFinal' <- maybe (pure tenvFinal) (Unify.typ tenvFinal t) mDeclared
  let zonked = Zonk.typ tenvFinal' t
  let regeneralized = regeneralize tenvFinal' zonked
  for_ mDeclared (instanceCheck "inferred" regeneralized "declared")
  pure (Zonk.term tenvFinal' term', regeneralized)

-- We infer the type of a term and annotate each terminal with the inferred type
-- as we go. We ignore any existing annotations because a definition may need to
-- be re-inferred and re-annotated if a program is updated with a new
-- implementation of an existing definition.

inferType ::
  (Show a) =>
  Dictionary ->
  TypeEnv ->
  TypeEnv ->
  Term a ->
  M (Term Type, Type, TypeEnv)
inferType dictionary tenvFinal tenv0 term0 = case term0 of
  -- A coercion is a typed no-op.
  --
  -- An identity coercion is the identity function on stacks. The empty program is
  -- an identity coercion.
  --
  -- A type coercion is the identity function specialised to a certain type. It
  -- may be specialised to the identity function on stacks; to the unary identity
  -- function on a particular type, in order to constrain the type of a value atop
  -- the stack; or to an arbitrary type, in order to unsafely reinterpret-cast
  -- between types, e.g., to grant or revoke permissions.

  Coercion hint@Term.IdentityCoercion _ origin ->
    while (Term.origin term0) context $ do
      [a] <- fresh origin [("s", Stack)]
      let typ = Type.Fun origin a a
      let type' = Zonk.typ tenvFinal typ
      pure (Coercion hint type' origin, typ, tenv0)
  Coercion hint@(Term.AnyCoercion sig) _ origin ->
    while (Term.origin term0) context $ do
      typ <- typeFromSignature tenv0 sig
      let type' = Zonk.typ tenvFinal typ
      pure (Coercion hint type' origin, typ, tenv0)

  -- The type of the composition of two expressions is the composition of the
  -- types of those expressions.

  Compose _ term1 term2 -> do
    (term1', t1, tenv1) <- inferType' tenv0 term1
    (term2', t2, tenv2) <- inferType' tenv1 term2
    (a, b, tenv3) <- Unify.function tenv2 t1
    (c, d, tenv4) <- Unify.function tenv3 t2
    tenv5 <- Unify.typ tenv4 b c
    -- FIXME: Use range origin over whole composition?
    let origin = Term.origin term1
    let typ = Type.Fun origin a d
    let type' = Zonk.typ tenvFinal typ
    pure (Compose type' term1' term2', typ, tenv5)

  -- TODO: Verify that this is correct.
  Generic _name _ t _ -> inferType' tenv0 t
  Group {} ->
    ice "Mlatu.Infer.inferType" "group expression should not appear during type inference"
  -- A local variable binding in Mlatu is in fact a lambda term in the ordinary
  -- lambda-calculus sense. We infer the type of its body in the environment
  -- extended with a fresh local bound to a fresh type variable, and produce a
  -- type of the form 'R..., A -> S... +P'.

  Lambda _ name@(Unqualified unqualified) _ term s origin -> do
    let varTypeName = Unqualified ("local" <> capitalize unqualified)
    a <-
      TypeEnv.freshTv
        tenv0
        varTypeName
        origin
        ( case s of
            Just 1 -> Circle
            _ -> Star
        )
    let oldLocals = view TypeEnv.vs tenv0
    let localEnv = over TypeEnv.vs (a :) tenv0
    (term', t1, tenv1) <- inferType' localEnv term
    let tenv2 = set TypeEnv.vs oldLocals tenv1
    (b, c, tenv3) <- Unify.function tenv2 t1
    let typ = Type.Fun origin (Type.Prod origin b a) c
        type' = Zonk.typ tenvFinal typ
        varType' = Zonk.typ tenvFinal a
    pure
      ( Lambda type' name varType' term' s origin,
        typ,
        tenv3
      )

  -- A match expression consumes an instance of a data type, pushing its fields
  -- onto the stack, and testing its tag to determine which case to apply, if
  -- any. Note that 'if' is sugar for 'match' on Booleans. An 'if' without an
  -- 'else' is sugar for a 'match' with a present-but-empty (i.e., identity,
  -- modulo permissions) 'else' branch, and this works out neatly in the types. A
  -- 'match' without an else branch raises 'abort', causing the 'match' to require
  -- the +Fail permission.

  Match hint _ cases else_ origin -> while (Term.origin term0) context $ do
    let constructors = case cases of
          -- Curiously, because an empty match works on any type, no
          -- constructors are actually permitted.
          [] -> []
          Case (QualifiedName ctorName) _ _ : _ ->
            case Dictionary.lookup (Instantiated ctorName []) dictionary of
              Just (Entry.Word _ _ _ (Just (Parent.Type typeName)) _ _) ->
                case Dictionary.lookup (Instantiated typeName []) dictionary of
                  Just (Entry.Type _ _ ctors _) -> ctors
                  -- TODO: Check whether this can happen if a non-constructor
                  -- word is erroneously used in a case; if this is possible, we
                  -- should generate a report rather than an error.
                  _nonType -> ice "Mlatu.Infer.inferType" "constructor not linked to type"
              _notFound -> ice "Mlatu.Infer.inferType" "constructor not found after name resolution"
          _unqualified -> ice "Mlatu.Infer.inferType" "unqualified constructor after name resolution"
    (cases', caseTypes, constructors', tenv1) <-
      foldlM inferCase' ([], [], constructors, tenv0) cases
    -- Checkpoint to halt after redundant cases are reported.
    errorCheckpoint
    (else', elseType, tenv2) <- case else_ of
      DefaultElse elseMetadata elseOrigin -> do
        (body', bodyType, tenv') <- inferType' tenv1 (defaultElseBody elseMetadata elseOrigin)
        -- The type of a match is the union of the types of the cases, and
        -- since the cases consume the scrutinee, the 'else' branch must have
        -- a dummy (fully polymorphic) type for the scrutinee. This may be
        -- easier to see when considering the type of an expression like:
        --
        --     match { else { ... } }
        --
        -- Which consumes a value of any type and always executes the 'else'
        -- branch.
        --
        -- TODO: This should be considered a drop.
        unusedScrutinee <- TypeEnv.freshTv tenv1 "MatchUnused" origin Star
        (a, b, tenv'') <- Unify.function tenv' bodyType
        let elseType =
              Type.Fun
                elseOrigin
                (Type.Prod elseOrigin a unusedScrutinee)
                b
        pure (Else body' elseOrigin, elseType, tenv'')
      Else body elseOrigin -> do
        (body', bodyType, tenv') <- inferType' tenv1 body
        -- The type of a match is the union of the types of the cases, and
        -- since the cases consume the scrutinee, the 'else' branch must have
        -- a dummy (fully polymorphic) type for the scrutinee. This may be
        -- easier to see when considering the type of an expression like:
        --
        --     match { else { ... } }
        --
        -- Which consumes a value of any type and always executes the 'else'
        -- branch.
        --
        -- TODO: This should be considered a drop.
        unusedScrutinee <- TypeEnv.freshTv tenv1 "MatchUnused" origin Star
        (a, b, tenv'') <- Unify.function tenv' bodyType
        let elseType =
              Type.Fun
                elseOrigin
                (Type.Prod elseOrigin a unusedScrutinee)
                b
        pure (Else body' elseOrigin, elseType, tenv'')
    (typ, tenv3) <- case constructors' of
      -- FIXME: Assumes caseTypes is non-empty.
      [] -> do
        let firstCase : remainingCases = caseTypes
        tenv' <-
          foldrM
            (\typ tenv -> Unify.typ tenv firstCase typ)
            tenv2
            remainingCases
        pure (Type.setOrigin origin firstCase, tenv')
      -- Only include 'else' branch if there are unhandled cases.
      _list -> do
        tenv' <-
          foldrM
            (\typ tenv -> Unify.typ tenv elseType typ)
            tenv2
            caseTypes
        pure (Type.setOrigin origin elseType, tenv')
    let type' = Zonk.typ tenvFinal typ
    pure (Match hint type' cases' else' origin, typ, tenv3)
    where
      inferCase' (cases', types, remaining, tenv) case_ = do
        (case', typ, remaining', tenv') <-
          inferCase dictionary tenvFinal tenv remaining case_
        pure (case' : cases', typ : types, remaining', tenv')

  -- A 'new' expression simply tags some fields on the stack, so the most
  -- straightforward way to type it is as an unsafe cast. For now, we can rely on
  -- the type signature of the desugared data constructor definition to make this
  -- type-safe, since only the compiler can generate 'new' expressions.

  New _ constructor size origin ->
    while (Term.origin term0) context $ do
      [a, b] <-
        fresh
          origin
          [ ("r", Stack),
            ("s", Stack)
          ]
      let typ = Type.Fun origin a b
      let type' = Zonk.typ tenvFinal typ
      pure (New type' constructor size origin, typ, tenv0)

  -- Unlike with 'new', we cannot simply type a 'new closure' expression as an
  -- unsafe cast because we need to know its effect on the stack within the body
  -- of a definition. So we type a 'new.closure.x' expression as:
  --
  --     ∀ρστα̂. ρ × α₀ × … × αₓ × (σ → τ) → ρ × (σ → τ)
  --

  NewClosure _ size origin ->
    while (Term.origin term0) context $ do
      as <-
        fresh origin $
          zip
            [ Unqualified ("Capture" <> show i)
              | i <- [1 :: Int ..]
            ]
            $ replicate size Circle
      [r, s, t] <-
        fresh
          origin
          [ ("r", Stack),
            ("ClosureIn", Stack),
            ("ClosureOut", Stack)
          ]
      let f = Type.Fun origin s t
          typ =
            Type.Fun
              origin
              (foldl' (Type.Prod origin) r (as ++ [f]))
              (Type.Prod origin r f)
          type' = Zonk.typ tenvFinal typ
      pure (NewClosure type' size origin, typ, tenv0)

  -- This is similar for 'new vector' expressions, which we type as:
  --
  --     ∀ρα. ρ × α₀ × … × αₓ → ρ × vector<α>
  --

  NewVector _ size _ origin ->
    while (Term.origin term0) context $ do
      [a, b] <-
        fresh
          origin
          [ ("r", Stack),
            ("Item", Star)
          ]
      let typ =
            Type.Fun
              origin
              (foldl' (Type.Prod origin) a (replicate size b))
              (Type.Prod origin a (TypeConstructor origin "list" :@ b))
          type' = Zonk.typ tenvFinal typ
          b' = Zonk.typ tenvFinal b
      pure (NewVector type' size b' origin, typ, tenv0)

  -- Pushing a value results in a stack with that value on top.

  Push _ value origin ->
    while (Term.origin term0) context $ do
      [a] <-
        fresh
          origin
          [ ("s", Stack)
          ]
      (value', t, tenv1) <- inferValue dictionary tenvFinal tenv0 origin value
      let typ = Type.Fun origin a (Type.Prod origin a t)
      let type' = Zonk.typ tenvFinal typ
      pure (Push type' value' origin, typ, tenv1)

  -- FIXME: Should generic parameters be restricted to none?
  Word _ name _ origin ->
    while (Term.origin term0) context $
      inferCall dictionary tenvFinal tenv0 name origin
  where
    inferType' = inferType dictionary tenvFinal
    fresh origin =
      foldrM
        (\(name, k) ts -> (: ts) <$> TypeEnv.freshTv tenv0 name origin k)
        []

    context :: Doc ()
    context = hsep ["inferring the type of", dquotes $ Pretty.printTerm term0]

-- A case in a 'match' expression is simply the inverse of a constructor:
-- whereas a constructor takes some fields from the stack and produces
-- an instance of a data type, a 'case' deconstructs an instance of a data type
-- and produces the fields on the stack for the body of the case to consume.

inferCase ::
  (Show a) =>
  Dictionary ->
  TypeEnv ->
  TypeEnv ->
  [DataConstructor] ->
  Case a ->
  M (Case Type, Type, [DataConstructor], TypeEnv)
inferCase
  dictionary
  tenvFinal
  tenv0
  dataConstructors
  (Case qualified@(QualifiedName name) body origin) = do
    (body', bodyType, tenv1) <- inferType dictionary tenvFinal tenv0 body
    (a1, b1, tenv2) <- Unify.function tenv1 bodyType
    case Map.lookup name $ view TypeEnv.sigs tenv2 of
      Just signature -> do
        (a2, b2, tenv3) <- Unify.function tenv2 signature
        -- Note that we swap the consumption and production of the constructor
        -- to get the type of the deconstructor. The body consumes the fields.
        tenv4 <- Unify.typ tenv3 a1 a2
        let typ = Type.Fun origin b2 b1
        -- FIXME: Should a case be annotated with a type?
        -- let type' = Zonk.typ tenvFinal typ
        dataConstructors' <- case partition (\ctor -> view DataConstructor.name ctor == unqualifiedName name) dataConstructors of
          ([], remaining) -> do
            report $ Report.makeError $ Report.RedundantCase origin
            pure remaining
          (_covered, remaining) -> pure remaining
        pure (Case qualified body' origin, typ, dataConstructors', tenv4)
      Nothing ->
        ice
          "Mlatu.Infer.inferCase"
          "case constructor missing signature after name resolution"
inferCase _ _ _ _ _ = ice "Mlatu.Infer.infeCase" "case of non-qualified name after name resolution"

inferValue ::
  (Show a) =>
  Dictionary ->
  TypeEnv ->
  TypeEnv ->
  Origin ->
  Value a ->
  M (Value Type, Type, TypeEnv)
inferValue dictionary tenvFinal tenv0 origin = \case
  Capture names term -> do
    let types = mapMaybe (TypeEnv.getClosed tenv0) names
    let oldClosure = view TypeEnv.closure tenv0
    let localEnv = set TypeEnv.closure types tenv0
    (term', t1, tenv1) <- inferType dictionary tenvFinal localEnv term
    let tenv2 = set TypeEnv.closure oldClosure tenv1
    pure (Capture names term', t1, tenv2)
  Character x -> pure (Character x, TypeConstructor origin "char", tenv0)
  Closed (ClosureIndex index) ->
    pure
      (Closed $ ClosureIndex index, Unsafe.fromJust (view TypeEnv.closure tenv0 !!? index), tenv0)
  Float x -> pure (Float x, TypeConstructor origin "float", tenv0)
  Integer x -> pure (Integer x, TypeConstructor origin "int", tenv0)
  Local (LocalIndex index) ->
    pure
      (Local $ LocalIndex index, Unsafe.fromJust (view TypeEnv.vs tenv0 !!? index), tenv0)
  Quotation {} -> ice "Mlatu.Infer.inferValue" "quotation should not appear during type inference"
  Name name -> case Dictionary.lookup (Instantiated name []) dictionary of
    Just (Entry.Word _ _ _ _ (Just signature) _) -> do
      typ <- typeFromSignature tenv0 signature
      pure (Name name, typ, tenv0)
    _noBinding ->
      ice "Mlatu.Infer.inferValue" "unbound word name found during type inference"
  Text x ->
    pure
      ( Text x,
        TypeConstructor origin "string",
        tenv0
      )

inferCall ::
  Dictionary ->
  TypeEnv ->
  TypeEnv ->
  GeneralName ->
  Origin ->
  M (Term Type, Type, TypeEnv)
inferCall dictionary tenvFinal tenv0 (QualifiedName name) origin =
  case Map.lookup name $ view TypeEnv.sigs tenv0 of
    Just t@Forall {} -> do
      (typ, params, tenv1) <- Instantiate.prenex tenv0 t
      let type' = Type.setOrigin origin typ
      params' <- starKinded dictionary params
      let type'' = Zonk.typ tenvFinal type'
          params'' = Zonk.typ tenvFinal <$> params'
      let mangled = QualifiedName name
      pure
        ( Word
            type''
            mangled
            params''
            origin,
          type',
          tenv1
        )
    Just {} -> ice "Mlatu.Infer.inferCall" "what is a non-quantified type doing as a type signature?"
    Nothing -> do
      report $ Report.makeError $ Report.MissingTypeSignature origin name
      halt
inferCall _dictionary _tenvFinal _tenv0 _ _ =
  ice
    "Mlatu.Infer.inferCall"
    "cannot infer type of non-qualified name"

-- | Desugars a parsed signature into an actual type. We resolve whether names
-- refer to quantified type variables or data definitions, and make stack
-- polymorphism explicit.
typeFromSignature :: TypeEnv -> Signature -> M Type
typeFromSignature tenv signature0 = do
  (typ, env) <-
    usingStateT
      SignatureEnv
        { sigEnvAnonymous = [],
          sigEnvVars = Map.empty
        }
      $ go signature0
  let forallAnonymous = Forall (Signature.origin signature0)
      forallVar (var, origin) = Forall origin var
  pure $
    foldr
      forallAnonymous
      (foldr forallVar typ $ Map.elems $ sigEnvVars env)
      $ sigEnvAnonymous env
  where
    go :: Signature -> StateT SignatureEnv M Type
    go signature = case signature of
      Signature.Application a b _ -> (:@) <$> go a <*> go b
      Signature.Bottom origin -> pure $ Type.Bottom origin
      Signature.Function as bs origin -> do
        r <- lift $ freshTypeId tenv
        let var = Var "r" r Stack
        let typeVar = TypeVar origin var
        Forall origin var <$> makeFunction origin typeVar as typeVar bs
      Signature.Quantified vars a origin -> do
        original <- get
        (envVars, vars') <-
          foldrM
            ((lift .) . declare)
            (sigEnvVars original, [])
            vars
        modify $ \env -> env {sigEnvVars = envVars}
        a' <- go a
        let result = foldr (Forall origin) a' vars'
        put original
        pure result
        where
          declare ::
            Parameter ->
            (Map Unqualified (Var, Origin), [Var]) ->
            M (Map Unqualified (Var, Origin), [Var])
          declare (Parameter varOrigin name kind) (envVars, freshVars) = do
            x <- freshTypeId tenv
            let var = Var name x kind
            pure (Map.insert name (var, varOrigin) envVars, var : freshVars)
      Signature.Variable name origin -> fromVar origin name
      Signature.StackFunction r as s bs origin -> do
        r' <- go r
        s' <- go s
        makeFunction origin r' as s' bs
      -- TODO: Verify that the type contains no free variables.
      Signature.Type typ -> pure typ

    fromVar :: Origin -> GeneralName -> StateT SignatureEnv M Type
    fromVar origin (UnqualifiedName name) = do
      existing <- gets $ Map.lookup name . sigEnvVars
      case existing of
        Just (var, varOrigin) -> pure $ TypeVar varOrigin var
        Nothing -> lift $ do
          report $ Report.makeError $ Report.CannotResolveType origin $ UnqualifiedName name
          halt
    fromVar origin (QualifiedName name) =
      pure $ TypeConstructor origin $ Constructor name
    fromVar _ _ =
      ice
        "Mlatu.Infer.typeFromSignature"
        "incorrectly resolved name in signature"

    makeFunction ::
      Origin ->
      Type ->
      [Signature] ->
      Type ->
      [Signature] ->
      StateT SignatureEnv M Type
    makeFunction origin r as s bs = do
      as' <- traverse go as
      bs' <- traverse go bs
      pure $ Type.Fun origin (stack r as') (stack s bs')
      where
        stack :: Type -> [Type] -> Type
        stack = foldl' $ Type.Prod origin

data SignatureEnv = SignatureEnv
  { sigEnvAnonymous :: ![Var],
    sigEnvVars :: !(Map Unqualified (Var, Origin))
  }

starKinded :: Dictionary -> [Type] -> M [Type]
starKinded dictionary =
  filterM $
    fmap (\x -> Circle == x || Star == x) . typeKind dictionary

-- | Infers the kind of a type.
typeKind :: Dictionary -> Type -> M Kind
typeKind dictionary = go
  where
    go :: Type -> M Kind
    go t = case t of
      TypeConstructor _origin (Constructor qualified) ->
        case Dictionary.lookup (Instantiated qualified []) dictionary of
          Just (Entry.Type _origin parameters _ctors kind) -> case parameters of
            [] -> pure kind
            _list ->
              pure $
                foldr
                  ((:->) . (\(Parameter _ _ k) -> k))
                  kind
                  parameters
          _noParameters -> case qualified of
            Qualified qualifier unqualified
              | qualifier == Vocabulary.global -> case unqualified of
                "BOTTOM" -> pure Stack
                "FUN" -> pure $ Stack :-> Stack :-> Star
                "PROD" -> pure $ Stack :-> Star :-> Stack
                "SUM" -> pure $ Star :-> Star :-> Star
                _noKind ->
                  ice "Mlatu.Infer.typeKind" "can't infer kind of constructor in dictionary"
            _noKInd -> ice "Mlatu.Infer.typeKind" "can't infer kind of constructor in dictionary"
      TypeVar _origin (Var _name _ k) -> pure k
      TypeConstant _origin (Var _name _ k) -> pure k
      Forall _origin _ t' -> go t'
      a :@ _ -> do
        ka <- go a
        case ka of
          _ :-> k -> pure k
          _nonConstructor ->
            ice "Mlatu.Infer.typeKind" "applying type of non-constructor kind to type"

capitalize :: Text -> Text
capitalize x
  | Text.null x = x
  | otherwise =
    Text.toUpper (one (Text.head x)) <> Text.tail x
