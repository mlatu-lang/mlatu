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
import Mlatu.Bits
  ( FloatBits (Float32, Float64),
    IntegerBits
      ( Signed16,
        Signed32,
        Signed64,
        Signed8,
        Unsigned16,
        Unsigned32,
        Unsigned64,
        Unsigned8
      ),
  )
import Mlatu.DataConstructor (DataConstructor)
import Mlatu.DataConstructor qualified as DataConstructor
import Mlatu.Dictionary (Dictionary)
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Parameter (Parameter (Parameter))
import Mlatu.Entry.Parent qualified as Parent
import Mlatu.Informer (Informer (..), errorCheckpoint)
import Mlatu.InstanceCheck (instanceCheck)
import Mlatu.Instantiate qualified as Instantiate
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Kind (Kind (..))
import Mlatu.Literal qualified as Literal
import Mlatu.Monad (K)
import Mlatu.Name (ClosureIndex (..), GeneralName (..), LocalIndex (..), Qualified (..), Unqualified (..))
import Mlatu.Operator qualified as Operator
import Mlatu.Origin (Origin)
import Mlatu.Pretty qualified as Pretty
import Mlatu.Regeneralize (regeneralize)
import Mlatu.Report qualified as Report
import Mlatu.Signature (Signature)
import Mlatu.Signature qualified as Signature
import Mlatu.Term (Case (..), Else (..), Term (..), Value (..))
import Mlatu.Term qualified as Term
import Mlatu.Type (Constructor (..), Type (..), Var (..))
import Mlatu.Type qualified as Type
import Mlatu.TypeEnv (TypeEnv, freshTypeId)
import Mlatu.TypeEnv qualified as TypeEnv
import Mlatu.Unify qualified as Unify
import Mlatu.Vocabulary qualified as Vocabulary
import Mlatu.Zonk qualified as Zonk
import Relude hiding (Compose, Type)
import Relude.Unsafe qualified as Unsafe
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

-- | Type inference takes a program fragment and produces a program with every
-- term annotated with its inferred type. It's polymorphic in the annotation
-- type of its input so that it can't depend on those annotations.
typecheck ::
  -- | Current dictionary, for context.
  Dictionary ->
  -- | Optional signature to check inferred type against.
  Maybe Signature ->
  -- | Term to infer.
  Term a ->
  -- | Type-annotated term and its inferred type.
  K (Term Type, Type)
typecheck dictionary mDeclaredSignature term = do
  let tenv0 = TypeEnv.empty
  declaredType <- traverse (typeFromSignature tenv0) mDeclaredSignature
  declaredTypes <-
    mapM
      (\(name, signature) -> (,) name <$> typeFromSignature tenv0 signature)
      $ Dictionary.signatures dictionary
  let tenv1 =
        tenv0
          { TypeEnv.sigs =
              Map.union (Map.fromList declaredTypes) $
                TypeEnv.sigs tenv0
          }
  inferType0 dictionary tenv1 declaredType term

-- | Mangles an instance name according to its trait signature.
mangleInstance ::
  Dictionary -> Qualified -> Signature -> Signature -> K Instantiated
mangleInstance dictionary name instanceSignature traitSignature = do
  let tenv0 = TypeEnv.empty
  instanceType <- typeFromSignature tenv0 instanceSignature
  traitType <- typeFromSignature tenv0 traitSignature
  instanceCheck "trait" traitType "instance" instanceType
  (traitType', args, tenv1) <- Instantiate.prenex tenv0 traitType
  tenv2 <- Unify.typ tenv1 instanceType traitType'
  args' <- valueKinded dictionary $ map (Zonk.typ tenv2) args
  return $ Instantiated name args'

-- | Since type variables can be generalized if they do not depend on the
-- initial state of the typing environment, the type of a single definition is
-- inferred in an empty environment so that it can be trivially generalized. It
-- is then regeneralized to increase stack polymorphism.
inferType0 ::
  -- | Current dictionary, for context.
  Dictionary ->
  -- | Current typing environment.
  TypeEnv ->
  -- | Optional type to check inferred type against.
  Maybe Type ->
  -- | Term to infer.
  Term a ->
  -- | Type-annotated term and its inferred type.
  K (Term Type, Type)
inferType0 dictionary tenv mDeclared term = do
  rec (term', t, tenvFinal) <- inferType dictionary tenvFinal' tenv term
      tenvFinal' <- maybe (return tenvFinal) (Unify.typ tenvFinal t) mDeclared
  let zonked = Zonk.typ tenvFinal' t
  let regeneralized = regeneralize tenvFinal' zonked
  forM_ mDeclared (instanceCheck "inferred" regeneralized "declared")

  return (Zonk.term tenvFinal' term', regeneralized)

-- We infer the type of a term and annotate each terminal with the inferred type
-- as we go. We ignore any existing annotations because a definition may need to
-- be re-inferred and re-annotated if a program is updated with a new
-- implementation of an existing definition.

inferType ::
  Dictionary ->
  TypeEnv ->
  TypeEnv ->
  Term a ->
  K (Term Type, Type, TypeEnv)
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
      [a, p] <-
        fresh
          origin
          [ ("S", Stack),
            ("P", Permission)
          ]
      let typ = Type.fun origin a a p
      let type' = Zonk.typ tenvFinal typ
      return (Coercion hint type' origin, typ, tenv0)
  Coercion hint@(Term.AnyCoercion sig) _ origin ->
    while (Term.origin term0) context $ do
      typ <- typeFromSignature tenv0 sig
      let type' = Zonk.typ tenvFinal typ
      return (Coercion hint type' origin, typ, tenv0)

  -- The type of the composition of two expressions is the composition of the
  -- types of those expressions.

  Compose _ term1 term2 -> do
    (term1', t1, tenv1) <- inferType' tenv0 term1
    (term2', t2, tenv2) <- inferType' tenv1 term2
    (a, b, e1, tenv3) <- Unify.function tenv2 t1
    (c, d, e2, tenv4) <- Unify.function tenv3 t2
    tenv5 <- Unify.typ tenv4 b c
    tenv6 <- Unify.typ tenv5 e1 e2
    -- FIXME: Use range origin over whole composition?
    let origin = Term.origin term1
    let typ = Type.fun origin a d e1
    let type' = Zonk.typ tenvFinal typ
    return (Compose type' term1' term2', typ, tenv6)

  -- TODO: Verify that this is correct.
  Generic _name _ t _ -> inferType' tenv0 t
  Group {} ->
    error
      "group expression should not appear during type inference"
  -- A local variable binding in Mlatu is in fact a lambda term in the ordinary
  -- lambda-calculus sense. We infer the type of its body in the environment
  -- extended with a fresh local bound to a fresh type variable, and produce a
  -- type of the form 'R..., A -> S... +P'.

  Lambda _ name@(Unqualified unqualified) _ term origin -> do
    let varTypeName = Unqualified ("Local" <> capitalize unqualified)
    a <- TypeEnv.freshTv tenv0 varTypeName origin Value
    let oldLocals = TypeEnv.vs tenv0
    let localEnv = tenv0 {TypeEnv.vs = a : TypeEnv.vs tenv0}
    (term', t1, tenv1) <- inferType' localEnv term
    let tenv2 = tenv1 {TypeEnv.vs = oldLocals}
    (b, c, e, tenv3) <- Unify.function tenv2 t1
    let typ = Type.fun origin (Type.prod origin b a) c e
        type' = Zonk.typ tenvFinal typ
        varType' = Zonk.typ tenvFinal a
    return
      ( Lambda type' name varType' term' origin,
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
                  Just (Entry.Type _ _ ctors) -> ctors
                  -- TODO: Check whether this can happen if a non-constructor
                  -- word is erroneously used in a case; if this is possible, we
                  -- should generate a report rather than an error.
                  _nonType -> error "constructor not linked to type"
              _notFound -> error "constructor not found after name resolution"
          _unqualified -> error "unqualified constructor after name resolution"
    (cases', caseTypes, constructors', tenv1) <-
      foldlM inferCase' ([], [], constructors, tenv0) cases
    -- Checkpoint to halt after redundant cases are reported.
    errorCheckpoint
    (else', elseType, tenv2) <- case else_ of
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
        unusedScrutinee <- TypeEnv.freshTv tenv1 "MatchUnused" origin Value
        (a, b, e, tenv'') <- Unify.function tenv' bodyType
        let elseType =
              Type.fun
                elseOrigin
                (Type.prod elseOrigin a unusedScrutinee)
                b
                e
        return (Else body' elseOrigin, elseType, tenv'')
    (typ, tenv3) <- case constructors' of
      -- FIXME: Assumes caseTypes is non-empty.
      [] -> do
        let firstCase : remainingCases = caseTypes
        tenv' <-
          foldrM
            (\typ tenv -> Unify.typ tenv firstCase typ)
            tenv2
            remainingCases
        return (Type.setOrigin origin firstCase, tenv')
      -- Only include 'else' branch if there are unhandled cases.
      _list -> do
        tenv' <-
          foldrM
            (\typ tenv -> Unify.typ tenv elseType typ)
            tenv2
            caseTypes
        return (Type.setOrigin origin elseType, tenv')
    let type' = Zonk.typ tenvFinal typ
    return (Match hint type' cases' else' origin, typ, tenv3)
    where
      inferCase' (cases', types, remaining, tenv) case_ = do
        (case', typ, remaining', tenv') <-
          inferCase dictionary tenvFinal tenv remaining case_
        return (case' : cases', typ : types, remaining', tenv')

  -- A 'new' expression simply tags some fields on the stack, so the most
  -- straightforward way to type it is as an unsafe cast. For now, we can rely on
  -- the type signature of the desugared data constructor definition to make this
  -- type-safe, since only the compiler can generate 'new' expressions.

  New _ constructor size origin ->
    while (Term.origin term0) context $ do
      [a, b, e] <-
        fresh
          origin
          [ ("R", Stack),
            ("S", Stack),
            ("P", Permission)
          ]
      let typ = Type.fun origin a b e
      let type' = Zonk.typ tenvFinal typ
      return (New type' constructor size origin, typ, tenv0)

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
            $ replicate size Value
      [r, s, t, p1, p2] <-
        fresh
          origin
          [ ("R", Stack),
            ("ClosureIn", Stack),
            ("ClosureOut", Stack),
            ("ClosurePermissions", Permission),
            ("P", Permission)
          ]
      let f = Type.fun origin s t p1
          typ =
            Type.fun
              origin
              (foldl' (Type.prod origin) r (as ++ [f]))
              (Type.prod origin r f)
              p2
          type' = Zonk.typ tenvFinal typ
      return (NewClosure type' size origin, typ, tenv0)

  -- This is similar for 'new vector' expressions, which we type as:
  --
  --     ∀ρα. ρ × α₀ × … × αₓ → ρ × vector<α>
  --

  NewVector _ size _ origin ->
    while (Term.origin term0) context $ do
      [a, b, e] <-
        fresh
          origin
          [ ("R", Stack),
            ("Item", Value),
            ("P", Permission)
          ]
      let typ =
            Type.fun
              origin
              (foldl' (Type.prod origin) a (replicate size b))
              (Type.prod origin a (TypeConstructor origin "List" :@ b))
              e
          type' = Zonk.typ tenvFinal typ
          b' = Zonk.typ tenvFinal b
      return (NewVector type' size b' origin, typ, tenv0)

  -- Pushing a value results in a stack with that value on top.

  Push _ value origin ->
    while (Term.origin term0) context $ do
      [a, e] <-
        fresh
          origin
          [ ("S", Stack),
            ("P", Permission)
          ]
      (value', t, tenv1) <- inferValue dictionary tenvFinal tenv0 origin value
      let typ = Type.fun origin a (Type.prod origin a t) e
      let type' = Zonk.typ tenvFinal typ
      return (Push type' value' origin, typ, tenv1)

  -- FIXME: Should generic parameters be restricted to none?
  Word _ _fixity name _ origin ->
    while (Term.origin term0) context $
      inferCall dictionary tenvFinal tenv0 name origin
  where
    inferType' = inferType dictionary tenvFinal
    fresh origin =
      foldrM
        (\(name, k) ts -> (: ts) <$> TypeEnv.freshTv tenv0 name origin k)
        []

    context :: Pretty.Doc
    context = Pretty.hsep ["inferring the type of", Pretty.quote term0]

-- A case in a 'match' expression is simply the inverse of a constructor:
-- whereas a constructor takes some fields from the stack and produces
-- an instance of a data type, a 'case' deconstructs an instance of a data type
-- and produces the fields on the stack for the body of the case to consume.

inferCase ::
  Dictionary ->
  TypeEnv ->
  TypeEnv ->
  [DataConstructor] ->
  Case a ->
  K (Case Type, Type, [DataConstructor], TypeEnv)
inferCase
  dictionary
  tenvFinal
  tenv0
  dataConstructors
  (Case qualified@(QualifiedName name) body origin) = do
    (body', bodyType, tenv1) <- inferType dictionary tenvFinal tenv0 body
    (a1, b1, e1, tenv2) <- Unify.function tenv1 bodyType
    case Map.lookup name $ TypeEnv.sigs tenv2 of
      Just signature -> do
        (a2, b2, e2, tenv3) <- Unify.function tenv2 signature
        -- Note that we swap the consumption and production of the constructor
        -- to get the type of the deconstructor. The body consumes the fields.
        tenv4 <- Unify.typ tenv3 a1 a2
        tenv5 <- Unify.typ tenv4 e1 e2
        let typ = Type.fun origin b2 b1 e1
        -- FIXME: Should a case be annotated with a type?
        -- let type' = Zonk.typ tenvFinal typ
        let matching ctor = DataConstructor.name ctor == unqualifiedName name
        dataConstructors' <- case partition matching dataConstructors of
          ([], remaining) -> do
            report $ Report.makeError $ Report.RedundantCase origin
            return remaining
          (_covered, remaining) -> return remaining
        return (Case qualified body' origin, typ, dataConstructors', tenv5)
      Nothing ->
        error
          "case constructor missing signature after name resolution"
inferCase _ _ _ _ _ = error "case of non-qualified name after name resolution"

inferValue ::
  Dictionary ->
  TypeEnv ->
  TypeEnv ->
  Origin ->
  Value a ->
  K (Value Type, Type, TypeEnv)
inferValue dictionary tenvFinal tenv0 origin = \case
  Capture names term -> do
    let types = mapMaybe (TypeEnv.getClosed tenv0) names
    let oldClosure = TypeEnv.closure tenv0
    let localEnv = tenv0 {TypeEnv.closure = types}
    (term', t1, tenv1) <- inferType dictionary tenvFinal localEnv term
    let tenv2 = tenv1 {TypeEnv.closure = oldClosure}
    return (Capture names term', t1, tenv2)
  Character x -> return (Character x, TypeConstructor origin "Char", tenv0)
  Closed (ClosureIndex index) ->
    return
      (Closed $ ClosureIndex index, Unsafe.fromJust (TypeEnv.closure tenv0 !!? index), tenv0)
  Float x ->
    let ctor = case Literal.floatBits x of
          Float32 -> "Float32"
          Float64 -> "Float64"
     in return (Float x, TypeConstructor origin ctor, tenv0)
  Integer x ->
    let ctor = case Literal.integerBits x of
          Signed8 -> "Int8"
          Signed16 -> "Int16"
          Signed32 -> "Int32"
          Signed64 -> "Int64"
          Unsigned8 -> "UInt8"
          Unsigned16 -> "UInt16"
          Unsigned32 -> "UInt32"
          Unsigned64 -> "UInt64"
     in return (Integer x, TypeConstructor origin ctor, tenv0)
  Local (LocalIndex index) ->
    return
      (Local $ LocalIndex index, Unsafe.fromJust (TypeEnv.vs tenv0 !!? index), tenv0)
  Quotation {} -> error "quotation should not appear during type inference"
  Name name -> case Dictionary.lookup (Instantiated name []) dictionary of
    Just (Entry.Word _ _ _ _ (Just signature) _) -> do
      typ <- typeFromSignature tenv0 signature
      return (Name name, typ, tenv0)
    _noBinding ->
      error $
        toText $
          Pretty.render $
            Pretty.hsep
              [ "unbound word name",
                Pretty.quote name,
                "found during type inference"
              ]
  Text x ->
    return
      ( Text x,
        TypeConstructor origin "String",
        tenv0
      )

inferCall ::
  Dictionary ->
  TypeEnv ->
  TypeEnv ->
  GeneralName ->
  Origin ->
  K (Term Type, Type, TypeEnv)
inferCall dictionary tenvFinal tenv0 (QualifiedName name) origin =
  case Map.lookup name $ TypeEnv.sigs tenv0 of
    Just t@Forall {} -> do
      (typ, params, tenv1) <- Instantiate.prenex tenv0 t
      let type' = Type.setOrigin origin typ
      params' <- valueKinded dictionary params
      let type'' = Zonk.typ tenvFinal type'
          params'' = map (Zonk.typ tenvFinal) params'
      let mangled = QualifiedName name
      return
        ( Word
            type''
            Operator.Postfix
            mangled
            params''
            origin,
          type',
          tenv1
        )
    Just {} -> error "what is a non-quantified type doing as a type signature?"
    Nothing -> do
      report $ Report.makeError $ Report.MissingTypeSignature origin name
      halt
inferCall _dictionary _tenvFinal _tenv0 name _ =
  -- FIXME: Use proper reporting. (Internal error?)
  error $
    toText $
      Pretty.render $
        Pretty.hsep
          ["cannot infer type of non-qualified name", Pretty.quote name]

-- | Desugars a parsed signature into an actual type. We resolve whether names
-- refer to quantified type variables or data definitions, and make stack
-- polymorphism explicit.
typeFromSignature :: TypeEnv -> Signature -> K Type
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
  return $
    foldr
      forallAnonymous
      (foldr forallVar typ $ Map.elems $ sigEnvVars env)
      $ sigEnvAnonymous env
  where
    go :: Signature -> StateT SignatureEnv K Type
    go signature = case signature of
      Signature.Application a b _ -> (:@) <$> go a <*> go b
      Signature.Bottom origin -> return $ Type.bottom origin
      Signature.Function as bs es origin -> do
        r <- lift $ freshTypeId tenv
        let var = Var "R" r Stack
        let typeVar = TypeVar origin var
        es' <- mapM (fromVar origin) es
        (me, es'') <- lift $ permissionVar origin es'
        Forall origin var <$> makeFunction origin typeVar as typeVar bs es'' me
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
        return result
        where
          declare ::
            Parameter ->
            (Map Unqualified (Var, Origin), [Var]) ->
            K (Map Unqualified (Var, Origin), [Var])
          declare (Parameter varOrigin name kind) (envVars, freshVars) = do
            x <- freshTypeId tenv
            let var = Var name x kind
            return (Map.insert name (var, varOrigin) envVars, var : freshVars)
      Signature.Variable name origin -> fromVar origin name
      Signature.StackFunction r as s bs es origin -> do
        let var = fromVar origin
        r' <- go r
        s' <- go s
        es' <- mapM var es
        (me, es'') <- lift $ permissionVar origin es'
        makeFunction origin r' as s' bs es'' me
      -- TODO: Verify that the type contains no free variables.
      Signature.Type typ -> return typ

    permissionVar :: Origin -> [Type] -> K (Maybe Type, [Type])
    permissionVar origin types = case splitFind isTypeVar types of
      Just (preceding, typ, following) -> case find isTypeVar following of
        Nothing -> return (Just typ, preceding ++ following)
        Just type' -> do
          report $ Report.makeError $ Report.MultiplePermissionVariables origin typ type'
          halt
      Nothing -> return (Nothing, types)
      where
        isTypeVar TypeVar {} = True
        isTypeVar _ = False

    fromVar :: Origin -> GeneralName -> StateT SignatureEnv K Type
    fromVar origin (UnqualifiedName name) = do
      existing <- gets $ Map.lookup name . sigEnvVars
      case existing of
        Just (var, varOrigin) -> return $ TypeVar varOrigin var
        Nothing -> lift $ do
          report $ Report.makeError $ Report.CannotResolveType origin $ UnqualifiedName name
          halt
    fromVar origin (QualifiedName name) =
      return $ TypeConstructor origin $ Constructor name
    fromVar _ name =
      error $
        toText $
          "incorrectly resolved name in signature: " ++ show name

    makeFunction ::
      Origin ->
      Type ->
      [Signature] ->
      Type ->
      [Signature] ->
      [Type] ->
      Maybe Type ->
      StateT SignatureEnv K Type
    makeFunction origin r as s bs es me = do
      as' <- mapM go as
      bs' <- mapM go bs
      e <- case me of
        Just e -> return e
        Nothing -> do
          ex <- lift $ freshTypeId tenv
          let var = Var "P" ex Permission
          modify $ \env -> env {sigEnvAnonymous = var : sigEnvAnonymous env}
          return $ TypeVar origin var
      return $
        Type.fun origin (stack r as') (stack s bs') $
          foldr (Type.join origin) e es
      where
        stack :: Type -> [Type] -> Type
        stack = foldl' $ Type.prod origin

splitFind :: (Eq a) => (a -> Bool) -> [a] -> Maybe ([a], a, [a])
splitFind f = go []
  where
    go acc (x : xs)
      | f x = Just (reverse acc, x, xs)
      | otherwise = go (x : acc) xs
    go _ [] = Nothing

data SignatureEnv = SignatureEnv
  { sigEnvAnonymous :: ![Var],
    sigEnvVars :: !(Map Unqualified (Var, Origin))
  }

valueKinded :: Dictionary -> [Type] -> K [Type]
valueKinded dictionary =
  filterM $
    fmap (Value ==) . typeKind dictionary

-- | Infers the kind of a type.
typeKind :: Dictionary -> Type -> K Kind
typeKind dictionary = go
  where
    go :: Type -> K Kind
    go t = case t of
      TypeConstructor _origin (Constructor qualified) ->
        case Dictionary.lookup (Instantiated qualified []) dictionary of
          Just (Entry.Type _origin parameters _ctors) -> case parameters of
            [] -> return Value
            _list ->
              return $
                foldr
                  ((:->) . (\(Parameter _ _ k) -> k))
                  Value
                  parameters
          _noParameters -> case qualified of
            Qualified qualifier unqualified
              | qualifier == Vocabulary.global -> case unqualified of
                "Bottom" -> return Stack
                "Fun" -> return $ Stack :-> Stack :-> Permission :-> Value
                "Prod" -> return $ Stack :-> Value :-> Stack
                "Sum" -> return $ Value :-> Value :-> Value
                "Unsafe" -> return Label
                "Void" -> return Value
                "IO" -> return Label
                "Fail" -> return Label
                "Join" -> return $ Label :-> Permission :-> Permission
                "List" -> return $ Value :-> Value
                _noKind ->
                  error $
                    toText $
                      Pretty.render $
                        Pretty.hsep
                          [ "can't infer kind of constructor",
                            Pretty.quote qualified,
                            "in dictionary",
                            pPrint dictionary
                          ]
            -- TODO: Better error reporting.
            _noKInd ->
              error $
                toText $
                  Pretty.render $
                    Pretty.hsep
                      [ "can't infer kind of constructor",
                        Pretty.quote qualified,
                        "in dictionary",
                        pPrint dictionary
                      ]
      TypeValue {} -> error "TODO: infer kind of type value"
      TypeVar _origin (Var _name _ k) -> return k
      TypeConstant _origin (Var _name _ k) -> return k
      Forall _origin _ t' -> go t'
      a :@ b -> do
        ka <- go a
        case ka of
          _ :-> k -> return k
          -- TODO: Better error reporting.
          _nonConstructor ->
            error $
              toText $
                Pretty.render $
                  Pretty.hsep
                    [ "applying type",
                      Pretty.quote a,
                      "of non-constructor kind",
                      Pretty.quote ka,
                      "to type",
                      Pretty.quote b
                    ]

capitalize :: Text -> Text
capitalize x
  | Text.null x = x
  | otherwise =
    Text.toUpper (one (Text.head x)) <> Text.tail x
