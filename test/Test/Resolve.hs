module Test.Resolve
  ( spec,
  )
where

import Mlatu (fragmentFromSource)
import Mlatu.Definition (Definition (Definition))
import Mlatu.Definition qualified as Definition
import Mlatu.Dictionary qualified as Dictionary
import Mlatu.Enter qualified as Enter
import Mlatu.Entry qualified as Entry
import Mlatu.Entry.Category qualified as Category
import Mlatu.Entry.Merge qualified as Merge
import Mlatu.Entry.Parameter (Parameter (..))
import Mlatu.Fragment qualified as Fragment
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Kind (Kind (..))
import Mlatu.Monad (runMlatuExceptT)
import Mlatu.Name
  ( GeneralName (QualifiedName, UnqualifiedName),
    Qualified (Qualified),
    Qualifier (..),
    Root (Absolute, Relative),
  )
import Mlatu.Operator qualified as Operator
import Mlatu.Origin qualified as Origin
import Mlatu.Pretty (printEntry, printGeneralName, printInstantiated, printQualified, printQualifier, printSignature)
import Mlatu.Report (human)
import Mlatu.Resolve qualified as Resolve
import Mlatu.Signature qualified as Signature
import Mlatu.Term qualified as Term
import Mlatu.Vocabulary qualified as Vocabulary
import Prettyprinter (dquotes, hsep, list, tupled)
import Relude
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec (Spec, it)
import Optics

spec :: Spec
spec = do
  it "resolves global words" $ do
    testWord
      "define f (->) {}"
      Vocabulary.global
      (UnqualifiedName "f")
      (Qualified Vocabulary.global "f")
    testWord
      "define f (->) {}"
      (Qualifier Absolute ["v"])
      (UnqualifiedName "f")
      (Qualified Vocabulary.global "f")
    testWord
      "define f (->) {}"
      (Qualifier Absolute ["v1", "v2"])
      (UnqualifiedName "f")
      (Qualified Vocabulary.global "f")

  it "resolves words in the same vocabulary" $ do
    testWord
      "vocab v { define f (->) {} }"
      (Qualifier Absolute ["v"])
      (UnqualifiedName "f")
      (Qualified (Qualifier Absolute ["v"]) "f")
    testWord
      "vocab v1 { vocab v2 { define f (->) {} } }"
      (Qualifier Absolute ["v1", "v2"])
      (UnqualifiedName "f")
      (Qualified (Qualifier Absolute ["v1", "v2"]) "f")
    testWord
      "vocab v1::v2 { define f (->) {} }"
      (Qualifier Absolute ["v1", "v2"])
      (UnqualifiedName "f")
      (Qualified (Qualifier Absolute ["v1", "v2"]) "f")

  it "resolves words in nested vocabularies" $ do
    testWord
      "vocab v1::v2 { define f (->) {} }"
      (Qualifier Absolute ["v1"])
      (QualifiedName (Qualified (Qualifier Relative ["v2"]) "f"))
      (Qualified (Qualifier Absolute ["v1", "v2"]) "f")
    testWord
      "vocab v1::v2::v3 { define f (->) {} }"
      (Qualifier Absolute ["v1"])
      (QualifiedName (Qualified (Qualifier Relative ["v2", "v3"]) "f"))
      (Qualified (Qualifier Absolute ["v1", "v2", "v3"]) "f")
    testWord
      "vocab v1::v2::v3 { define f (->) {} }"
      (Qualifier Absolute ["v1", "v2"])
      (QualifiedName (Qualified (Qualifier Relative ["v3"]) "f"))
      (Qualified (Qualifier Absolute ["v1", "v2", "v3"]) "f")

  it "resolves global types" $ do
    testType
      "type T {}"
      Vocabulary.global
      (UnqualifiedName "T")
      (Qualified Vocabulary.global "T")
    testType
      "type T {}"
      (Qualifier Absolute ["v"])
      (UnqualifiedName "T")
      (Qualified Vocabulary.global "T")
    testType
      "type T {}"
      (Qualifier Absolute ["v1", "v2"])
      (UnqualifiedName "T")
      (Qualified Vocabulary.global "T")

  it "resolves types in the same vocabulary" $ do
    testType
      "vocab v { type T {} }"
      (Qualifier Absolute ["v"])
      (UnqualifiedName "T")
      (Qualified (Qualifier Absolute ["v"]) "T")
    testType
      "vocab v1 { vocab v2 { type T {} } }"
      (Qualifier Absolute ["v1", "v2"])
      (UnqualifiedName "T")
      (Qualified (Qualifier Absolute ["v1", "v2"]) "T")
    testType
      "vocab v1::v2 { type T {} }"
      (Qualifier Absolute ["v1", "v2"])
      (UnqualifiedName "T")
      (Qualified (Qualifier Absolute ["v1", "v2"]) "T")

  it "resolves types in nested vocabularies" $ do
    testType
      "vocab v1::v2 { type T {} }"
      (Qualifier Absolute ["v1"])
      (QualifiedName (Qualified (Qualifier Relative ["v2"]) "T"))
      (Qualified (Qualifier Absolute ["v1", "v2"]) "T")
    testType
      "vocab v1::v2::v3 { type T {} }"
      (Qualifier Absolute ["v1"])
      (QualifiedName (Qualified (Qualifier Relative ["v2", "v3"]) "T"))
      (Qualified (Qualifier Absolute ["v1", "v2", "v3"]) "T")
    testType
      "vocab v1::v2::v3 { type T {} }"
      (Qualifier Absolute ["v1", "v2"])
      (QualifiedName (Qualified (Qualifier Relative ["v3"]) "T"))
      (Qualified (Qualifier Absolute ["v1", "v2", "v3"]) "T")

  it "resolves types in trait signatures" $ do
    testType
      "type Size {}\n\
      \trait alignment[T] (T -> Size)"
      Vocabulary.global
      (UnqualifiedName "Size")
      (Qualified Vocabulary.global "Size")

testWord :: Text -> Qualifier -> GeneralName -> Qualified -> IO ()
testWord contextSource viewpoint name expected = do
  dictionary <- runMlatuExceptT $ do
    context <- fragmentFromSource [] Nothing 1 "<common>" contextSource
    contextDictionary <- Enter.fragment context Dictionary.empty
    let origin = Origin.point "<test>" 0 0
        fragment =
          set Fragment.definitions (one
                  Definition
                    { Definition._body = Term.Word () Operator.Postfix name [] origin,
                      Definition._category = Category.Word,
                      Definition._fixity = Operator.Postfix,
                      Definition._inferSignature = False,
                      Definition._merge = Merge.Deny,
                      Definition._name = Qualified viewpoint "test",
                      Definition._origin = origin,
                      Definition._parent = Nothing,
                      Definition._signature =
                        Signature.Quantified
                          [Parameter origin "R" Stack Nothing]
                          ( Signature.StackFunction
                              (Signature.Variable "R" origin)
                              []
                              (Signature.Variable "R" origin)
                              []
                              []
                              origin
                          )
                          origin
                    }) mempty
    Enter.fragment fragment contextDictionary
  case Dictionary.toList <$> dictionary of
    Right definitions -> case find matching definitions of
      Just (_, Entry.Word _ _ _ _ _ (Just term))
        | [Term.Word _ _ name' _ _] <- Term.decompose term ->
          let message =
                show $
                  hsep
                    [ printGeneralName name,
                      "resolves to",
                      printQualified expected,
                      "within",
                      printQualifier viewpoint
                    ]
           in assertEqual message (QualifiedName expected) name'
      _ ->
        assertFailure $
          show $
            hsep
              ["missing test word definition:", list $ fmap (\(i, e) -> tupled [printInstantiated i, printEntry e]) definitions]
      where
        matching (Instantiated (Qualified v "test") _, _)
          | v == viewpoint =
            True
        matching _ = False
    Left reports ->
      assertFailure $
        toString $
          unlines $
            fmap (show . human) reports

testType :: Text -> Qualifier -> GeneralName -> Qualified -> IO ()
testType contextSource viewpoint name expected = do
  resolved <- runMlatuExceptT $ do
    context <- fragmentFromSource [] Nothing 1 "<common>" contextSource
    contextDictionary <- Enter.fragment context Dictionary.empty
    let origin = Origin.point "<test>" 0 0
    Resolve.run $
      Resolve.signature
        contextDictionary
        viewpoint
        (Signature.Variable name origin)
  case resolved of
    Right (Signature.Variable name' _) ->
      let message =
            show $
              hsep
                [ printGeneralName name,
                  "resolves to",
                  printQualified expected,
                  "within",
                  printQualifier viewpoint
                ]
       in assertEqual message (QualifiedName expected) name'
    Right result ->
      assertFailure $
        show $
          hsep
            [ "signature variable",
              dquotes $ printGeneralName name,
              "resolved to non-variable",
              printSignature result
            ]
    Left reports ->
      assertFailure $
        toString $
          unlines $
            fmap (show . human) reports
