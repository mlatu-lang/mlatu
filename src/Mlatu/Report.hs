{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Report
-- Description : Error reports
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Report
  ( NameCategory (..),
    Report (..),
    Level (..),
    ReportKind (..),
    parseError,
    makeError,
    makeWarning,
    human,
    _MissingTypeSignature,
    _MultiplePermissionVariables,
    _CannotResolveType,
    _FailedInstanceCheck,
    _MissingPermissionLabel,
    _TypeArgumentCountMismatch,
    _CannotResolveName,
    _MultipleDefinitions,
    _WordRedefinition,
    _WordRedeclaration,
    _TypeMismatch,
    _RedundantCase,
    _Chain,
    _OccursCheckFailure,
    _StackDepthMismatch,
    _InvalidOperatorMetadata,
    _ParseError,
    _UseCommon,
    _Context
  )
where

import GHC.List (span)
import Mlatu.Name (GeneralName, Qualified)
import Mlatu.Origin (Origin)
import Mlatu.Origin qualified as Origin
import Mlatu.Pretty (printConstructor, printGeneralName, printOrigin, printQualified, printSignature, printTerm, printType)
import Mlatu.Signature (Signature)
import Mlatu.Term (Term)
import Mlatu.Term qualified as Term
import Mlatu.Type (Constructor, Type)
import Mlatu.Type qualified as Type
import Prettyprinter (Doc, Pretty (pretty), comma, dquotes, hsep, list, parens, punctuate, vsep, (<+>), colon)
import Relude hiding (Type)
import Text.Parsec qualified as Parsec
import Text.Parsec.Error qualified as Parsec
import Optics.TH (makePrisms)

data NameCategory = WordName | TypeName
  deriving (Eq, Show)

data Level
  = Info
  | Warn
  | Error
  deriving (Ord, Eq, Show)

data Report = Report Level ReportKind
  deriving (Eq, Show)

makeError :: ReportKind -> Report
makeError = Report Error

makeWarning :: ReportKind -> Report
makeWarning = Report Warn

instance Eq (Doc ()) where
  d1 == d2 = (show d1 :: Text) == (show d2 :: Text)

data ReportKind
  = MissingTypeSignature !Origin !Qualified
  | MultiplePermissionVariables !Origin !Type !Type
  | CannotResolveType !Origin !GeneralName
  | FailedInstanceCheck !Type !Type
  | MissingPermissionLabel !Type !Type !Origin !Constructor
  | TypeArgumentCountMismatch !(Term Type) ![Type]
  | CannotResolveName !Origin !NameCategory !GeneralName
  | MultipleDefinitions !Origin !Qualified ![Origin]
  | WordRedefinition !Origin !Qualified !Origin
  | WordRedeclaration !Origin !Qualified !Signature !Origin !(Maybe Signature)
  | TypeMismatch !Type !Type
  | RedundantCase !Origin
  | Chain ![ReportKind]
  | OccursCheckFailure !Type !Type
  | StackDepthMismatch !Origin
  | InvalidOperatorMetadata !Origin !Qualified !(Term ())
  | ParseError !Origin ![Doc ()] !(Doc ())
  | UseCommon !Origin !Qualified
  | Context ![(Origin, Doc ())] !Report
  deriving (Eq, Show)

makePrisms ''ReportKind

human :: Report -> Doc ()
human (Report _ kind) = kindMsg kind
  where
    kindMsg = \case
      (MissingTypeSignature origin name) ->
        showOriginPrefix origin
          <+> "I can't find a type signature for the word"
          <+> dquotes (printQualified name)
      (MultiplePermissionVariables origin a b) ->
        hsep
          [ showOriginPrefix origin,
            "I found multiple permission variables:",
            dquotes $ printType a,
            "and",
            dquotes $ printType b,
            "but only one is allowed per function"
          ]
      (CannotResolveType origin name) ->
        hsep
          [ showOriginPrefix origin,
            "I can't tell which type",
            dquotes $ printGeneralName name,
            "refers to",
            parens "did you mean to add it as a type parameter?"
          ]
      (FailedInstanceCheck a b) ->
        hsep
          -- TODO: Show type kind.
          [ "I expected",
            dquotes $ printType a,
            "to be at least as polymorphic as",
            dquotes $ printType b,
            "but it isn't"
          ]
      (MissingPermissionLabel a b origin name) ->
        hsep
          [ showOriginPrefix origin,
            "the permission label",
            dquotes $ printConstructor name,
            "was missing when I tried to match the permission type",
            dquotes $ printType a,
            "with the permission type",
            dquotes $ printType b
          ]
      (TypeArgumentCountMismatch term args) ->
        hsep
          [ showOriginPrefix $ Term.origin term,
            "I expected",
            pretty $ Term.quantifierCount term,
            "type arguments to",
            dquotes $ printTerm term,
            "but",
            pretty (length args),
            "were provided:",
            list $ map (dquotes . printType) args
          ]
      (CannotResolveName origin category name) ->
        hsep
          -- TODO: Suggest similar names in scope.
          [ showOriginPrefix origin,
            "I can't find the",
            case category of
              WordName -> "word"
              TypeName -> "type",
            "that the",
            case category of
              WordName -> "word"
              TypeName -> "type",
            "name",
            dquotes $ printGeneralName name,
            "refers to"
          ]
      (MultipleDefinitions origin name duplicates) ->
        vsep $
          hsep
            [ showOriginPrefix origin,
              "I found multiple definitions of",
              dquotes $ printQualified name,
              parens "did you mean to declare it as a trait?"
            ] :
          map
            ( \duplicateOrigin ->
                hsep
                  ["also defined at", printOrigin duplicateOrigin]
            )
            duplicates
      (WordRedefinition origin name originalOrigin) ->
        vsep
          [ hsep
              [ showOriginPrefix origin,
                "I can't redefine the word",
                dquotes $ printQualified name,
                "because it already exists",
                parens "did you mean to declare it as a trait?"
              ],
            hsep
              [ showOriginPrefix originalOrigin,
                "it was originally defined here"
              ]
          ]
      ( WordRedeclaration
          origin
          name
          signature
          originalOrigin
          mOriginalSignature
        ) ->
          vsep $
            hsep
              [ showOriginPrefix origin,
                "I can't redeclare the word",
                dquotes $ printQualified name,
                "with the signature",
                dquotes $ printSignature signature
              ] :
            hsep
              [ showOriginPrefix originalOrigin,
                "because it was declared or defined already"
              ] :
              [ hsep
                  [ "with the signature",
                    dquotes $ printSignature originalSignature
                  ]
                | Just originalSignature <- [mOriginalSignature]
              ]
      -- TODO: Report type kind.
      (TypeMismatch a b) ->
        vsep
          [ hsep
              [ showOriginPrefix $ Type.origin a,
                "I can't match the type",
                dquotes $ printType a
              ],
            hsep
              [ showOriginPrefix $ Type.origin b,
                "with the type",
                dquotes $ printType b
              ]
          ]
      (RedundantCase origin) ->
        hsep
          [ showOriginPrefix origin,
            "this case is redundant and will never match"
          ]
      (UseCommon origin instead) ->
        hsep
          [ showOriginPrefix origin,
            "I think you can use ",
            dquotes $ printQualified instead,
            " from the common library instead of what you have here"
          ]
      (Chain reports) -> vsep $ map kindMsg reports
      (OccursCheckFailure a b) ->
        vsep
          [ hsep
              [ showOriginPrefix $ Type.origin a,
                "the type",
                dquotes $ printType a
              ],
            hsep
              [ showOriginPrefix $ Type.origin b,
                "occurs in the type",
                dquotes $ printType b,
                parens "which often indicates an infinite type"
              ]
          ]
      (StackDepthMismatch origin) ->
        hsep
          [ showOriginPrefix origin,
            "you may have a stack depth mismatch"
          ]
      (InvalidOperatorMetadata origin name term) ->
        hsep
          [ showOriginPrefix origin,
            " invalid operator metadata for ",
            dquotes $ printQualified name,
            ":",
            printTerm term
          ]
      (ParseError origin unexpectedThing expectedThing) ->
        hsep $
          (showOriginPrefix origin :) $ intersperse "; " $ unexpectedThing ++ [expectedThing]
      (Context context message) ->
        vsep $
          map
            (\(origin, doc) -> hsep [showOriginPrefix origin, "while", doc])
            context
            ++ [human message]

showOriginPrefix :: Origin.Origin -> Doc a
showOriginPrefix origin = printOrigin origin <> colon

parseError :: Parsec.ParseError -> Report
parseError parsecError = Report Error (ParseError origin unexpected' expected')
  where
    origin :: Origin
    origin = Origin.pos $ Parsec.errorPos parsecError

    sysUnexpected, unexpected, expected :: [Parsec.Message]
    (sysUnexpected, unexpected, expected) =
      flip evalState (Parsec.errorMessages parsecError) $
        (,,)
          <$> state (span (Parsec.SysUnExpect "" ==))
            <*> state (span (Parsec.UnExpect "" ==))
            <*> state (span (Parsec.Expect "" ==))

    unexpected' :: [Doc ()]
    unexpected' = ((++) `on` unexpectedMessages) sysUnexpected unexpected

    expected' :: Doc ()
    expected' =
      hsep
        ( "expected" :
          punctuate
            comma
            ( map pretty $
                ordNub $
                  filter (not . null) $ -- TODO: Replace with "end of input"
                    map Parsec.messageString expected
            )
        )

unexpectedMessages :: [Parsec.Message] -> [Doc ()]
unexpectedMessages = map unexpectedMessage . ordNub

unexpectedMessage :: Parsec.Message -> Doc ()
unexpectedMessage message =
  let string = Parsec.messageString message
   in hsep
        [ "unexpected",
          if null string
            then "end of input"
            else pretty string
        ]