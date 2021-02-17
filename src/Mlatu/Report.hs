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
    human,
    parseError,
    makeError,
    makeWarning,
  )
where

import GHC.List (span)
import Mlatu.Name (GeneralName, Qualified)
import Mlatu.Origin (Origin)
import Mlatu.Origin qualified as Origin
import Mlatu.Pretty qualified as Pretty
import Mlatu.Signature (Signature)
import Mlatu.Term (Term)
import Mlatu.Term qualified as Term
import Mlatu.Type (Constructor, Type)
import Mlatu.Type qualified as Type
import Relude hiding (Type)
import Text.Parsec qualified as Parsec
import Text.Parsec.Error qualified as Parsec
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

data NameCategory = WordName | TypeName
  deriving (Eq, Show)

instance Pretty NameCategory where
  pPrint = \case
    WordName -> "word"
    TypeName -> "type"

data Level
  = Info
  | Warn
  | Error
  deriving (Eq, Show)

data Report = Report Level ReportKind
  deriving (Eq, Show)

makeError :: ReportKind -> Report
makeError = Report Error

makeWarning :: ReportKind -> Report
makeWarning = Report Warn

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
  | ParseError !Origin ![Pretty.Doc] !Pretty.Doc !(Maybe Pretty.Doc)
  | UseCommon !Origin !Qualified
  | Context ![(Origin, Pretty.Doc)] !Report
  deriving (Eq, Show)

human :: Report -> Pretty.Doc
human (Report lvl kind) =
  Pretty.hsep
    [ case lvl of
        Info -> "Info: "
        Warn -> "Warn: "
        Error -> "Error: ",
      kindMsg kind
    ]
  where
    kindMsg = \case
      (MissingTypeSignature origin name) ->
        Pretty.hsep
          [ showOriginPrefix origin,
            "I can't find a type signature for the word",
            Pretty.quote name
          ]
      (MultiplePermissionVariables origin a b) ->
        Pretty.hsep
          [ showOriginPrefix origin,
            "I found multiple permission variables",
            Pretty.colon,
            Pretty.quote a,
            "and",
            Pretty.quote b,
            "but only one is allowed per function"
          ]
      (CannotResolveType origin name) ->
        Pretty.hsep
          [ showOriginPrefix origin,
            "I can't tell which type",
            Pretty.quote name,
            "refers to",
            Pretty.parens "did you mean to add it as a type parameter?"
          ]
      (FailedInstanceCheck a b) ->
        Pretty.hsep
          -- TODO: Show type kind.
          [ "I expected",
            Pretty.quote a,
            "to be at least as polymorphic as",
            Pretty.quote b,
            "but it isn't"
          ]
      (MissingPermissionLabel a b origin name) ->
        Pretty.hsep
          [ showOriginPrefix origin,
            "the permission label",
            Pretty.quote name,
            "was missing when I tried to match the permission type",
            Pretty.quote a,
            "with the permission type",
            Pretty.quote b
          ]
      (TypeArgumentCountMismatch term args) ->
        Pretty.hsep
          [ showOriginPrefix $ Term.origin term,
            "I expected",
            Pretty.int $ Term.quantifierCount term,
            "type arguments to",
            Pretty.quote term,
            "but",
            Pretty.int (length args),
            "were provided",
            Pretty.colon,
            Pretty.oxford "and" $ map Pretty.quote args
          ]
      (CannotResolveName origin category name) ->
        Pretty.hsep
          -- TODO: Suggest similar names in scope.
          [ showOriginPrefix origin,
            "I can't find the",
            case category of
              WordName -> "word"
              TypeName -> "type",
            "that the",
            pPrint category,
            "name",
            Pretty.quote name,
            "refers to"
          ]
      (MultipleDefinitions origin name duplicates) ->
        Pretty.vcat $
          Pretty.hsep
            [ showOriginPrefix origin,
              "I found multiple definitions of",
              Pretty.quote name,
              Pretty.parens "did you mean to declare it as a trait?"
            ] :
          map
            ( \duplicateOrigin ->
                Pretty.hsep
                  ["also defined at", pPrint duplicateOrigin]
            )
            duplicates
      (WordRedefinition origin name originalOrigin) ->
        Pretty.vcat
          [ Pretty.hsep
              [ showOriginPrefix origin,
                "I can't redefine the word",
                Pretty.quote name,
                "because it already exists",
                Pretty.parens "did you mean to declare it as a trait?"
              ],
            Pretty.hsep
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
          Pretty.vcat $
            Pretty.hsep
              [ showOriginPrefix origin,
                "I can't redeclare the word",
                Pretty.quote name,
                "with the signature",
                Pretty.quote signature
              ] :
            Pretty.hsep
              [ showOriginPrefix originalOrigin,
                "because it was declared or defined already"
              ] :
              [ Pretty.hsep
                  [ "with the signature",
                    Pretty.quote originalSignature
                  ]
                | Just originalSignature <- [mOriginalSignature]
              ]
      -- TODO: Report type kind.
      (TypeMismatch a b) ->
        Pretty.vcat
          [ Pretty.hsep
              [ showOriginPrefix $ Type.origin a,
                "I can't match the type",
                Pretty.quote a
              ],
            Pretty.hsep
              [ showOriginPrefix $ Type.origin b,
                "with the type",
                Pretty.quote b
              ]
          ]
      (RedundantCase origin) ->
        Pretty.hcat
          [ showOriginPrefix origin,
            "this case is redundant and will never match"
          ]
      (UseCommon origin instead) ->
        Pretty.hcat
          [ showOriginPrefix origin,
            "I think you can use ",
            Pretty.quote instead,
            " from the common library instead of what you have here"
          ]
      (Chain reports) -> Pretty.vsep $ map kindMsg reports
      (OccursCheckFailure a b) ->
        Pretty.vcat
          [ Pretty.hsep
              [ showOriginPrefix $ Type.origin a,
                "the type",
                Pretty.quote a
              ],
            Pretty.hsep
              [ showOriginPrefix $ Type.origin b,
                "occurs in the type",
                Pretty.quote b,
                Pretty.parens "which often indicates an infinite type"
              ]
          ]
      (StackDepthMismatch origin) ->
        Pretty.hsep
          [ showOriginPrefix origin,
            "you may have a stack depth mismatch"
          ]
      (InvalidOperatorMetadata origin name term) ->
        Pretty.hcat
          [ showOriginPrefix origin,
            " invalid operator metadata for ",
            Pretty.quote name,
            Pretty.colon,
            pPrint term
          ]
      (ParseError origin unexpectedThing expectedThing message) ->
        Pretty.hcat $
          showOriginPrefix origin : unexpectedThing ++ [Pretty.text "; "] ++ expectedThing : maybe [] (\x -> [Pretty.parens x]) message
      (Context context message) ->
        Pretty.vsep $
          map
            (\(origin, doc) -> Pretty.hsep [showOriginPrefix origin, "while", doc])
            context
            ++ [human message]

showOriginPrefix :: Origin -> Pretty.Doc
showOriginPrefix origin = Pretty.hcat [pPrint origin, ":"]

parseError :: Parsec.ParseError -> Report
parseError parsecError = Report Error (ParseError origin unexpected' expected' messages')
  where
    origin :: Origin
    origin = Origin.pos $ Parsec.errorPos parsecError

    sysUnexpected, unexpected, expected, messages :: [Parsec.Message]
    (sysUnexpected, unexpected, expected, messages) =
      flip evalState (Parsec.errorMessages parsecError) $
        (,,,)
          <$> state (span (Parsec.SysUnExpect "" ==))
            <*> state (span (Parsec.UnExpect "" ==))
            <*> state (span (Parsec.Expect "" ==))
            <*> state (span (Parsec.Message "" ==))

    unexpected' :: [Pretty.Doc]
    unexpected' = Pretty.text " unexpected " : map parsecMessage (ordNub (sysUnexpected ++ unexpected))

    expected' :: Pretty.Doc
    expected' = if not $ null xs then Pretty.hsep [Pretty.text "expected", Pretty.oxford (Pretty.text "or") xs] else Pretty.text ""
      where
        xs = map parsecMessage (ordNub expected)

    messages' :: Maybe Pretty.Doc
    messages' = if not $ null xs then Just $ Pretty.hsep xs else Nothing
      where
        xs = map parsecMessage $ ordNub messages

parsecMessage :: Parsec.Message -> Pretty.Doc
parsecMessage message =
  let string = Parsec.messageString message
   in case length string of
        0 -> "end of input"
        1 -> Pretty.quotes $ Pretty.text string
        _ -> Pretty.text string