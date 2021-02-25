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
  )
where

import GHC.List (span)
import Mlatu.Name (GeneralName, Qualified)
import Mlatu.Origin (Origin)
import Mlatu.Origin qualified as Origin
import Mlatu.Signature (Signature)
import Mlatu.Term (Term)
import Mlatu.Type (Constructor, Type)
import Prettyprinter (Doc, Pretty (pretty), hsep, list)
import Relude hiding (Type)
import Text.Parsec qualified as Parsec
import Text.Parsec.Error qualified as Parsec

data NameCategory = WordName | TypeName
  deriving (Eq, Show)

data Level
  = Info
  | Warn
  | Error
  deriving (Ord, Eq, Show)

data Report = Report Level ReportKind
  deriving (Show)

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
  | ParseError !Origin ![Doc ()] !(Doc ())
  | UseCommon !Origin !Qualified
  | Context ![(Origin, Doc ())] !Report
  deriving (Show)

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
        [ "expected",
          list $
            map pretty $
              ordNub $
                filter (not . null) $ -- TODO: Replace with "end of input"
                  map Parsec.messageString expected
        ]

unexpectedMessages :: [Parsec.Message] -> [Doc ()]
unexpectedMessages = map unexpectedMessage

unexpectedMessage :: Parsec.Message -> Doc ()
unexpectedMessage message =
  let string = Parsec.messageString message
   in hsep
        [ "unexpected",
          if null string
            then "end of input"
            else pretty string
        ]