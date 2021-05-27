{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Informer
-- Description : Error-reporting monad
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Informer
  ( M,
    MT,
    NameCategory (..),
    Level (..),
    Report (..),
    halt,
    while,
    attempt,
    runMlatu,
    errorCheckpoint,
    warnCheckpoint,
    infoCheckpoint,
    ice,
    human,
    reportParseError,
    reportTypeMismatch,
    reportCannotResolveName,
    reportTypeArgumentCountMismatch,
    reportMissingPermissionLabel,
    reportFailedInstanceCheck,
    reportWordRedeclaration,
    reportWordRedefinition,
    reportMissingTypeSignature,
    reportMultiplePermissionVariables,
    reportCannotResolveType,
    reportStackDepthMismatch,
    reportOccursTypeMismatch,
    reportRedundantCase,
  )
where

import Control.Monad (ap)
import Control.Monad.Fix (MonadFix (..))
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
import Optics.TH (makePrisms)
import Prettyprinter (Doc, Pretty (pretty), colon, comma, dquotes, hsep, list, parens, punctuate, vsep, (<+>))
import Relude hiding (Type)
import System.IO.Unsafe (unsafeInterleaveIO)
import Text.Parsec qualified as Parsec
import Text.Parsec.Error qualified as Parsec
import Prelude (errorWithoutStackTrace)

ice :: String -> a
ice msg =
  errorWithoutStackTrace $
    "Internal Compiler Error: Mlatu has failed internally.\n\
    \Please submit a bug report at https://github.com/brightly-salty/mlatu/issues/new\n\
    \Error: "
      <> msg

reportError :: (Monad m) => ReportKind -> MT m ()
reportError = report . Report Error

reportWarning :: (Monad m) => ReportKind -> MT m ()
reportWarning = report . Report Warn

errorCheckpoint :: (Applicative m) => MT m ()
errorCheckpoint = checkpoint Error

warnCheckpoint :: (Applicative m) => MT m ()
warnCheckpoint = checkpoint Warn

infoCheckpoint :: (Applicative m) => MT m ()
infoCheckpoint = checkpoint Info

-- | A Mlatu action atop a 'Monad' 'm', pureing a result of type 'a', which
-- maintains a '[(Origin, Doc ())]' stack and can fail with a list of '[Report]'.
newtype MT m a = MT
  { unMT :: [(Origin, Doc ())] -> [Report] -> ExceptT [Report] m (a, [Report])
  }

type M = MT IO

-- | Runs a nested action, pureing whether it completed successfully, that is,
-- without generating any reports.
attempt :: (Monad m) => MT m a -> MT m Bool
attempt action = MT $ \context reports ->
  ExceptT $
    runExceptT (unMT action context reports)
      <&> Right . either (False,) ((True,) . snd)

runMlatu :: (Monad m) => MT m a -> m (Either [Report] a)
runMlatu (MT m) = runExceptT $ m [] [] <&> fst

instance (Monad m) => Functor (MT m) where
  fmap f (MT ax) = MT $ flip flip (first f) . ((<&>) .) . ax
  {-# INLINEABLE fmap #-}

instance (Monad m) => Applicative (MT m) where
  pure x = MT $ const (pure . (x,))
  MT af <*> MT ax = MT $ ap (flip . ((>>=) .) . af) (uncurry . (. first) . flip . ((<&>) .) . ax)
  {-# INLINEABLE (<*>) #-}

instance (Monad m) => Monad (MT m) where
  return = pure
  MT ax >>= f = MT $ ap (flip . ((>>=) .) . ax) (uncurry . (. f) . flip unMT)
  {-# INLINEABLE (>>=) #-}

instance (MonadIO m) => MonadFix (MT m) where
  mfix k = MT $ \context reports ->
    newEmptyMVar
      >>= ap
        ((>>=) . liftIO . unsafeInterleaveIO . takeMVar)
        ((<=< flip (flip unMT context . k) reports) . (liftIO .) . (`ap` pure) . ((>>) .) . (. fst) . putMVar)

instance (MonadIO m) => MonadIO (MT m) where
  liftIO m = MT $ const ((liftIO m <&>) . flip (,))

instance (Monad m) => MonadFail (MT m) where
  fail = ice "Mlatu.monadFail -- fail was called somewhere, somehow"

checkpoint :: (Applicative m) => Level -> MT m ()
checkpoint minLvl = MT $ \_ reports ->
  hoistEither $
    if all (\(Report lvl _) -> lvl < minLvl) reports
      then Right ((), reports)
      else Left reports

halt :: (Applicative m) => MT m a
halt = MT $ const (hoistEither . Left)

report :: (Monad m) => Report -> MT m ()
report r = MT $ \context reports ->
  pure . (,) () $
    (\ctxt -> (if null ctxt then r else Report Info (Context ctxt r)) : reports)
      context

while :: Origin -> Doc () -> MT m a -> MT m a
while origin message action = MT $ unMT action . ((origin, message) :)

data NameCategory = WordName | TypeName
  deriving (Eq, Show)

data Level
  = Info
  | Warn
  | Error
  deriving (Ord, Eq, Show)

data Report = Report Level ReportKind
  deriving (Eq, Show)

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
            list $ dquotes . printType <$> args
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
          ( ( \duplicateOrigin ->
                hsep
                  ["also defined at", printOrigin duplicateOrigin]
            )
              <$> duplicates
          )
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
      (Chain reports) -> vsep $ kindMsg <$> reports
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
          ( (\(origin, doc) -> hsep [showOriginPrefix origin, "while", doc])
              <$> context
          )
            ++ [human message]

showOriginPrefix :: Origin.Origin -> Doc a
showOriginPrefix origin = printOrigin origin <> colon

parseError :: Parsec.ParseError -> ReportKind
parseError parsecError = ParseError origin unexpected' expected'
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
            ( pretty
                <$> ordNub
                  ( filter (not . null) $ -- TODO: Replace with "end of input"
                      Parsec.messageString <$> expected
                  )
            )
        )

unexpectedMessages :: [Parsec.Message] -> [Doc ()]
unexpectedMessages = fmap unexpectedMessage . ordNub

unexpectedMessage :: Parsec.Message -> Doc ()
unexpectedMessage message =
  let string = Parsec.messageString message
   in hsep
        [ "unexpected",
          if null string
            then "end of input"
            else pretty string
        ]

reportParseError x = reportError (parseError x)

reportTypeMismatch x y = reportError (TypeMismatch x y)

reportCannotResolveName x y z = reportError (CannotResolveName x y z)

reportOccursTypeMismatch a b c d = reportError (Chain [TypeMismatch a b, OccursCheckFailure c d])

reportStackDepthMismatch a b c d e = reportError (Chain [TypeMismatch a b, OccursCheckFailure c d, StackDepthMismatch e])

reportTypeArgumentCountMismatch x y = reportError (TypeArgumentCountMismatch x y)

reportWordRedefinition x y z = reportError (WordRedefinition x y z)

reportMissingPermissionLabel a b c d = reportError (MissingPermissionLabel a b c d)

reportFailedInstanceCheck x y = reportError (FailedInstanceCheck x y)

reportWordRedeclaration a b c d e = reportError (WordRedeclaration a b c d e)

reportMissingTypeSignature x y = reportError (MissingTypeSignature x y)

reportMultiplePermissionVariables x y z = reportError (MultiplePermissionVariables x y z)

reportCannotResolveType x y = reportError (CannotResolveType x y)

reportRedundantCase x = reportWarning (RedundantCase x)
