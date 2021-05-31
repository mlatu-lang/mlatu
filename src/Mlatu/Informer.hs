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
    ice,
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

errorCheckpoint :: (Applicative m) => MT m ()
errorCheckpoint = checkpoint Error

warnCheckpoint :: (Applicative m) => MT m ()
warnCheckpoint = checkpoint Warn

-- | A Mlatu action atop a 'Monad' 'm', pureing a result of type 'a', which
-- maintains a '[(Origin, Doc ())]' stack and can fail with a list of '[Report]'.
newtype MT m a = MT (ReaderT [(Origin, Doc ())] (MaybeT (StateT [Report] m)) a)
  deriving (Functor, Applicative, Monad, MonadReader [(Origin, Doc ())], MonadState [Report], MonadIO, MonadFix, MonadFail)

type M = MT IO

mt :: ([(Origin, Doc ())] -> [Report] -> m (Maybe a, [Report])) -> MT m a
mt f = MT (ReaderT (MaybeT . StateT . f))

unMT :: MT m a -> [(Origin, Doc ())] -> [Report] -> m (Maybe a, [Report])
unMT (MT action) context = runStateT (runMaybeT (runReaderT action context))

-- | Runs a nested action, pureing whether it completed successfully, that is,
-- without generating any reports.
attempt :: (Monad m) => MT m a -> MT m Bool
attempt action =
  mt
    ( \context reports ->
        ( \case
            (Just x, rs) -> (Just True, rs)
            (Nothing, rs) -> (Just False, rs)
        )
          <$> unMT action context reports
    )

runMlatu :: (Monad m) => MT m a -> m (Maybe a, [Report])
runMlatu action = unMT action [] []

checkpoint :: (Applicative m) => Level -> MT m ()
checkpoint minLvl =
  mt $ \_ reports ->
    pure $
      if all (\(Report lvl _ _) -> lvl < minLvl) reports
        then (Just (), reports)
        else (Nothing, reports)

halt :: (Applicative m) => MT m a
halt = mt $ const (\reports -> pure (Nothing, reports))

showOriginPrefix :: Origin.Origin -> Doc a
showOriginPrefix origin = printOrigin origin <> colon

report :: (Monad m) => Report -> MT m ()
report r@(Report l o kind) =
  mt $ \context reports ->
    pure . (Just (),) $
      ( if null context
          then r : reports
          else
            Report
              l
              o
              ( vsep
                  ( ((\(origin, doc) -> hsep [showOriginPrefix origin, "while", doc]) <$> context)
                      ++ [kind]
                  )
              ) :
            reports
      )

while :: Origin -> Doc () -> MT m a -> MT m a
while origin message action = mt $ unMT action . ((origin, message) :)

data NameCategory = WordName | TypeName
  deriving (Eq, Show)

data Level
  = Warn
  | Error
  deriving (Ord, Eq, Show)

data Report = Report Level Origin (Doc ())
  deriving (Eq, Show)

parseError :: Parsec.ParseError -> Doc ()
parseError parsecError =
  hsep (("I am not wanting to see" : intersperse ", " unexpected') ++ ("here; instead" : expected'))
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

    expected' :: [Doc ()]
    expected' =
      "I want to see " :
      punctuate
        comma
        ( pretty
            <$> ordNub
              ( filter (not . null) $ -- TODO: Replace with "end of input"
                  Parsec.messageString <$> expected
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

reportParseError :: (Monad m) => Parsec.ParseError -> MT m ()
reportParseError x = report $ Report Error (Origin.pos $ Parsec.errorPos x) (parseError x)

reportTypeMismatch :: (Monad m) => Type -> Type -> Origin -> MT m ()
reportTypeMismatch a b origin =
  report $
    Report
      Error
      origin
      ( hsep
          [ "I want to match the type",
            dquotes $ printType a,
            "with the type",
            dquotes $ printType b,
            "but I am not able to."
          ]
      )

reportCannotResolveName :: (Monad m) => Origin -> NameCategory -> GeneralName -> MT m ()
reportCannotResolveName origin category name =
  report $
    Report
      Error
      origin
      ( hsep
          [ "I want to see the ",
            case category of
              WordName -> "word"
              TypeName -> "type",
            "that the name",
            dquotes $ printGeneralName name,
            "refers to here, but I am not seeing it."
          ]
      )

reportOccursTypeMismatch :: (Monad m) => Type -> Type -> Origin -> MT m ()
reportOccursTypeMismatch a b origin =
  report $
    Report
      Error
      origin
      ( vsep
          [ hsep
              [ "I want to match the type",
                dquotes $ printType a,
                "with the type",
                dquotes $ printType b,
                "but I am not able to."
              ],
            hsep
              [ "The type",
                dquotes $ printType a,
                "occurs in the type",
                dquotes $ printType b,
                "which could indicate an infinite type."
              ]
          ]
      )

reportStackDepthMismatch :: (Monad m) => Type -> Type -> Origin -> MT m ()
reportStackDepthMismatch a b origin =
  report $
    Report
      Error
      origin
      ( vsep
          [ hsep
              [ "I want to match the type",
                dquotes $ printType a,
                "with the type",
                dquotes $ printType b,
                "but I am not able to."
              ],
            hsep
              [ "The type",
                dquotes $ printType a,
                "occurs in the type",
                dquotes $ printType b,
                "which could indicate an infinite type."
              ],
            "You may have a stack depth mismatch here."
          ]
      )

reportTypeArgumentCountMismatch :: (Monad m) => Term a -> [Type] -> Origin -> MT m ()
reportTypeArgumentCountMismatch term args origin =
  report $
    Report
      Error
      origin
      ( hsep
          [ "I want to find",
            pretty $ Term.quantifierCount term,
            "type arguments to",
            dquotes $ printTerm term,
            "but I only found",
            pretty (length args),
            "instead:",
            list $ dquotes . printType <$> args
          ]
      )

reportWordRedefinition :: (Monad m) => Origin -> Qualified -> Origin -> MT m ()
reportWordRedefinition origin name originalOrigin = do
  report $
    Report
      Error
      origin
      ( hsep
          [ "I want for there to be just one definition for",
            dquotes $ printQualified name,
            "but instead it was already defined",
            parens "did you mean to declare it as a trait?"
          ]
      )
  report $ Report Error originalOrigin "It was originally defined here."

reportMissingPermissionLabel :: (Monad m) => Type -> Type -> Origin -> Constructor -> MT m ()
reportMissingPermissionLabel a b origin name =
  report $
    Report
      Error
      origin
      ( hsep
          [ "I want to match the permission type",
            dquotes $ printType a,
            "with the permission type",
            dquotes $ printType b,
            ", but the permission label",
            dquotes $ printConstructor name,
            "is missing."
          ]
      )

reportFailedInstanceCheck :: (Monad m) => Type -> Type -> Origin -> MT m ()
reportFailedInstanceCheck a b origin = do
  report $
    Report
      Error
      origin
      ( hsep
          -- TODO: Show type kind.
          [ "I want",
            dquotes $ printType a,
            "to be as general or more general then",
            dquotes $ printType b,
            "but it is less general instead."
          ]
      )

reportWordRedeclaration :: (Monad m) => Origin -> Qualified -> Signature -> Origin -> Maybe Signature -> MT m ()
reportWordRedeclaration origin name signature originalOrigin mOriginalSignature = do
  report $
    Report
      Error
      origin
      ( hsep
          [ "I can't redeclare the word",
            dquotes $ printQualified name,
            "with the signature",
            dquotes $ printSignature signature,
            "."
          ]
      )
  report $
    Report
      Error
      originalOrigin
      ( hsep
          ( "because it was declared or defined already here" :
            ( case mOriginalSignature of
                Just originalSignature ->
                  [ "with the signature",
                    dquotes $ printSignature originalSignature,
                    "."
                  ]
                Nothing -> ["."]
            )
          )
      )

reportMissingTypeSignature :: (Monad m) => Origin -> Qualified -> MT m ()
reportMissingTypeSignature origin name =
  report $
    Report
      Error
      origin
      ( hsep
          [ "I want every word to have a type signature,",
            "but I'm not seeing a type signature for the word",
            dquotes (printQualified name),
            "."
          ]
      )

reportMultiplePermissionVariables :: (Monad m) => Origin -> Type -> Type -> MT m ()
reportMultiplePermissionVariables origin a b =
  report $
    Report
      Error
      origin
      ( hsep
          [ "I want every function to have just one permission variable,",
            "but I'm seeing more than one permission variable in this function instead:",
            dquotes $ printType a,
            "and",
            dquotes $ printType b,
            "."
          ]
      )

reportCannotResolveType :: (Monad m) => Origin -> GeneralName -> MT m ()
reportCannotResolveType origin name =
  report $
    Report
      Error
      origin
      ( hsep
          [ "I want to see the type",
            dquotes $ printGeneralName name,
            "is supposed to refer to, but I am not seeing it",
            parens "did you mean to add it as a type parameter?"
          ]
      )

reportRedundantCase :: (Monad m) => Origin -> MT m ()
reportRedundantCase origin =
  report $
    Report
      Warn
      origin
      "I'm not seeing a value this case matches - maybe its redundant and should be removed?"
