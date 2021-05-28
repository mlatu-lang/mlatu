{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Origin
-- Description : Source locations
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Origin
  ( Origin (..),
    begin,
    point,
    pos,
    range,
    name,
    beginLine,
    beginColumn,
    endLine,
    endColumn,
    between,
  )
where

import Optics
import Relude
import Text.Parsec.Pos
  ( Column,
    Line,
    SourceName,
    SourcePos,
    newPos,
    sourceColumn,
    sourceLine,
    sourceName,
  )

-- | A source location, in the form of an origin name (typically a file path)
-- and source span between two ('Line', 'Column') pairs.
data Origin = Origin
  { _name :: !Text,
    _beginLine :: !Line,
    _beginColumn :: !Column,
    _endLine :: !Line,
    _endColumn :: !Column
  }
  deriving (Ord, Eq, Show)

makeLenses ''Origin

-- | The starting 'SourcePos' of an 'Origin'.
begin :: Origin -> SourcePos
begin = newPos <$> toString . view name <*> view beginLine <*> view beginColumn

-- | A zero-width 'Origin' at the given 'Line' and 'Column'.
point :: SourceName -> Line -> Column -> Origin
point path line column =
  Origin
    { _name = toText path,
      _beginLine = line,
      _beginColumn = column,
      _endLine = line,
      _endColumn = column
    }

-- | Makes a zero-width 'Origin' from a 'SourcePos'.
pos :: SourcePos -> Origin
pos = point <$> sourceName <*> sourceLine <*> sourceColumn

-- | Makes a range between two 'SourcePos' points.
range :: SourcePos -> SourcePos -> Origin
range a b =
  Origin
    { _name = toText $ sourceName a,
      _beginLine = sourceLine a,
      _beginColumn = sourceColumn a,
      _endLine = sourceLine b,
      _endColumn = sourceColumn b
    }

between :: Origin -> Origin -> Origin
between a b =
  Origin
    { _name = view name a,
      _beginLine = view beginLine a,
      _beginColumn = view beginColumn a,
      _endLine = view endLine b,
      _endColumn = view endColumn b
    }
