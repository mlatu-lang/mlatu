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
  )
where

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
  { name :: !Text,
    beginLine :: !Line,
    beginColumn :: !Column,
    endLine :: !Line,
    endColumn :: !Column
  }
  deriving (Ord, Eq, Show)

-- | The starting 'SourcePos' of an 'Origin'.
begin :: Origin -> SourcePos
begin = newPos <$> toString . name <*> beginLine <*> beginColumn

-- | A zero-width 'Origin' at the given 'Line' and 'Column'.
point :: SourceName -> Line -> Column -> Origin
point path line column =
  Origin
    { name = toText path,
      beginLine = line,
      beginColumn = column,
      endLine = line,
      endColumn = column
    }

-- | Makes a zero-width 'Origin' from a 'SourcePos'.
pos :: SourcePos -> Origin
pos = point <$> sourceName <*> sourceLine <*> sourceColumn

-- | Makes a range between two 'SourcePos' points.
range :: SourcePos -> SourcePos -> Origin
range a b =
  Origin
    { name = toText $ sourceName a,
      beginLine = sourceLine a,
      beginColumn = sourceColumn a,
      endLine = sourceLine b,
      endColumn = sourceColumn b
    }

