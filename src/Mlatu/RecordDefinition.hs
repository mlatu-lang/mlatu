{-# LANGUAGE TemplateHaskell #-}

module Mlatu.RecordDefinition (RecordDefinition (..), fields, name, origin, parameters) where

import Mlatu.Entry.Parameter (Parameter)
import Mlatu.Name (Qualified)
import Mlatu.Origin (Origin)
import Mlatu.RecordField (RecordField (..))
import Optics.TH (makeLenses)
import Relude

data RecordDefinition = RecordDefinition
  { _fields :: ![RecordField],
    _name :: !Qualified,
    _origin :: !Origin,
    _parameters :: ![Parameter]
  }
  deriving (Eq, Ord, Show)

makeLenses ''RecordDefinition
