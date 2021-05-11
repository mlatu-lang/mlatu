{-# LANGUAGE TemplateHaskell #-}

module Mlatu.TypeAlias (TypeAlias (..), name, alias, origin) where

import Mlatu.Name (Qualified (..), Unqualified (..))
import Mlatu.Origin (Origin (..))
import Optics (makeLenses)
import Relude

data TypeAlias = TypeAlias
  { _name :: !Unqualified,
    _alias :: !Qualified,
    _origin :: !Origin
  }
  deriving (Show, Eq, Ord)

makeLenses ''TypeAlias
