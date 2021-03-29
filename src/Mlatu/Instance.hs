{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Instance
-- Description : Declarations of instances
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Instance
  ( Instance (..),
    name,
    origin,
    methods,
    target,
  )
where

import Mlatu.Definition (WordDefinition)
import Mlatu.Entry.Parameter (Parameter)
import Mlatu.Name (Qualified (..))
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Optics.TH (makeLenses)
import Relude

data Instance a = Instance
  { _name :: !Qualified,
    _origin :: !Origin,
    _methods :: ![WordDefinition a],
    _target :: !Signature
  }
  deriving (Eq, Ord, Show)

makeLenses ''Instance
