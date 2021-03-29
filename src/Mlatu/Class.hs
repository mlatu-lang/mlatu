{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.Class
-- Description : Type classes
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Class
  ( Class (..),
    Method (..),
    className,
    classOrigin,
    methods,
    name,
    origin,
    signature,
    parameters,
  )
where

import Mlatu.Entry.Parameter (Parameter)
import Mlatu.Name (Qualified)
import Mlatu.Origin (Origin)
import Mlatu.Signature (Signature)
import Optics.TH (makeLenses)
import Relude

data Method = Method
  { _name :: !Qualified,
    _origin :: !Origin,
    _signature :: !Signature
  }
  deriving (Eq, Ord, Show)

makeLenses ''Method

data Class = Class
  { _className :: !Qualified,
    _classOrigin :: !Origin,
    _methods :: ![Method],
    _parameters :: ![Parameter]
  }
  deriving (Eq, Ord, Show)

makeLenses ''Class
