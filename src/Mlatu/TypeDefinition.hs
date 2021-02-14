{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Mlatu.TypeDefinition
-- Description : Definitions of types
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.TypeDefinition
  ( TypeDefinition (..),
    constructors,
    name,
    origin,
    parameters,
  )
where

import Mlatu.DataConstructor (DataConstructor)
import Mlatu.Entry.Parameter (Parameter)
import Mlatu.Name (Qualified)
import Mlatu.Origin (Origin)
import Optics.TH (makeLenses)
import Optics (view)
import Relude
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

data TypeDefinition = TypeDefinition
  { _constructors :: ![DataConstructor],
    _name :: !Qualified,
    _origin :: !Origin,
    _parameters :: ![Parameter]
  }
  deriving (Show)

makeLenses ''TypeDefinition

instance Pretty TypeDefinition where
  pPrint typedef =
    Pretty.vcat
      [ "type"
          Pretty.<+> pPrint (view name typedef),
        Pretty.colon,
        Pretty.braces $
          Pretty.hsep $
            map pPrint $ view parameters typedef,
        Pretty.nest
          4
          $ Pretty.vcat $ map pPrint $ view constructors typedef
      ]
