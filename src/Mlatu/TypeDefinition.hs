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

import Control.Lens (makeLenses, (^.))
import Mlatu.DataConstructor (DataConstructor)
import Mlatu.Entry.Parameter (Parameter)
import Mlatu.Name (Qualified)
import Mlatu.Origin (Origin)
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
          Pretty.<+> pPrint (typedef ^. name),
        Pretty.colon,
        Pretty.braces $
          Pretty.hsep $
            map pPrint $ typedef ^. parameters,
        Pretty.nest
          4
          $ Pretty.vcat $ map pPrint $ typedef ^. constructors
      ]
