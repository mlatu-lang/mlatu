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
  )
where

import Mlatu.DataConstructor (DataConstructor)
import Mlatu.Entry.Parameter (Parameter)
import Mlatu.Name (Qualified)
import Mlatu.Origin (Origin)
import Relude
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

data TypeDefinition = TypeDefinition
  { constructors :: ![DataConstructor],
    name :: !Qualified,
    origin :: !Origin,
    parameters :: ![Parameter]
  }
  deriving (Show)

instance Pretty TypeDefinition where
  pPrint (TypeDefinition constructors name _ parameters) =
    Pretty.vcat
      [ "type"
          Pretty.<+> pPrint name,
        Pretty.colon,
        Pretty.braces $
          Pretty.hsep $
            map pPrint parameters,
        Pretty.nest
          4
          $ Pretty.vcat $ map pPrint constructors
      ]
