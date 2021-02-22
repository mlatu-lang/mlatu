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
import Mlatu.Signature (Constraint)
import Relude hiding (Constraint)
import Text.PrettyPrint (($$))
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.HughesPJClass (Pretty (..))

data TypeDefinition = TypeDefinition
  { constructors :: ![DataConstructor],
    name :: !Qualified,
    origin :: !Origin,
    parameters :: !([Parameter], [Constraint])
  }
  deriving (Eq, Ord, Show)

instance Pretty TypeDefinition where
  pPrint (TypeDefinition constructors name _ parameters) =
    ( "type " <> pPrint name
        <> ( if Pretty.isEmpty printedParameters
               then Pretty.empty
               else Pretty.braces printedParameters
           )
        <> ":"
    )
      $$ Pretty.nest
        2
        (Pretty.vcat $ map pPrint constructors)
    where
      printedParameters = Pretty.hsep (map pPrint (fst parameters))
