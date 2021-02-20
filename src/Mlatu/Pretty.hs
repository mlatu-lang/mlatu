-- |
-- Module      : Mlatu.Pretty
-- Description : Pretty-printing utilities
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Pretty
  ( angles,
    asDefinition,
    list,
    oxford,
    quote,
    vsep,
  )
where

import Relude
import Text.PrettyPrint
  ( Doc,
    char,
    hcat,
    hsep,
    nest,
    quotes,
    vcat,
  )
import Text.PrettyPrint.HughesPJClass (Pretty (..))

angles :: Doc -> Doc
angles doc = hcat [char '<', doc, char '>']

list :: [Doc] -> Doc
list = hcat . intersperse ", "

oxford :: Doc -> [Doc] -> Doc
oxford conjunction = go
  where
    go :: [Doc] -> Doc
    go [] = ""
    go [x] = x
    go [x, y] = hsep [x, conjunction, y]
    go [x, y, z] = hcat [x, ", ", y, ", ", conjunction, " ", z]
    go (x : xs) = hcat [x, ", ", go xs]

quote :: (Pretty a) => a -> Doc
quote = quotes . pPrint

vsep :: [Doc] -> Doc
vsep = vcat . intersperse ""

asDefinition :: Doc -> Doc -> Doc -> Doc -> Doc
asDefinition name signature body keyword =
  vcat
    [ hcat
        [hsep [keyword, name, signature], ":"],
      nest 2 body
    ]
