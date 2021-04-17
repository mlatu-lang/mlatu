module Mlatu.Uses where

import Relude

data Uses = None | Affi | Once | Many deriving (Show, Eq)

instance Ord Uses where
  Many <= x = x == Many
  Affi <= x = x == Affi || x == Many
  x <= Once = x == Once || x == None
  x <= None = x == None
  _ <= _ = True

mul :: Uses -> Uses -> Uses
mul None _ = None
mul _ None = None
mul Many _ = Many
mul _ Many = Many
mul Affi _ = Affi
mul Once x = x

add :: Uses -> Uses -> Uses
add None x = x
add x None = x
add _ _ = Many

instance Hashable Uses where
  hashWithSalt s u =
    hashWithSalt
      s
      ( ( case u of
            None -> 0
            Affi -> 1
            Once -> 2
            Many -> 3
        ) ::
          Int
      )
