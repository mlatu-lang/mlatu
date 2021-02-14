-- |
-- Module      : Mlatu.Stack
-- Description : Strict stack
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Stack
  ( Stack (..),
    fromList,
    popNote,
    pops,
    pushes,
  )
where

import Relude hiding (fromList)

-- | A stack with strictly evaluated elements and spine.
data Stack a = Bottom | !a ::: !(Stack a)
  deriving (Functor, Foldable)

infixr 5 :::

fromList :: [a] -> Stack a
fromList = foldr (:::) Bottom

popNote :: Stack a -> Stack a
popNote Bottom = error "Mlatu.Stack.drop: empty stack"
popNote (_ ::: s) = s

pushes :: [a] -> Stack a -> Stack a
pushes xs s = foldr (:::) s xs

pops :: Int -> Stack a -> ([a], Stack a)
pops n s
  | n <= 0 = ([], s)
  | otherwise = case s of
    Bottom -> ([], s)
    a ::: s' ->
      let (as, s'') = pops (n - 1) s'
       in (a : as, s'')
