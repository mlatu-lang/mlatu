{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Mlatu.Parser
-- Description : Parsing utilities
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Front.Parser
  ( Parser,
    getTokenOrigin,
    parserMatch,
    parserMatch_,
    tokenSatisfy,
  )
where

import Mlatu.Base.Located (Located)
import Mlatu.Base.Located qualified as Located
import Mlatu.Base.Name (Qualifier)
import Mlatu.Base.Origin (Origin)
import Mlatu.Base.Origin qualified as Origin
import Mlatu.Front.Token (Token)
import Mlatu.Pretty ()
import Text.Parsec (ParsecT, (<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos (SourcePos)

type Parser a = GeneralParser a

type GeneralParser a = ParsecT [Located Token] Qualifier Identity a

getTokenOrigin :: GeneralParser Origin
getTokenOrigin =
  Located.origin
    <$> Parsec.lookAhead (tokenSatisfy (const True))

tokenSatisfy ::
  (Located Token -> Bool) ->
  GeneralParser (Located Token)
tokenSatisfy predicate =
  Parsec.tokenPrim
    show
    advance
    (\token -> if predicate token then Just token else Nothing)
  where
    advance ::
      SourcePos ->
      Located Token ->
      [Located Token] ->
      SourcePos
    advance _ _ (token : _) = Origin.begin (Located.origin token)
    advance sourcePos _ _ = sourcePos

parserMatch :: Token -> GeneralParser (Located Token)
parserMatch token = tokenSatisfy ((== token) . Located.item) <?> show token

parserMatch_ :: Token -> GeneralParser ()
parserMatch_ = void . parserMatch
