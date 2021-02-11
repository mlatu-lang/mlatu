{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Mlatu.Bracket
-- Description : Whitespace-sensitive syntax desugaring
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Bracket
  ( bracket,
  )
where

import Mlatu.Indent (Indent (..))
import Mlatu.Informer (Informer (..))
import Mlatu.Layoutness (Layoutness (..))
import Mlatu.Located (Located (..))
import Mlatu.Located qualified as Located
import Mlatu.Origin qualified as Origin
import Mlatu.Parser (Bracketer, parserMatch, tokenSatisfy)
import Mlatu.Report qualified as Report
import Mlatu.Token (Token (..))
import Mlatu.Token qualified as Token
import Mlatu.Vocabulary qualified as Vocabulary
import Relude
import Text.Parsec ((<?>))
import Text.Parsec qualified as Parsec

-- | Desugars layout-based syntax into explicit brace-delimited blocks according
-- to the *layout rule*:
--
-- A layout block begins with a colon followed by a token whose source column is
-- greater than the indent level of the colon token, and contains all tokens
-- (and bracket-delimited groups of tokens) whose source column is greater than
-- or equal to that of the first token.
bracket ::
  (Informer m) =>
  FilePath ->
  [Located (Token 'Layout)] ->
  m [Located (Token 'Nonlayout)]
bracket path tokens =
  case Parsec.runParser insertBraces Vocabulary.global path tokens of
    Left parseError -> do
      report $ Report.parseError parseError
      halt
    Right result -> return result

insertBraces :: Bracketer [Located (Token 'Nonlayout)]
insertBraces = (concat <$> many unit) <* Parsec.eof

unit :: Bracketer [Located (Token 'Nonlayout)]
unit = unitWhere (const True)

unitWhere ::
  (Located (Token 'Layout) -> Bool) ->
  Bracketer [Located (Token 'Nonlayout)]
unitWhere predicate =
  Parsec.try (Parsec.lookAhead (tokenSatisfy predicate))
    *> Parsec.choice
      [ between BlockBegin BlockEnd,
        between GroupBegin GroupEnd,
        between VectorBegin VectorEnd,
        layoutBlock,
        one <$> (fromLayout =<< tokenSatisfy nonbracket)
      ]
    <?> "layout item"

between ::
  Token 'Layout ->
  Token 'Layout ->
  Bracketer [Located (Token 'Nonlayout)]
between open close = do
  begin <- fromLayout =<< parserMatch open
  inner <- concat <$> many unit
  end <- fromLayout =<< parserMatch close
  return (begin : inner ++ [end])

nonbracket :: Located (Token 'Layout) -> Bool
nonbracket = not . (`elem` brackets) . Located.item

brackets :: [Token 'Layout]
brackets =
  blockBrackets
    ++ [ GroupBegin,
         GroupEnd,
         VectorBegin,
         VectorEnd
       ]

blockBrackets :: [Token 'Layout]
blockBrackets =
  [ BlockBegin,
    BlockEnd,
    Colon
  ]

layoutBlock :: Bracketer [Located (Token 'Nonlayout)]
layoutBlock = do
  colon <- parserMatch Colon
  let colonOrigin = Located.origin colon
      Indent colonIndent = Located.indent colon
      validFirst =
        (> colonIndent)
          . Parsec.sourceColumn
          . Origin.begin
          . Located.origin
  firstToken <-
    Parsec.lookAhead (tokenSatisfy validFirst)
      <?> "a token with a source column greater than \
          \the start of the layout block"
  let firstOrigin = Origin.begin (Located.origin firstToken)
      inside =
        (>= Parsec.sourceColumn firstOrigin)
          . Parsec.sourceColumn
          . Origin.begin
          . Located.origin

  body <- concat <$> many (unitWhere inside)
  return $
    At colonOrigin (Indent colonIndent) BlockBegin :
    body ++ [At colonOrigin (Indent colonIndent) BlockEnd]

fromLayout ::
  Located (Token 'Layout) ->
  Bracketer (Located (Token 'Nonlayout))
fromLayout located = case Token.fromLayout (Located.item located) of
  Just nonlayout -> pure located {Located.item = nonlayout}
  Nothing -> Parsec.unexpected "colon not beginning valid layout block"
