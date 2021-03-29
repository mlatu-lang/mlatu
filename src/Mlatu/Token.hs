{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      : Mlatu.Token
-- Description : Tokens produced by the tokenizer
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Token
  ( Token (..),
  )
where

import Mlatu.Literal (FloatLiteral, IntegerLiteral)
import Mlatu.Name (Unqualified)
import Relude

data Token
  = -- | @about@
    About
  | -- | @->@
    Arrow
  | -- | @as@
    As
  | -- | @{@, @:@
    BlockBegin
  | -- | @}@
    BlockEnd
  | -- | @case@
    Case
  | -- | @'x'@
    Character !Char
  | -- | @class@
    Class
  | -- | @:@
    Colon
  | -- | @,@
    Comma
  | -- | @define@
    Define
  | -- | @do@
    Do
  | -- | @...@
    Ellipsis
  | -- | @else@
    Else
  | -- | See note [Float Literals].
    Float !FloatLiteral
  | -- | @for@
    For
  | -- | @(@
    GroupBegin
  | -- | @)@
    GroupEnd
  | -- | @if@
    If
  | -- | @_@
    Ignore
  | -- | @instance@
    Instance
  | -- | @1@, 0b1@, @0o1@, @0x1@, @1i64, @1u16@
    Integer !IntegerLiteral
  | -- | @intrinsic@
    Intrinsic
  | -- | @match@
    Match
  | -- | @method@
    Method
  | -- | @+@
    Operator !Unqualified
  | -- | @permission@
    Permission
  | -- | @\@
    Reference
  | -- | @"..."@
    Text !Text
  | -- | @trait@
    Type
  | -- | @[@
    VectorBegin
  | -- | @]@
    VectorEnd
  | -- | @vocab@
    Vocab
  | -- | @::@
    VocabLookup
  | -- | @where@
    Where
  | -- | @with@
    With
  | -- | @word@
    Word !Unqualified

instance Eq Token where
  About == About = True
  Arrow == Arrow = True
  As == As = True
  BlockBegin == BlockBegin = True
  BlockEnd == BlockEnd = True
  Case == Case = True
  Character a == Character b = a == b
  Class == Class = True
  Colon == Colon = True
  Comma == Comma = True
  Define == Define = True
  Do == Do = True
  Ellipsis == Ellipsis = True
  Else == Else = True
  -- See note [Float Literals].
  Float a == Float b = a == b
  For == For = True
  GroupBegin == GroupBegin = True
  GroupEnd == GroupEnd = True
  If == If = True
  Ignore == Ignore = True
  Instance == Instance = True
  Integer a == Integer b = a == b
  Intrinsic == Intrinsic = True
  Match == Match = True
  Method == Method = True
  Operator a == Operator b = a == b
  Permission == Permission = True
  Reference == Reference = True
  Text a == Text b = a == b
  Type == Type = True
  VectorBegin == VectorBegin = True
  VectorEnd == VectorEnd = True
  Vocab == Vocab = True
  VocabLookup == VocabLookup = True
  Where == Where = True
  With == With = True
  Word a == Word b = a == b
  _ == _ = False

-- Note [Angle Brackets]:
--
-- Since we separate the passes of tokenization and parsing, we are faced with a
-- classic ambiguity between angle brackets as used in operator names such as
-- '>>' and '<+', and as used in type argument and parameter lists such as
-- 'vector<vector<T>>' and '<+E>'.
--
-- Our solution is to parse a less-than or greater-than character as an 'angle'
-- token if it was immediately followed by a symbol character in the input, with
-- no intervening whitespace. This is enough information for the parser to
-- disambiguate the intent:
--
--   • When parsing an expression, it joins a sequence of angle tokens and
--     an operator token into a single operator token.
--
--   • When parsing a signature, it treats them separately.
--
-- You may ask why we permit this silly ambiguity in the first place. Why not
-- merge the passes of tokenization and parsing, or use a different bracketing
-- character such as '[]' for type argument lists?
--
-- We separate tokenization and parsing for the sake of tool support: it's
-- simply easier to provide token-accurate source locations when we keep track
-- of source locations at the token level, and it's easier to provide a list of
-- tokens to external tools (e.g., for syntax highlighting) if we already have
-- such a list at hand.
--
-- The reason for the choice of bracketing character is for the sake of
-- compatibility with C++ tools. When setting a breakpoint in GDB, for example,
-- it's nice to be able to type:
--
--     break foo::bar<int>
--
-- And for this to refer to the Mlatu definition 'foo::bar<int>' precisely,
-- rather than to some syntactic analogue such as 'foo.bar[int]'. The modest
-- increase in complexity of implementation is justified by fostering a better
-- experience for people.
