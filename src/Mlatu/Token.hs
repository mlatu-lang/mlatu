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

import Mlatu.Name (Unqualified)
import Relude

data Token
  = -- | @about@
    About
  | Alias
  | -- | @<@ See note [Angle Brackets].
    AngleBegin
  | -- | @>@ See note [Angle Brackets].
    AngleEnd
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
  | Codata
  | -- | @:@
    Colon
  | -- | @,@
    Comma
  | Data
  | -- | @define@
    Define
  | -- | @dot@
    Dot
  | -- | @else@
    Else
  | -- | @for
    For
  | Field
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
    Integer !Int
  | -- | @match@
    Match
  | Module
  | -- | @+@
    Operator !Unqualified
  | -- | @permission@
    Permission
  | -- | @\@
    Reference
  | -- | @"..."@
    Text !Text
  | Trait
  | -- | @[@
    VectorBegin
  | -- | @]@
    VectorEnd
  | -- | @where@
    Where
  | -- | @with@
    With
  | -- | @word@
    LowerWord !Unqualified
  | UpperWord !Unqualified

instance Eq Token where
  About == About = True
  Alias == Alias = True
  AngleBegin == AngleBegin = True
  AngleEnd == AngleEnd = True
  Arrow == Arrow = True
  As == As = True
  BlockBegin == BlockBegin = True
  BlockEnd == BlockEnd = True
  Case == Case = True
  Character a == Character b = a == b
  Codata == Codata = True
  Colon == Colon = True
  Comma == Comma = True
  Data == Data = True
  Define == Define = True
  Dot == Dot = True
  Else == Else = True
  -- See note [Float Literals].
  For == For = True
  Field == Field = True
  GroupBegin == GroupBegin = True
  GroupEnd == GroupEnd = True
  If == If = True
  Ignore == Ignore = True
  Instance == Instance = True
  Integer a == Integer b = a == b
  Match == Match = True
  Module == Module = True
  Operator a == Operator b = a == b
  Permission == Permission = True
  Reference == Reference = True
  Text a == Text b = a == b
  Trait == Trait = True
  VectorBegin == VectorBegin = True
  VectorEnd == VectorEnd = True
  Where == Where = True
  With == With = True
  LowerWord a == LowerWord b = a == b
  UpperWord a == UpperWord b = a == b
  _ == _ = False
