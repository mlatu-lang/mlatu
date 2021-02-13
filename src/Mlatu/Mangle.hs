-- |
-- Module      : Mlatu.Mangle
-- Description : Name mangling
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Mangle
  ( name,
  )
where

import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text qualified as Text
import Mlatu.Instantiated (Instantiated (Instantiated))
import Mlatu.Name (Qualified (..), Qualifier (..), Unqualified (..), qualifierName, unqualifiedName)
import Mlatu.Type (Constructor (..), Type (..), Var (..))
import Mlatu.Vocabulary qualified as Vocabulary
import Relude hiding (Type)
import Control.Lens ((^.))

-- | Mangles a fully qualified, fully saturated name into a linker symbol.
--
-- FIXME: This should use the platform C++ name mangling scheme, if possible.
name :: Instantiated -> Text
name (Instantiated n args) =
  Text.concat
  -- mlatu
  $
    ["_K", qualified n]
      ++ if null args
        then []
        else -- instantiate

          "_I" :
          map typ args
            -- end
            ++ ["_E"]

-- TODO: Use root.
qualified :: Qualified -> Text
qualified (Qualified (Qualifier _root parts) (Unqualified unqualified)) =
  Text.concat
  -- nested
  $
    "_N" :
    map (lengthPrefix . normalize) (parts ++ [unqualified])
      -- end
      ++ ["_E"]

lengthPrefix :: Text -> Text
lengthPrefix t = show (BS.length (encodeUtf8 t)) <> t

normalize :: Text -> Text
normalize = Text.concatMap go
  where
    go :: Char -> Text
    go c = case c of
      '@' -> "_a" -- at
      '\\' -> "_b" -- backslash
      '^' -> "_c" -- circumflex
      '.' -> "_d" -- dot
      '=' -> "_e" -- equal
      '/' -> "_f" -- fraction
      '>' -> "_g" -- greater
      '#' -> "_h" -- hash
      -- "_i"
      -- "_j"
      -- "_k"
      '<' -> "_l" -- less
      '-' -> "_m" -- minus
      '&' -> "_n" -- and ('n')
      -- "_o"
      '+' -> "_p" -- plus
      '?' -> "_q" -- question
      '%' -> "_r" -- remainder
      '*' -> "_s" -- star (asterisk)
      '~' -> "_t" -- tilde
      -- "_u"
      '|' -> "_v" -- vertical bar
      -- "_w"
      '!' -> "_x" -- exclamation
      -- "_y"
      -- "_z"
      '_' -> "__" -- underscore
      _
        | isDigit c || isAsciiLower c || isAsciiUpper c ->
          one c
        | otherwise ->
          Text.concat
            -- unicode
            ["_U", show $ ord c, "_"]

typ :: Type -> Text
typ t = case t of
  a :@ b ->
    Text.concat
      -- apply
      ["_A", typ a, typ b]
  TypeConstructor _ (Constructor constructor)
    | constructor ^. qualifierName == Vocabulary.global ->
      case constructor ^. unqualifiedName of
        "Bool" -> "_B" -- bool
        "Char" -> "_C" -- char
        "Float32" -> "_F4" -- float
        "Float64" -> "_F8"
        "Int8" -> "_I1" -- integer
        "Int16" -> "_I2"
        "Int32" -> "_I4"
        "Int64" -> "_I8"
        "List" -> "_L" -- list
        "UInt8" -> "_U1" -- unsigned
        "UInt16" -> "_U2"
        "UInt32" -> "_U4"
        "UInt64" -> "_U8"
        _ -> qualified constructor
    | otherwise ->
      qualified constructor
  TypeVar _ (Var _name i _kind) ->
    -- variable
    Text.concat ["_V", show i]
  TypeValue {} -> error "TODO: mangle type value"
  TypeConstant _ (Var _name i _) ->
    -- constant
    Text.concat ["_K", show i]
  Forall _ (Var _name i _) t' ->
    -- quantified
    Text.concat ["_Q", show i, typ t', "_E"]
