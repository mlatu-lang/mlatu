# Copyright (C) 2021 (Caden Haustein) <mlatu@brightlysalty.33mail.com>
# This file is part of the Mlatu programming language.
#
# The Mlatu programming language is non-violent software: you can use, 
# redistribute, and/or modify it under the terms of the CNPLv6+ as found
# in the LICENSE file in the source code root directory or
# at <https://git.pixie.town/thufie/CNPL>.
#
# The Mlatu programming language comes with ABSOLUTELY NO WARRANTY, to the 
# extent permitted by applicable law.  See the CNPL for details.
import unicode, strutils

type
  Index2d* = object
    x*: int
    y*: int

  Box* = object
    min*: Index2d
    max*: Index2d

  Direction* = enum DirUp, DirDown, DirLeft, DirRight

func `+`*(a, b: Index2d): Index2d = Index2d(x: a.x + b.x, y: a.y + b.y)

func `-`*(a, b: Index2d): Index2d = Index2d(x: a.x - b.x, y: a.y - b.y)

func `*`*(a, b: Index2d): Index2d = Index2d(x: a.x * b.x, y: a.y * b.y)

func size*(box: Box): Index2d = box.max - box.min

func is_inside*(box: Box, pos: Index2d): bool =
  pos.x >= box.min.x and
    pos.x < box.max.x and
    pos.y >= box.min.y and
    pos.y < box.max.y

func to_index2d*(dir: Direction): Index2d =
  case dir:
    of DirUp: return Index2d(x: 0, y: -1)
    of DirDown: return Index2d(x: 0, y: 1)
    of DirLeft: return Index2d(x: -1, y: 0)
    of DirRight: return Index2d(x: 1, y: 0)

func is_ascii*(rune: Rune): bool = rune.int32 < 127

func to_char*(rune: Rune): char = rune.int32.char

func is_alpha_numeric*(rune: Rune): bool =
  rune.is_alpha or (rune.is_ascii and rune.to_char.is_digit)

func substr*(text: seq[Rune], first: int): seq[Rune] =
  result = new_seq[Rune](text.len - first)
  for it in first..<text.len:
    result[it - first] = text[it]

func substr*(text: seq[Rune], first, last: int): seq[Rune] =
  result = new_seq[Rune](last - first + 1)
  var it2 = 0
  for it in first.max(0)..last.min(text.len - 1):
    result[it2] = text[it]
    it2 += 1

func pattern_at*(text, pattern: seq[Rune], pos: int): bool =
  if pos + pattern.len <= text.len:
    result = true
    for it in 0..<pattern.len:
      if text[it + pos] != pattern[it]:
        return false
  else:
    result = false

func pattern_at*(text: seq[Rune], pattern: string, pos: int): bool =
  text.pattern_at pattern.to_runes, pos

func find*(text: seq[Rune], pattern: seq[Rune], start: int = 0): int =
  for it in start..<(text.len - pattern.len + 1):
    if text.pattern_at(pattern, it):
      return it
  return -1

func find_all*(text: seq[Rune], pattern: seq[Rune]): seq[int] =
  for it in 0..<(text.len - pattern.len + 1):
    if text.pattern_at(pattern, it):
      result.add it

func join*(strs: seq[seq[Rune]], chr: Rune): seq[Rune] =
  result = @[]
  for it, str in strs:
    if it != 0:
      result.add chr
    result &= str

func join*(strs: seq[seq[Rune]]): seq[Rune] =
  for str in strs:
    result &= str

func split*(str: seq[Rune], delimiter: Rune): seq[seq[Rune]] =
  result.add @[]
  for chr in str:
    if chr == delimiter:
      result.add @[]
    else:
      result[^1].add chr

func capitalize*(str: seq[Rune]): seq[Rune] =
  result = str
  if str.len != 0:
    result = @[str[0].to_upper] & str[1..^1]

converter to_rune*(chr: char): Rune = chr.Rune
