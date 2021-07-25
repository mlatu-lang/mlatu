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

import strutils, sequtils, tables, algorithm

type
  EvalError* = object of ValueError

  TokKind = enum TokWord, TokLeftParen, TokRightParen, TokEqual, TokNum, TokBool

  Tok = object 
    case kind: TokKind
      of TokWord: word: string
      of TokNum: num: int
      of TokBool: bool: bool
      of TokLeftParen, TokRightParen:
        depth: int
      of TokEqual: discard

  ValueKind = enum ValueNum, ValueBool, ValueQuot

  Value = object
    case kind: ValueKind:
      of ValueNum: num: int
      of ValueBool: bool: bool
      of ValueQuot: toks: seq[Tok]

  Stack* = seq[Value]

func `==`(a, b: Tok): bool =
  case a.kind:
    of TokWord: return b.kind == TokWord and a.word == b.word
    of TokNum: return b.kind == TokNum and a.num == b.num
    of TokBool: return b.kind == TokBool and a.bool == b.bool
    of TokLeftParen: return b.kind == TokLeftParen
    of TokRightParen: return b.kind == TokRightParen
    of TokEqual: return b.kind == TokEqual

func `==`(a, b: Value): bool =
  case a.kind:
    of ValueNum: return b.kind == ValueNum and a.num == b.num
    of ValueQuot: return b.kind == ValueQuot and a.toks == b.toks
    of ValueBool: return b.kind == ValueBool and a.bool == b.bool

func parse_word(input: string): Tok =
  try: Tok(kind: TokNum, num: input.parse_int)
  except ValueError: Tok(kind: TokWord, word: input)

func parse*(input: string): seq[Tok] =
  var acc: string
  var depth: int 
  for c in input:
    if c in {' ', '\t', '\v', '\c', '\n', '\f', '(', ')', '='}: 
      if acc.len > 0: 
        result.add acc.parse_word
        acc = ""
      if c == '(': 
        result.add Tok(kind: TokLeftParen, depth: depth)
        depth.inc
      elif c == ')': 
        depth.dec
        result.add Tok(kind: TokRightParen, depth: depth)
      elif  c == '=': result.add Tok(kind: TokEqual)
    else:
      acc.add c
  if acc.len > 0: result.add acc.parse_word
  while depth > 0:
    depth.dec
    result.add Tok(kind: TokRightParen, depth: depth)

func new_stack*(): Stack = @[]

func push_num(stack: var Stack, value: int) {.raises: [].} =
  stack.add Value(kind: ValueNum, num: value)

func push_bool(stack: var Stack, value: bool) {.raises: [].} =
  stack.add Value(kind: ValueBool, bool: value)

func push_quot(stack: var Stack, toks: seq[Tok]) {.raises: [].} =
  stack.add Value(kind: ValueQuot, toks: toks)

func push_val(stack: var Stack, value: Value) {.raises: [].} =
  stack.add value

func pop_num(stack: var Stack): int {.raises: [EvalError].} =
  try:
    let top = stack.pop
    case top.kind:
      of ValueNum: result = top.num
      else: raise newException(EvalError, "Expected number on the stack")
  except IndexDefect:
    raise newException(EvalError, "Expected number on the stack")

func pop_quot(stack: var Stack): seq[Tok] {.raises: [EvalError].} =
  try:
    let top = stack.pop
    case top.kind:
      of ValueQuot: result = top.toks
      else: raise newException(EvalError, "Expected quotation on the stack")
  except IndexDefect:
    raise newException(EvalError, "Expected quotation on the stack")

func pop_bool(stack: var Stack): bool {.raises: [EvalError].} = 
  try:
    let top = stack.pop
    case top.kind:
      of ValueBool: result = top.bool
      else: raise newException(EvalError, "Expected boolean on the stack")
  except IndexDefect:
    raise newException(EvalError, "Expected boolean on the stack")

func pop_val(stack: var Stack): Value {.raises: [EvalError].} =
  try:
    result = stack.pop
  except IndexDefect:
    raise newException(EvalError, "Expected value on the stack")

func unparse(value: Value): seq[Tok] {.raises: [].} =
  case value.kind:
    of ValueNum: result.add Tok(kind: TokNum, num: value.num)
    of ValueBool: result.add Tok(kind: TokBool, bool: value.bool)
    of ValueQuot: 
      result.add Tok(kind: TokLeftParen)
      result &= value.toks
      result.add Tok(kind: TokRightParen)

type
  EvalState* = Table[string, seq[Tok]]

  EvalModeKind = enum EvalTop, EvalQuot
  EvalMode = object
    case kind: EvalModeKind:
      of EvalTop: discard
      of EvalQuot: 
        toks: seq[Tok]
        depth: int

func eval*(stack: var Stack, state: var EvalState, toks: seq[Tok]) {.raises: [EvalError], tags: [].} =
  var mode = EvalMode(kind: EvalTop)
  var toks = toks.reversed()
  while toks.len > 0:
    let tok = toks.pop
    case mode.kind:
      of EvalTop:
        case tok.kind:
          of TokLeftParen: 
            mode = EvalMode(kind: EvalQuot, toks: @[], depth: tok.depth)
          of TokRightParen: raise newException(EvalError, "Expected `(` before `)`")
          of TokEqual: 
            let body = stack.pop_quot
            try:
              let tok = toks.pop
              case tok.kind:
                of TokWord: state[tok.word] = body
                else: raise newException(EvalError, "Expected word name after `=`")
            except IndexDefect:
              raise newException(EvalError, "Expected word name after `=`")
          of TokNum: stack.push_num tok.num
          of TokBool: stack.push_bool tok.bool
          of TokWord:
              case tok.word:
                of "true":
                  stack.push_bool(true)
                of "false":
                  stack.push_bool(false)
                of "and":
                  let a = stack.pop_bool
                  let b = stack.pop_bool

                  stack.push_bool(a and b)
                of "not":
                  let a = stack.pop_bool

                  stack.push_bool(not a)
                of "or":
                  let a = stack.pop_bool
                  let b = stack.pop_bool

                  stack.push_bool(a or b)
                of "gt":
                  let a = stack.pop_num
                  let b = stack.pop_num

                  stack.push_bool(b > a)
                of "geq":
                  let a = stack.pop_num
                  let b = stack.pop_num

                  stack.push_bool(b >= a)
                of "lt":
                  let a = stack.pop_num
                  let b = stack.pop_num

                  stack.push_bool(b < a)
                of "leq":
                  let a = stack.pop_num
                  let b = stack.pop_num

                  stack.push_bool(b <= a)
                of "eq":
                  let a = stack.pop_val
                  let b = stack.pop_val

                  stack.push_bool(a == b)
                of "+":
                  let a = stack.pop_num
                  let b = stack.pop_num
                  stack.push_num(b + a)
                of "-":
                  let a = stack.pop_num
                  let b = stack.pop_num
                  stack.push_num(b - a)
                of "*":
                  let a = stack.pop_num
                  let b = stack.pop_num
                  stack.push_num(b * a)
                of "/":
                  let a = stack.pop_num
                  let b = stack.pop_num
                  stack.push_num(b /% a)
                of "dup":
                  let a = stack.pop_val
                  stack.push_val a
                  stack.push_val a
                of "pop": discard stack.pop_val
                of "swap":
                  let a = stack.pop_val
                  let b = stack.pop_val
                  stack.push_val a
                  stack.push_val b
                of "dip":
                  let a = stack.pop_quot
                  let b = stack.pop_val
                  stack.eval state, a
                  stack.push_val b
                of "rollup":
                  let a = stack.pop_val
                  let b = stack.pop_val
                  let c = stack.pop_val
                  stack.push_val a
                  stack.push_val c
                  stack.push_val b
                of "rolldown":
                  let a = stack.pop_val
                  let b = stack.pop_val
                  let c = stack.pop_val
                  stack.push_val b
                  stack.push_val a
                  stack.push_val c
                of "rotate":
                  let a = stack.pop_val
                  let b = stack.pop_val
                  let c = stack.pop_val
                  stack.push_val a
                  stack.push_val b
                  stack.push_val c
                of "cat":
                  let a = stack.pop_quot
                  let b = stack.pop_quot

                  stack.push_quot(b & a)
                of "i":
                  stack.eval(state, stack.pop_quot)
                of "if":
                  let a = stack.pop_quot
                  let b = stack.pop_quot
                  let c = stack.pop_bool

                  if c:
                    stack.eval(state, b)
                  else:
                    stack.eval(state, a)
                of "cons":
                  let a = stack.pop_quot
                  let b = stack.pop_val.unparse
                  stack.push_quot(b & a)
                else:
                  try:
                    stack.eval(state, state[tok.word])
                  except KeyError:
                    raise newException(EvalError, "Unknown word `" & tok.word & "`")
      of EvalQuot: 
        if tok.kind == TokRightParen and tok.depth == mode.depth:
          stack.push_quot mode.toks
          mode = EvalMode(kind: EvalTop)
        else:
          mode.toks.add tok
  if mode.kind == EvalQuot:
    stack.push_quot mode.toks

func init_state*(): EvalState {.raises: [], tags: [].} =
  {
    "id" : "".parse, 
    "popd" : "(pop) dip".parse,
    "dupd" : "(dup) dip".parse,
    "swapd" : "(swap) dip".parse,
    "rollupd" : "(rollup) dip".parse,
    "rolldownd" : "(rolldown) dip".parse,
    "rotated" : "(rotate) dip".parse,
    "pred" : "1 -".parse,
    "succ" : "1 +".parse,
    "neq" : "eq not".parse,
    "itob" : "0 neq".parse,
    "btoi" : "(1) (0) if".parse,
    "repeat" : "dupd pred dup itob rollup (repeat cat) cons cons () if".parse,
  }.toTable

func `$`(tok: Tok): string =
  case tok.kind:
    of TokWord: tok.word
    of TokNum: $tok.num
    of TokBool: $tok.bool
    of TokEqual: "="
    of TokLeftParen: "("
    of TokRightParen: ")"

func display_stack*(stack: Stack): string =
  var acc: seq[Tok] = @[]
  for val in stack:
    acc &= val.unparse
  return acc.map_it($it).join(" ")
