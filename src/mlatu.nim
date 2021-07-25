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

  TokKind = enum TokWord, TokLeftParen, TokRightParen, TokEqual, TokNum

  Tok = object 
    case kind: TokKind
      of TokWord: word: string
      of TokNum: num: int
      of TokLeftParen, TokRightParen, TokEqual: discard

  ValueKind = enum ValueNum, ValueQuot

  Value = object
    case kind: ValueKind:
      of ValueNum: num: int
      of ValueQuot: toks: seq[Tok]

  Stack* = seq[Value]

func parse_word(input: string): Tok =
  try: Tok(kind: TokNum, num: input.parse_int)
  except ValueError: Tok(kind: TokWord, word: input)

func parse*(input: string): seq[Tok] =
  var acc: string
  for c in input:
    if c in {' ', '\t', '\v', '\c', '\n', '\f', '(', ')', '='}: 
      if acc.len > 0: 
        result.add acc.parse_word
        acc = ""
      if c == '(': result.add Tok(kind: TokLeftParen)
      elif c == ')': result.add Tok(kind: TokRightParen)
      elif  c == '=': result.add Tok(kind: TokEqual)
    else:
      acc.add c
  if acc.len > 0: result.add acc.parse_word

func new_stack*(): Stack = @[]

func push_num(stack: var Stack, value: int) {.raises: [].} =
  stack.add Value(kind: ValueNum, num: value)

func push_quot(stack: var Stack, toks: seq[Tok]) {.raises: [].} =
  stack.add Value(kind: ValueQuot, toks: toks)

func push_val(stack: var Stack, value: Value) {.raises: [].} =
  stack.add value

func pop_num(stack: var Stack): int {.raises: [EvalError].} =
  try:
    let top = stack.pop
    case top.kind:
      of ValueNum: result = top.num
      of ValueQuot: raise newException(EvalError, "Expected number on the stack")
  except IndexDefect:
    raise newException(EvalError, "Expected number on the stack")

func pop_quot(stack: var Stack): seq[Tok] {.raises: [EvalError].} =
  try:
    let top = stack.pop
    case top.kind:
      of ValueNum: raise newException(EvalError, "Expected quotation on the stack")
      of ValueQuot: result = top.toks
  except IndexDefect:
    raise newException(EvalError, "Expected quotation on the stack")

func pop_val(stack: var Stack): Value {.raises: [EvalError].} =
  try:
    result = stack.pop
  except IndexDefect:
    raise newException(EvalError, "Expected value on the stack")

func unparse(value: Value): seq[Tok] {.raises: [].} =
  case value.kind:
    of ValueNum: result.add Tok(kind: TokNum, num: value.num)
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
            mode = EvalMode(kind: EvalQuot, toks: @[], depth: 0)
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
          of TokWord:
              case tok.word:
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
                of "ifz":
                  let a = stack.pop_quot
                  let b = stack.pop_quot
                  let c = stack.pop_num

                  if c == 0:
                    stack.push_quot a
                  else:
                    stack.push_quot b
                else:
                  try:
                    stack.eval(state, state[tok.word])
                  except KeyError:
                    raise newException(EvalError, "Unknown word `" & tok.word & "`")
      of EvalQuot: 
        if tok.kind == TokRightParen:
          if mode.depth > 0: mode.depth.dec
          else:
            stack.push_quot mode.toks
            mode = EvalMode(kind: EvalTop)
        else:
          if tok.kind == TokLeftParen: mode.depth.inc
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
    "succ" : "1 +".parse
  }.toTable

func `$`(tok: Tok): string =
  case tok.kind:
    of TokWord: tok.word
    of TokNum: $tok.num
    of TokEqual: "="
    of TokLeftParen: "("
    of TokRightParen: ")"

func display_stack*(stack: Stack): string =
  var acc: seq[Tok] = @[]
  for val in stack:
    acc &= val.unparse
  return acc.map_it($it).join(" ")
