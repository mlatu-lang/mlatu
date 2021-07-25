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
  EvalError* = ref object of ValueError
    index*: int
    message*: string

  TokKind = enum TokWord, TokLeftParen, TokRightParen, TokEqual, TokNum

  Tok = object
    start: int
    stop: int
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

func parse_word(input: string, start: int, stop: int): Tok =
  try: Tok(start: start, stop: stop, kind: TokNum, num: input.parse_int)
  except ValueError: Tok(start: start, stop: stop, kind: TokWord, word: input)

func parse*(input: string): seq[Tok] =
  var acc: string
  var acc_start: int = 0
  var index: int = 0
  while index < input.len:
    let c = input[index]
    if c in {' ', '\t', '\v', '\c', '\n', '\f', '(', ')', '='}:
      if acc.len > 0:
        result.add acc.parse_word(acc_start, index - 1)
        acc = ""
      if c == '(': result.add Tok(start: index, stop: index, kind: TokLeftParen)
      elif c == ')': result.add Tok(start: index, stop: index,
          kind: TokRightParen)
      elif c == '=': result.add Tok(start: index, stop: index, kind: TokEqual)
    else:
      if acc == "": acc_start = index
      acc.add c
    index.inc
  if acc.len > 0: result.add acc.parse_word(acc_start, index - 1)

func new_stack*(): Stack = @[]

func push_num(stack: var Stack, value: int) {.raises: [].} =
  stack.add Value(kind: ValueNum, num: value)

func push_quot(stack: var Stack, toks: seq[Tok]) {.raises: [].} =
  stack.add Value(kind: ValueQuot, toks: toks)

func push_val(stack: var Stack, value: Value) {.raises: [].} =
  stack.add value

func pop_num(stack: var Stack, index: int): int {.raises: [EvalError].} =
  try:
    let top = stack.pop
    case top.kind:
      of ValueNum: return top.num
      else: discard
  except IndexDefect: discard
  raise EvalError(index: index, message: "expected number on the stack")

func pop_quot(stack: var Stack, index: int): seq[Tok] {.raises: [EvalError].} =
  try:
    let top = stack.pop
    case top.kind:
      of ValueQuot: return top.toks
      else: discard
  except IndexDefect: discard
  raise EvalError(index: index, message: "expected quotation on the stack")

func pop_val(stack: var Stack, index: int): Value {.raises: [EvalError].} =
  try:
    return stack.pop
  except IndexDefect: discard
  raise EvalError(index: index, message: "expected value on the stack")

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

func eval*(stack: var Stack, state: var EvalState, toks: seq[Tok]) {.raises: [
    EvalError], tags: [].} =
  var mode = EvalMode(kind: EvalTop)
  var toks = toks.reversed()
  while toks.len > 0:
    let tok = toks.pop
    case mode.kind:
      of EvalTop:
        case tok.kind:
          of TokLeftParen:
            mode = EvalMode(kind: EvalQuot, toks: @[], depth: 0)
          of TokRightParen:
            raise EvalError(index: tok.start,
                message: "expected `(` before `)`")
          of TokEqual:
            let body = stack.pop_quot tok.start
            try:
              let tok = toks.pop
              case tok.kind:
                of TokWord:
                  state[tok.word] = body
                  continue
                else: discard
            except IndexDefect: discard
            raise EvalError(index: tok.stop,
                message: "expected word name after `=`")
          of TokNum: stack.push_num tok.num
          of TokWord:
            case tok.word:
              of "+":
                let a = stack.pop_num tok.start
                let b = stack.pop_num tok.start
                stack.push_num(b + a)
              of "-":
                let a = stack.pop_num tok.start
                let b = stack.pop_num tok.start
                stack.push_num(b - a)
              of "*":
                let a = stack.pop_num tok.start
                let b = stack.pop_num tok.start
                stack.push_num(b * a)
              of "/":
                let a = stack.pop_num tok.start
                let b = stack.pop_num tok.start
                stack.push_num(b /% a)
              of "dup":
                let a = stack.pop_val tok.start
                stack.push_val a
                stack.push_val a
              of "pop": discard stack.pop_val tok.start
              of "swap":
                let a = stack.pop_val tok.start
                let b = stack.pop_val tok.start
                stack.push_val a
                stack.push_val b
              of "dip":
                let a = stack.pop_quot tok.start
                let b = stack.pop_val tok.start
                stack.eval state, a
                stack.push_val b
              of "rollup":
                let a = stack.pop_val tok.start
                let b = stack.pop_val tok.start
                let c = stack.pop_val tok.start
                stack.push_val a
                stack.push_val c
                stack.push_val b
              of "rolldown":
                let a = stack.pop_val tok.start
                let b = stack.pop_val tok.start
                let c = stack.pop_val tok.start
                stack.push_val b
                stack.push_val a
                stack.push_val c
              of "rotate":
                let a = stack.pop_val tok.start
                let b = stack.pop_val tok.start
                let c = stack.pop_val tok.start
                stack.push_val a
                stack.push_val b
                stack.push_val c
              else:
                try: stack.eval(state, state[tok.word])
                except KeyError:
                  if toks.len > 0:
                    raise EvalError(index: tok.start,
                        message: "this word is unknown")
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

proc eval_prelude(): EvalState {.raises: [], tags: [].} =
  var stack: Stack = @[]
  let toks = "../prelude.mlt".static_read.parse
  try:
    stack.eval result, toks
    if stack.len > 0:
      raise newException(Defect, ("Prelude is ill-formed: stack contained items after evaluation (" &
          stack.display_stack & ")"))
  except EvalError as e:
    raise newException(Defect, ("Prelude is ill-formed: evaluation raised exception (" &
        e.message & ")"))

const PRELUDE* = eval_prelude()
