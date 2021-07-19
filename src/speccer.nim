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

import tables, term, patty

type
  StackError* = ref object of ValueError
    message*: string

  Value = enum
    valInt

  Stack = seq[Value]

  FunSpec = object
    before: Stack
    after: Stack

  Speccer* = ref object
    current: Stack
    table: Table[string, FunSpec]

func newSpeccer*(): Speccer {.raises: [].} =
  Speccer(
    current: @[],
    table: [
      ("pop", FunSpec(before: @[valInt], after: @[])),
      ("dup", FunSpec(before: @[valInt], after: @[valInt, valInt])),
      ("swap", FunSpec(before: @[valInt, valInt], after: @[valInt, valInt])),
      ("+", FunSpec(before: @[valInt, valInt], after: @[valInt])),
      ("-", FunSpec(before: @[valInt, valInt], after: @[valInt])),
      ("*", FunSpec(before: @[valInt, valInt], after: @[valInt])),
      ("/", FunSpec(before: @[valInt, valInt], after: @[valInt]))
    ].toTable
  )

func typeError(typ: FunSpec, current: Stack): StackError {.raises: [].} =
  let convert = proc(num: int): string =
    if num == 1:
      return $num & " element"
    else:
      return $num & " elements"
  return StackError(message:
    "I need a stack with at least " & typ.before.len.convert &
    ", but it currently has " & current.len.convert
  )

func check*(self: var Speccer, term: Term) {.raises: [StackError].} =
  var before: Stack
  var after: Stack
  match term:
    Num(_):
      before = @[]
      after = @[valInt]
    Call(name):
      if self.table.hasKey name:
        let inferred = self.table.getOrDefault name
        before = inferred.before
        after = inferred.after
      else:
        raise StackError(message: ("Unknown word: " & name))
  var new_spec = self.current
  for item in before.items:
    if new_spec.len == 0 or new_spec[new_spec.len - 1] != item:
      raise typeError(FunSpec(before: before, after: after), self.current)
    new_spec.newSeq (new_spec.len - 1)
  for item in after.items:
    new_spec.add(item)
  self.current = new_spec
