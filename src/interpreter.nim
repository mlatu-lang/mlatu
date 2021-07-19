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

import term, patty

type
  Interpreter* = ref object
    stack*: seq[int]

proc newInterpreter*(): Interpreter {.raises: [].} =
  Interpreter(stack: @[])

proc evaluate*(self: var Interpreter, term: Term) {.raises: [].} =
  match term:
    Num(val): self.stack.add val
    Call(name):
      case name:
        of "+":
          let first = self.stack.pop
          let second = self.stack.pop
          self.stack.add(second+first)
        of "-":
          let first = self.stack.pop
          let second = self.stack.pop
          self.stack.add(second-first)
        of "*":
          let first = self.stack.pop
          let second = self.stack.pop
          self.stack.add(second*first)
        of "/":
          let first = self.stack.pop
          let second = self.stack.pop
          self.stack.add(second /% first)
        of "pop": discard self.stack.pop
        of "dup":
          let top = self.stack.pop
          self.stack.add top
          self.stack.add top
        of "swap":
          let first = self.stack.pop
          let second = self.stack.pop
          self.stack.add first
          self.stack.add second
        else:
          raise newException(Defect, "Type checking did not cover this case")

proc display_stack*(self: Interpreter) {.raises: [].} =
  if self.stack.len != 0:
    var buffer = ""
    for item in self.stack:
      buffer.add($item & " ")
    echo buffer
