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
import strutils, speccer, interpreter, nimline, term

type
  Repler* = ref object
    spec: Speccer
    eval: Interpreter
    editor: LineEditor

proc newRepler*(): Repler {.raises: [LineError].} =
  Repler(
    spec: newSpeccer(),
    eval: newInterpreter(),
    editor: initEditor(historyFile = "history.txt")
  )

proc update_completions(self: var Repler) {.raises: [].} =
  let type_safe_words = self.spec.type_safe_words
  let valid_words = self.spec.valid_words
  self.editor.completionCallback = proc(ed: LineEditor): seq[
      string] = 
      let words = ed.lineText.split(" ")
      if words.len <= 1: type_safe_words
      else: valid_words

proc read(self: var Repler): seq[Term] {.raises: [LineError].} =
  let line = self.editor.readLine("> ", false).strip
  for word in line.split:
    try:
      result.add Num(word.strip.parseInt)
    except ValueError:
      result.add Call(word.strip)

proc eval(self: var Repler, terms: seq[Term]): bool {.raises: [].} =
  result = false
  for term in terms:
    try:
      self.spec.check term
      self.eval.evaluate term
      result = true
    except StackError as e:
      echo e.message
      break

proc print(self: var Repler) {.raises: [].} =
  self.eval.display_stack

proc loop*(self: var Repler) {.raises: [LineError].} =
  while true:
    let terms: seq[Term] = self.read
    let any_success: bool = eval(self, terms)
    if any_success: 
      self.print
      self.update_completions
