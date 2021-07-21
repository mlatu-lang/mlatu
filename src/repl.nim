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
import strutils, sequtils, speccer, interpreter, nimline, term, os

type
  Repler* = ref object
    spec: Speccer
    eval: Interpreter
    editor: LineEditor

proc newRepler*(): Repler
  {.raises: [IOError], tags: [WriteIOEffect, ReadIOEffect, ReadDirEffect].} =
  Repler(
    spec: newSpeccer(),
    eval: newInterpreter(),
    editor: initEditor(historyFile = "history.txt")
  )

func parse(word: string): Term {.raises: [], tags: [].} =
  try:
    result = num(word.strip.parseInt)
  except ValueError:
    result = call(word.strip)

proc updateCompletions(self: var Repler) {.raises: [], tags: [].} =
  let fun = completeWords(self.spec)
  self.editor.completionCallback =
    (proc (ed: LineEditor): seq[string] {.raises: [], tags: [].} = fun(
        ed.lineText.strip.split.mapIt(it.parse)))

proc read(self: var Repler): seq[Term]
  {.raises: [IOError, ValueError], tags: [ReadIOEffect, WriteIOEffect].} =
  return self.editor.readLine("> ", false).strip.split.map(parse)

proc eval(self: var Repler, terms: seq[Term]): bool {.raises: [], tags: [].} =
  result = false
  for term in terms:
    try:
      self.spec.check term
      self.eval.evaluate term
      result = true
    except StackError as e:
      echo e.message
      break

proc print(self: var Repler) {.raises: [], tags: [].} =
  self.eval.displayStack

proc loop*(self: var Repler)
  {.raises: [IOError, ValueError], tags: [ReadIOEffect, WriteIOEffect].} =
  while true:
    self.updateCompletions
    let any_success: bool = eval(self, self.read)
    if any_success:
      self.print
