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

import os, strutils, parseopt, parser, speccer, interpreter

proc interpret_contents*(contents: string) {.raises: [].} =
  var parse = newParser()
  var spec = newSpeccer()
  var eval = newInterpreter()
  var all_success = true
  for word in contents.strip.split:
    try:
      let term = parse.parse word.strip
      spec.check term
      eval.evaluate term
    except StackError as e:
      echo e.message
      all_success = false
      break
  if all_success: eval.display_stack

proc repl() {.raises: [IOError].} =
  var parse = newParser()
  var spec = newSpeccer()
  var eval = newInterpreter()
  while true:
    stdout.write "> "
    stdout.flushFile
    try:
      var any_success = false
      let line = stdin.readLine.strip
      for word in line.split:
        try:
          let term = parse.parse word.strip
          spec.check term
          eval.evaluate term
          any_success = true
        except StackError as e:
          echo e.message
          break
      if any_success: eval.display_stack
    except IOError:
      stdout.write "\n"
      stdout.flushFile
      break

proc writeHelp() {.raises: [].} =
  echo "Mlatu: the best way forth"
  echo "The repl and interpreter for the Mlatu programming language"
  echo "Usage:"
  echo "\tmlatu [<filename>]"

proc writeVersion() {.raises: [].} =
  echo "Mlatu 0.1.0"

proc main*() {.raises: [IOError].} =
  setControlCHook(proc() {.noConv raises: [IOError].} = raise newException(
      IOError, "Control-C used"))

  var filename: string
  var should_repl: bool = true

  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if isValidFilename(key): filename = key
      else: echo key & " is not a valid file name"
    of cmdLongOption, cmdShortOption:
      case key
      of "help", "h":
        writeHelp()
        should_repl = false
      of "version", "v":
        writeVersion()
        should_repl = false
      of "stdin":
        filename = "stdin"
    of cmdEnd: false.assert

  case filename:
    of "":
      if should_repl: repl()
    of "stdin": stdin.readAll.interpret_contents
    else:
      echo "Interpreting file " & filename
      filename.readFile.interpret_contents


when isMainModule: main()
