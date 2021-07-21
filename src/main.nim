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

import parseopt, repl, os

proc writeHelp() {.raises: [], tags: [].} =
  echo "Mlatu: the best way forth"
  echo "The repl and interpreter for the Mlatu programming language"
  echo "Usage:"
  echo "\tmlatu [options]"
  echo "\t-h, --help     Display this help message"
  echo "\t-v, --version  Print the version"

proc writeVersion() {.raises: [], tags: [].} =
  echo "Mlatu 0.1.0"

proc main*() {.raises: [], tags: [ReadIOEffect, WriteIOEffect,
    ReadDirEffect].} =
  var shouldRepl: bool = true

  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      writeHelp()
      shouldRepl = false
    of cmdLongOption, cmdShortOption:
      case key
      of "help", "h":
        writeHelp()
        shouldRepl = false
      of "version", "v":
        writeVersion()
        shouldRepl = false
    of cmdEnd: false.assert

  if shouldRepl:
    try:
      var repler = newRepler()
      repler.loop
    except CatchableError as e:
      echo e.msg

when isMainModule: main()
