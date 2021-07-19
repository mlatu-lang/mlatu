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

import term, strutils

type Parser* = ref object

func newParser*(): Parser {.raises: [].} =
  Parser()

func parse*(self: Parser, word: string): Term {.raises: [].} =
  try:
    return Num(word.parseInt)
  except ValueError:
    return Call(word)
