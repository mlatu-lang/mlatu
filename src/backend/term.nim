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
import terminal, common, unicode

export terminal_width, terminal_height, set_style, set_cursor_pos, getch

proc term_write*(chr: char) {.raises: [], tags: [WriteIOEffect].} =
  stdout.write chr

proc term_write*(rune: Rune) {.raises: [IOError], tags: [WriteIOEffect].} =
  stdout.write $rune

proc term_refresh*() {.raises: [], tags: [WriteIOEffect].} = stdout.flush_file

proc set_fg*(fg: Color) {.raises: [IOError, ValueError], tags: [
    WriteIOEffect].} =
  case fg.base:
    of ColorBlack: stdout.set_foreground_color(fgBlack, fg.bright)
    of ColorRed: stdout.set_foreground_color(fgRed, fg.bright)
    of ColorGreen: stdout.set_foreground_color(fgGreen, fg.bright)
    of ColorYellow: stdout.set_foreground_color(fgYellow, fg.bright)
    of ColorBlue: stdout.set_foreground_color(fgBlue, fg.bright)
    of ColorMagenta: stdout.set_foreground_color(fgMagenta, fg.bright)
    of ColorCyan: stdout.set_foreground_color(fgCyan, fg.bright)
    of ColorWhite: stdout.set_foreground_color(fgWhite, fg.bright)
    of ColorDefault: stdout.set_foreground_color(fgDefault, fg.bright)

proc set_bg*(bg: Color) {.raises: [IOError, ValueError], tags: [
    WriteIOEffect].} =
  case bg.base:
    of ColorBlack: stdout.set_background_color(bgBlack, bg.bright)
    of ColorRed: stdout.set_background_color(bgRed, bg.bright)
    of ColorGreen: stdout.set_background_color(bgGreen, bg.bright)
    of ColorYellow: stdout.set_background_color(bgYellow, bg.bright)
    of ColorBlue: stdout.set_background_color(bgBlue, bg.bright)
    of ColorMagenta: stdout.set_background_color(bgMagenta, bg.bright)
    of ColorCyan: stdout.set_background_color(bgCyan, bg.bright)
    of ColorWhite: stdout.set_background_color(bgWhite, bg.bright)
    of ColorDefault: stdout.set_background_color(bgDefault, bg.bright)

proc apply_style*(a, b: CharCell) {.tags: [WriteIOEffect], raises: [IOError,
    ValueError].} =
  if a.fg != b.fg:
    b.fg.set_fg

  if a.bg != a.bg:
    b.bg.set_bg

  if a.reverse != b.reverse:
    if b.reverse:
      stdout.setStyle {styleReverse}
    else:
      stdout.set_style {}
      reset_attributes()
      b.fg.set_fg
      b.bg.set_bg

proc apply_style*(cell: CharCell) {.tags: [WriteIOEffect], raises: [IOError,
    ValueError].} =
  reset_attributes()
  cell.fg.set_fg
  cell.bg.set_bg
  if cell.reverse:
    stdout.set_style {styleReverse}
  else:
    stdout.set_style {}
  stdout.flush_file

proc setup_term*() {.tags: [WriteIOEffect], raises: [IOError].} =
  hide_cursor()
  erase_screen()

proc reset_term*() {.tags: [WriteIOEffect], raises: [IOError, ValueError].} =
  set_style {}
  reset_attributes()
  stdout.set_foreground_color fgDefault, false
  stdout.set_background_color bgDefault, false
  erase_screen()
  set_cursor_pos 0, 0
  show_cursor()

proc read_escape(): Key {.tags: [ReadIOEffect], raises: [IOError].} =
  result = Key(kind: KeyUnknown)
  if getch().ord == 91:
    case getch().ord:
      of 49:
        if getch().ord == 59:
          var shift = false
          var ctrl = false
          var alt = false
          var kind = KeyUnknown
          case getch().ord:
            of 50: shift = true
            of 51: alt = true
            of 52:
              shift = true
              alt = true
            of 53: ctrl = true
            of 54:
              ctrl = true
              shift = true
            else: discard
          case getch().ord:
            of 65: kind = KeyArrowUp
            of 66: kind = KeyArrowDown
            of 67: kind = KeyArrowRight
            of 68: kind = KeyArrowLeft
            else: return
          result = Key(kind: kind, shift: shift, ctrl: ctrl, alt: alt)
      of 51:
        if getch().ord == 126:
          result = Key(kind: KeyArrowDown)
      of 65: result = Key(kind: KeyArrowUp)
      of 66: result = Key(kind: KeyArrowDown)
      of 67: result = Key(kind: KeyArrowRight)
      of 68: result = Key(kind: KeyArrowLeft)
      else: discard

proc read_key*(): Key {.tags: [ReadIOEffect], raises: [IOError].} =
  let chr = getch()
  result = Key(kind: KeyChar, chr: chr.Rune)

  if chr.ord == 127:
    return Key(kind: KeyBackspace)

  if chr.ord == 13:
    return Key(kind: KeyReturn)

  if chr.ord == 27:
    return read_escape()

  if chr.ord <= 26:
    return Key(kind: KeyChar, chr: (chr.ord - 1 + 'a'.ord).char.Rune, ctrl: true)

proc read_mouse*(): Mouse = discard
