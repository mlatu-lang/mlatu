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
import utils, backend/common, unicode, os

when defined(TermBackend):
  import backend/term
else:
  import backend/sdl

export TermScreen, make_term_screen, height, `[]`
export setup_term, reset_term, read_key, read_mouse, terminal_width, terminal_height

export Color, BaseColor
export Key, KeyKind, Mouse, MouseKind, `$`

proc make_term_screen*(): owned TermScreen {.tags: [ReadEnvEffect].} =
  make_term_screen(terminal_width(), terminal_height())

func pos*(mouse: Mouse): Index2d =
  Index2d(x: mouse.x, y: mouse.y)

proc show_all*(screen: TermScreen) {.tags: [TimeEffect, ReadIOEffect,
    TerminalEffect, WriteIOEffect].} =
  if screen.width != 0 and screen.height != 0:
    var cur_style = screen.data[0]
    cur_style.apply_style
    for y in 0..<screen.height:
      set_cursor_pos 0, y
      for x in 0..<screen.width:
        cur_style.apply_style screen[x, y]
        screen[x, y].chr.term_write
        cur_style = screen[x, y]
    term_refresh()

proc apply*(prev, cur: TermScreen) {.tags: [TimeEffect, ReadIOEffect,
    TerminalEffect, WriteIOEffect].} =
  if prev.width == cur.width and prev.data.len == cur.data.len:
    if cur.width != 0 and cur.height != 0:
      var cur_style = cur.data[0]
      var pos = Index2d(x: 0, y: 0)
      set_cursor_pos 0, 0
      cur_style.apply_style
      for y in 0..<cur.height:
        for x in 0..<cur.width:
          if cur[x, y] != prev[x, y]:
            set_cursor_pos x, y
          cur_style.apply_style cur[x, y]
          cur[x, y].chr.term_write
          pos.x += 1
          cur_style = cur[x, y]
      term_refresh()
  else:
    cur.show_all

type TermRenderer* = object
  screen*: TermScreen
  pos*: Index2d
  clip_area*: Box

func make_term_renderer*(screen: TermScreen): TermRenderer =
  result = TermRenderer(
    screen: screen,
    pos: Index2d(x: 0, y: 0),
    clip_area: Box(min: Index2d(x: 0, y: 0), max: Index2d(x: screen.width,
        y: screen.height))
  )

func move_to*(ren: var TermRenderer, pos: Index2d) = ren.pos = pos

func move_to*(ren: var TermRenderer, x, y: int) =
  ren.move_to Index2d(x: x, y: y)

func put*(ren: var TermRenderer, rune: Rune, fg: Color = Color(
    base: ColorDefault), bg: Color = Color(base: ColorDefault),
    reverse: bool = false) =
  if ren.clip_area.is_inside ren.pos:
    let index = ren.pos.x + ren.pos.y * ren.screen.width
    ren.pos.x += 1
    if index < ren.screen.data.len:
      ren.screen.data[index].chr = rune
      ren.screen.data[index].fg = fg
      ren.screen.data[index].bg = bg
      ren.screen.data[index].reverse = reverse

func put*(ren: var TermRenderer, chr: char, fg: Color = Color(
    base: ColorDefault), bg: Color = Color(base: ColorDefault),
    reverse: bool = false) =
  ren.put chr.Rune, fg, bg, reverse

func put*(ren: var TermRenderer, chr: char, pos: Index2d) =
  ren.pos = pos
  ren.put chr

func put*(ren: var TermRenderer, chr: char, x, y: int) =
  ren.put chr, Index2d(x: x, y: y)

func put*(ren: var TermRenderer, str: seq[Rune], fg: Color = Color(
    base: ColorDefault), bg: Color = Color(base: ColorDefault),
    reverse: bool = false) =
  for it, rune in str.pairs:
    if ren.clip_area.is_inside (ren.pos + Index2d(x: it, y: 0)):
      let index = ren.pos.x + ren.pos.y * ren.screen.width + it
      if index < ren.screen.data.len:
        ren.screen.data[index].chr = rune
        ren.screen.data[index].fg = fg
        ren.screen.data[index].bg = bg
        ren.screen.data[index].reverse = reverse
      else: break
  ren.pos.x += str.len

func put*(ren: var TermRenderer, str: string, fg: Color = Color(
    base: ColorDefault), bg: Color = Color(base: ColorDefault),
    reverse: bool = false) =
  ren.put(str.to_runes, fg = fg, bg = bg, reverse = reverse)

func clip*(ren: var TermRenderer, area: Box) = ren.clip_area = area

func bright_black*(): Color = Color(base: ColorBlack, bright: true)

func black*(): Color = Color(base: ColorBlack, bright: false)

func bright_white*(): Color = Color(base: ColorWhite, bright: true)

func white*(): Color = Color(base: ColorWhite, bright: false)

func bright_red*(): Color = Color(base: ColorRed, bright: true)

func red*(): Color = Color(base: ColorRed, bright: false)

func bright_blue*(): Color = Color(base: ColorBlue, bright: true)

func blue*(): Color = Color(base: ColorBlue, bright: false)

func bright_magenta*(): Color = Color(base: ColorMagenta, bright: true)

func magenta*(): Color = Color(base: ColorMagenta, bright: false)
