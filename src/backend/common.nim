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
import unicode, sequtils, strutils

type
  BaseColor* = enum
    ColorBlack = 0,
    ColorRed, ColorGreen, ColorYellow, ColorBlue,
    ColorMagenta, ColorCyan, ColorWhite, ColorDefault

  Color* = object
    base*: BaseColor
    bright*: bool

  CharCell* = object
    chr*: Rune
    fg*: Color
    bg*: Color
    reverse*: bool

  MouseKind* = enum
    MouseUnknown, MouseNone,
    MouseMove,
    MouseDown, MouseUp,
    MouseScroll

  Mouse* = object
    x*: int
    y*: int
    buttons*: array[3, bool]
    case kind*: MouseKind:
      of MouseDown, MouseUp:
        button*: int
        clicks*: int
      of MouseScroll:
        delta*: int
      of MouseUnknown:
        state*: uint64
      else: discard

  KeyKind* = enum
    KeyNone, KeyUnknown, KeyMouse,
    KeyChar, KeyReturn, KeyBackspace, KeyDelete, KeyEscape,
    KeyArrowLeft, KeyArrowRight, KeyArrowDown, KeyArrowUp
    KeyHome, KeyEnd, KeyPageUp, KeyPageDown, KeyFn,
    KeyPaste, KeyQuit

  Key* = object
    shift*: bool
    ctrl*: bool
    alt*: bool
    case kind*: KeyKind:
      of KeyChar: chr*: Rune
      of KeyUnknown: key_code*: int
      of KeyPaste: text*: seq[Rune]
      of KeyFn: fn*: int
      else: discard

  TerminalEffect* = object

type TermScreen* = ref object
  width*: int
  data*: seq[CharCell]

func make_term_screen*(w, h: int): owned TermScreen =
  let chr = CharCell(
    chr: Rune(' '),
    fg: Color(base: ColorDefault),
    bg: Color(base: ColorDefault)
  )
  result = TermScreen(width: w, data: repeat(chr, w * h))

func height*(screen: TermScreen): int =
  if screen.width == 0:
    result = 0
  else:
    result = screen.data.len div screen.width

func `[]`*(screen: TermScreen, x, y: int): CharCell =
  screen.data[x + y * screen.width]

func `[]=`*(screen: TermScreen, x, y: int, cell: CharCell) =
  screen.data[x + y * screen.width] = cell

func `$`*(key: Key): string =
  if key.ctrl:
    result &= "Ctrl + "
  if key.alt:
    result &= "Alt + "
  if key.shift:
    result &= "Shift + "

  case key.kind:
    of KeyUnknown: result &= "Unknown (" & $key.key_code & ")"
    of KeyChar: result &= key.chr
    of KeyFn: result &= "F" & $key.fn
    of KeyPaste: result &= "Paste (" & $key.text & ")"
    of KeyArrowUp: result &= "Up"
    of KeyArrowDown: result &= "Down"
    of KeyArrowLeft: result &= "Left"
    of KeyArrowRight: result &= "Home"
    of KeyHome: result &= "Home"
    of KeyEnd: result &= "End"
    of KeyPageUp: result &= "Page Up"
    of KeyPageDown: result &= "Page Down"
    of KeyEscape: result &= "Escape"
    of KeyDelete: result &= "Delete"
    of KeyReturn: result &= "Return"
    of KeyBackspace: result &= "Backspace"
    else: result &= $key.kind

func `$`*(keys: seq[Key]): string =
  keys.mapIt($it).join(" ")

func display_mouse_button(button: int): string =
  case button:
    of 0: result = "Left"
    of 1: result = "Middle"
    of 2: result = "Right"
    else: result = "Button" & $button

func `$`*(mouse: Mouse): string =
  case mouse.kind:
    of MouseDown: result &= "Down"
    of MouseUp: result &= "Up"
    of MouseMove: result &= "Move"
    of MouseScroll: result &= "Scroll"
    else: discard

  case mouse.kind:
    of MouseDown, MouseUp:
      result &= " " & mouse.button.display_mouse_button
      result &= " " & $mouse.clicks
    of MouseScroll:
      result &= " " & $mouse.delta
    else: discard

  var pressed_buttons: seq[string] = @[]
  for button, pressed in mouse.buttons.pairs:
    if pressed:
      pressed_buttons.add button.display_mouse_button

  result &= " {" & pressed_buttons.join(", ") & "}"
  result &= " (" & $mouse.x & ", " & $mouse.y & ")"
