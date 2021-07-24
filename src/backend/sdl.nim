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
import tables, unicode, strutils, deques, hashes, colors, times, os, sdl2,
    sdl2/ttf, "../utils", common


type
  Texture = object
    w, h: int
    tex: TexturePtr

  Cursor = object
    pos: Index2d
    fg: common.Color
    bg: common.Color
    reverse: bool

  Terminal = ref object
    window: WindowPtr
    ren: RendererPtr
    is_fullscreen: bool

    cursor: Cursor
    screen: TermScreen

    bright_colors: seq[colors.Color]
    colors: seq[colors.Color]
    default_fg: colors.Color
    default_bg: colors.Color

    font: FontPtr
    font_size: int
    runes: Table[colors.Color, Table[Rune, Texture]]
    cell_size: Index2d

    key_queue: Deque[Key]
    mouse_queue: Deque[Mouse]

    mouse_buttons: array[3, bool]
    mouse_pos: Index2d
    mod_ctrl: bool
    mod_alt: bool
    mod_shift: bool

    ptime: DateTime

func mouse_button_to_id(btn: uint8): int =
  case btn:
    of BUTTON_LEFT: return 0
    of BUTTON_MIDDLE: return 1
    of BUTTON_RIGHT: return 2
    else: return -1

func set_draw_color(ren: RendererPtr, color: colors.Color) =
  let c = color.extract_rgb()
  ren.set_draw_color(c.r.uint8, c.g.uint8, c.b.uint8)

func fg_color(term: Terminal, color: common.Color): colors.Color =
  if color.base == ColorDefault:
    return term.default_fg
  if color.bright and color.base.ord < term.bright_colors.len:
      return term.bright_colors[color.base.ord]
  if (not color.bright) and color.base.ord < term.colors.len:
      return term.colors[color.base.ord]
  return term.default_fg

func bg_color(term: Terminal, color: common.Color): colors.Color =
  if color.base == ColorDefault:
    return term.default_bg
  if color.bright and color.base.ord < term.bright_colors.len:
      return term.bright_colors[color.base.ord]
  if (not color.bright) and color.base.ord < term.colors.len:
      return term.colors[color.base.ord]
  return term.default_bg

proc prerender_rune(term: Terminal, rune: Rune, color: colors.Color) {.tags: [TerminalEffect].} =
  if not term.runes.has_key(color):
    term.runes[color] = init_table[Rune, Texture]()

  let c = color.extract_rgb()
  var col: sdl2.Color
  col.r = uint8(c.r)
  col.g = uint8(c.g)
  col.b = uint8(c.b)
  col.a = 255

  let surf = term.font.render_glyph_blended(rune.uint16, col)
  if surf == nil:
    echo "[warning] Could not render rune: ", $rune.int32
    if rune == ' ':
      quit "Could not render ' '"
    if not term.runes[color].has_key(Rune(' ')):
      term.prerender_rune(Rune(' '), color)
    term.runes[color][rune] = term.runes[color][Rune(' ')]
    return
  let tex = term.ren.create_texture_from_surface(surf)
  term.runes[color][rune] = Texture(w: surf.w, h: surf.h, tex: tex)

proc draw_rune(term: Terminal, rune: Rune, color: colors.Color, x, y: int) {.tags: [TerminalEffect].} =
  if (not term.runes.has_key(color)) or (not term.runes[color].has_key(rune)):
    term.prerender_rune(rune, color)
  let tex = term.runes[color][rune]
  var to_rect: sdl2.Rect
  to_rect.x = x.cint
  to_rect.y = y.cint
  to_rect.w = tex.w.cint
  to_rect.h = tex.h.cint
  term.ren.copy(tex.tex, nil, to_rect.addr)

func add_modifiers(key: Key, term: Terminal): Key =
  result = key
  result.ctrl = term.mod_ctrl
  result.alt = term.mod_alt
  result.shift = term.mod_shift

func recompute_screen_size(term: Terminal) =
  var
    w: cint = 0
    h: cint = 0
  term.window.get_size(w, h)
  let
    x = max(w div term.cell_size.x, 1)
    y = max(h div term.cell_size.y, 1)
  if x != term.screen.width or y != term.screen.height:
    term.screen = make_term_screen(x, y)
    if term.key_queue.len == 0:
      term.key_queue.add_last(Key(kind: KeyUnknown).add_modifiers(term))

proc set_font_size(term: Terminal, font_size: int) {.tags: [ReadIOEffect, TerminalEffect].} =
  let font = open_font(cstring(get_app_dir() / "assets" / "font.ttf"), font_size.cint)
  if font == nil:
    return
  term.font = font
  term.font_size = font_size
  term.runes = init_table[colors.Color, Table[Rune, Texture]]()
  term.prerender_rune(Rune('a'), term.default_fg)
  term.cell_size = Index2d(
    x: term.runes[term.default_fg][Rune('a')].w,
    y: term.runes[term.default_fg][Rune('a')].h
  )
  term.recompute_screen_size()

proc redraw(term: Terminal) {.tags: [TimeEffect, TerminalEffect].} =
  let
    current_time = now()
  term.ptime = current_time

  term.ren.set_draw_color(0, 0, 0, 255)
  term.ren.clear()

  for x in 0..<term.screen.width:
    for y in 0..<term.screen.height:
      let cell = term.screen[x, y]
      var bg = term.bg_color(cell.bg)
      if cell.reverse:
        bg = term.fg_color(cell.fg)
      term.ren.set_draw_color(bg)
      var rect: sdl2.Rect
      rect.x = cint(x * term.cell_size.x)
      rect.y = cint(y * term.cell_size.y)
      rect.w = cint(term.cell_size.x)
      rect.h = cint(term.cell_size.y)
      term.ren.fill_rect(rect)

  for x in 0..<term.screen.width:
    for y in 0..<term.screen.height:
      let cell = term.screen[x, y]
      if cell.chr == ' ':
        continue
      var fg = term.fg_color(cell.fg)
      if cell.reverse:
        fg = term.bg_color(cell.bg)
      term.draw_rune(
        cell.chr,
        fg,
        x * term.cell_size.x,
        y * term.cell_size.y
      )


  term.ren.present()

proc update(term: Terminal): bool {.tags: [TimeEffect, ReadIOEffect, TerminalEffect].} =
  term.redraw()

  var evt = sdl2.default_event
  if not wait_event_timeout(evt, 100):
    return
  while true:
    case evt.kind:
      of QuitEvent:
        term.key_queue.add_last(Key(kind: KeyQuit).add_modifiers(term))
        return true
      of KeyDown:
        let event = cast[KeyboardEventPtr](evt.addr)
        case event.keysym.scancode:
          of SDL_SCANCODE_LCTRL, SDL_SCANCODE_RCTRL, SDL_SCANCODE_LGUI, SDL_SCANCODE_RGUI:
            term.mod_ctrl = true
          of SDL_SCANCODE_LALT:
            term.mod_alt = true
          of SDL_SCANCODE_LSHIFT, SDL_SCANCODE_RSHIFT:
            term.mod_shift = true
          of SDL_SCANCODE_BACKSPACE:
            term.key_queue.add_last(Key(kind: KeyBackspace).add_modifiers(term))
          of SDL_SCANCODE_RETURN, SDL_SCANCODE_KP_ENTER:
            term.key_queue.add_last(Key(kind: KeyReturn).add_modifiers(term))
          of SDL_SCANCODE_LEFT:
            term.key_queue.add_last(Key(kind: KeyArrowLeft).add_modifiers(term))
          of SDL_SCANCODE_RIGHT:
            term.key_queue.add_last(Key(kind: KeyArrowRight).add_modifiers(term))
          of SDL_SCANCODE_UP:
            term.key_queue.add_last(Key(kind: KeyArrowUp).add_modifiers(term))
          of SDL_SCANCODE_DOWN:
            term.key_queue.add_last(Key(kind: KeyArrowDown).add_modifiers(term))
          of SDL_SCANCODE_DELETE:
            term.key_queue.add_last(Key(kind: KeyDelete).add_modifiers(term))
          of SDL_SCANCODE_ESCAPE:
            term.key_queue.add_last(Key(kind: KeyEscape).add_modifiers(term))
          of SDL_SCANCODE_PAGEDOWN:
            term.key_queue.add_last(Key(kind: KeyPageDown).add_modifiers(term))
          of SDL_SCANCODE_PAGEUP:
            term.key_queue.add_last(Key(kind: KeyPageUp).add_modifiers(term))
          of SDL_SCANCODE_HOME:
            term.key_queue.add_last(Key(kind: KeyHome).add_modifiers(term))
          of SDL_SCANCODE_END:
            term.key_queue.add_last(Key(kind: KeyEnd).add_modifiers(term))
          of SDL_SCANCODE_TAB:
            if not term.mod_alt:
              var key = Key(kind: KeyChar, chr: 'i')
              if term.mod_shift:
                key = Key(kind: KeyChar, chr: 'I')
              key = key.add_modifiers(term)
              key.ctrl = true
              term.key_queue.add_last(key)
          of SDL_SCANCODE_F1:
            term.key_queue.add_last(Key(kind: KeyFn, fn: 1).add_modifiers(term))
          of SDL_SCANCODE_F2:
            term.key_queue.add_last(Key(kind: KeyFn, fn: 2).add_modifiers(term))
          of SDL_SCANCODE_F3:
            term.key_queue.add_last(Key(kind: KeyFn, fn: 3).add_modifiers(term))
          of SDL_SCANCODE_F4:
            term.key_queue.add_last(Key(kind: KeyFn, fn: 4).add_modifiers(term))
          of SDL_SCANCODE_F5:
            term.key_queue.add_last(Key(kind: KeyFn, fn: 5).add_modifiers(term))
          of SDL_SCANCODE_F6:
            term.key_queue.add_last(Key(kind: KeyFn, fn: 6).add_modifiers(term))
          of SDL_SCANCODE_F7:
            term.key_queue.add_last(Key(kind: KeyFn, fn: 7).add_modifiers(term))
          of SDL_SCANCODE_F8:
            term.key_queue.add_last(Key(kind: KeyFn, fn: 8).add_modifiers(term))
          of SDL_SCANCODE_F9:
            term.key_queue.add_last(Key(kind: KeyFn, fn: 9).add_modifiers(term))
          of SDL_SCANCODE_F11:
            term.is_fullscreen = not term.is_fullscreen
            if term.is_fullscreen:
              let display = term.window.get_display_index()
              var dm: DisplayMode
              discard get_current_display_mode(display, dm)
              term.window.set_size(dm.w, dm.h)
              discard term.window.set_fullscreen(SDL_WINDOW_FULLSCREEN)
            else:
              discard term.window.set_fullscreen(0)
            term.recompute_screen_size()
          else:
            if term.mod_ctrl or term.mod_alt:
              var handled = false
              if term.mod_ctrl:
                case Rune(event.keysym.sym):
                  of '+', '=':
                    term.set_font_size(term.font_size + 1)
                    handled = true
                  of '-', '_':
                    term.set_font_size(max(term.font_size - 1, 1))
                    handled = true
                  of '0':
                    term.set_font_size(12)
                    handled = true
                  of 'v':
                    if term.mod_shift:
                      let text = get_clipboard_text()
                      term.key_queue.add_last(Key(kind: KeyPaste,
                        text: ($text).to_runes(),
                      ).add_modifiers(term))
                      handled = true
                  else: discard
              if not handled:
                term.key_queue.add_last(Key(kind: KeyChar,
                  chr: Rune(event.keysym.sym),
                ).add_modifiers(term))
      of KeyUp:
        let event = cast[KeyboardEventPtr](evt.addr)
        case event.keysym.scancode:
          of SDL_SCANCODE_LCTRL, SDL_SCANCODE_RCTRL, SDL_SCANCODE_LGUI, SDL_SCANCODE_RGUI:
            term.mod_ctrl = false
          of SDL_SCANCODE_LALT:
            term.mod_alt = false
          of SDL_SCANCODE_LSHIFT, SDL_SCANCODE_RSHIFT:
            term.mod_shift = false
          else: discard
      of TextInput:
        let event = cast[TextInputEventPtr](evt.addr)
        var len = 0
        while event.text[len] != '\0':
          len += 1
        let runes = event.text[0..<len].join("").to_runes()
        if not term.mod_ctrl and not term.mod_alt:
          for rune in runes:
            term.key_queue.add_last(Key(kind: KeyChar, chr: rune).add_modifiers(term))
      of WindowEvent:
        term.recompute_screen_size()
      of MouseButtonDown, MouseButtonUp:
        let
          event = cast[MouseButtonEventPtr](evt.addr)
          button = event.button.mouse_button_to_id()
          x = event.x.int div term.cell_size.x
          y = event.y.int div term.cell_size.y
          clicks = event.clicks.int

        if button != -1:
          term.mouse_buttons[button] = evt.kind == MouseButtonDown

          term.key_queue.add_last(Key(kind: KeyMouse))
          case evt.kind:
            of MouseButtonDown:
              term.mouse_queue.add_last(Mouse(kind: MouseDown,
                button: button, x: x, y: y,
                clicks: clicks,
                buttons: term.mouse_buttons
              ))
            of MouseButtonUp:
              term.mouse_queue.add_last(Mouse(kind: MouseUp,
                button: button, x: x, y: y,
                clicks: clicks,
                buttons: term.mouse_buttons
              ))
            else: quit "unreachable"
      of MouseMotion:
        let
          event = cast[MouseMotionEventPtr](evt.addr)
          x = event.x.int div term.cell_size.x
          y = event.y.int div term.cell_size.y

        term.mouse_pos = Index2d(x: x, y: y)
        if term.mouse_queue.len > 0 and term.mouse_queue[^1].kind == MouseMove:
          term.mouse_queue[^1].x = x
          term.mouse_queue[^1].y = y
        else:
          term.key_queue.add_last(Key(kind: KeyMouse))
          term.mouse_queue.add_last(Mouse(kind: MouseMove,
            x: x, y: y,
            buttons: term.mouse_buttons
          ))
      of MouseWheel:
        let event = cast[MouseWheelEventPtr](evt.addr)

        if term.mouse_queue.len > 0 and term.mouse_queue[^1].kind == MouseScroll:
          term.mouse_queue[^1].delta -= event.y
        else:
          term.key_queue.add_last(Key(kind: KeyMouse))
          term.mouse_queue.add_last(Mouse(kind: MouseScroll,
            x: term.mouse_pos.x, y: term.mouse_pos.y,
            buttons: term.mouse_buttons, delta: -event.y
          ))
      else: discard
    if not poll_event(evt):
      return

func destroy(term: Terminal) =
  term.ren.destroy()
  term.window.destroy()

func write(term: Terminal, rune: Rune) =
  case rune:
    of Rune('\n'):
      term.cursor.pos.x = 0
      term.cursor.pos.y += 1
    of Rune('\r'):
      term.cursor.pos.x = 0
    else:
      term.screen[term.cursor.pos.x, term.cursor.pos.y] = CharCell(
        chr: rune,
        fg: term.cursor.fg,
        bg: term.cursor.bg,
        reverse: term.cursor.reverse
      )
      term.cursor.pos.x += 1

func write(term: Terminal, chr: char) = term.write(Rune(chr))

func move_to(term: Terminal, x, y: int) =
  term.cursor.pos = Index2d(x: x, y: y)

proc make_terminal(default_fg: colors.Color, default_bg: colors.Color): Terminal {.tags: [ReadIOEffect, TimeEffect, TerminalEffect].} =
  let
    window = create_window("Mlatu", 100, 100, 640, 480, SDL_WINDOW_RESIZABLE or
                                                         SDL_WINDOW_SHOWN)
    ren = window.create_renderer(-1, Renderer_PresentVSync)
    font = open_font(cstring(get_app_dir() / "assets" / "font.ttf"), 12)

  if font == nil:
    quit("Could not open font at " & get_app_dir() / "assets" / "font.ttf")

  result = Terminal(
    window: window,
    ren: ren,
    font: font,
    font_size: 12,
    screen: make_term_screen(80, 25),
    default_fg: default_fg,
    default_bg: default_bg,
    # Ubuntu default color scheme
    colors: @[ 
      rgb(46, 52, 54), # black
      rgb(204, 0, 0), # red
      rgb(78, 154, 0), # green
      rgb(196, 160, 0), # yellow
      rgb(114, 159, 207), # blue
      rgb(117, 80, 123), # magenta
      rgb(6, 152, 154), # cyan
      rgb(211, 215, 207) # white
    ],
    bright_colors: @[ 
      rgb(85, 87, 83), # bright black
      rgb(239, 41, 41), # bright red
      rgb(138, 226, 52), # bright green
      rgb(252, 233, 79), # bright yellow
      rgb(114, 159, 207), # bright blue
      rgb(173, 127, 168), # bright magenta
      rgb(52, 226, 226), # bright cyan
      rgb(238, 238, 236) # bright white
    ],
    cursor: Cursor(
      fg: common.Color(base: ColorDefault),
      bg: common.Color(base: ColorDefault),
      reverse: false
    ),
    ptime: now()
  )
  result.prerender_rune(Rune('a'), result.default_fg)
  result.cell_size = Index2d(
    x: result.runes[result.default_fg][Rune('a')].w,
    y: result.runes[result.default_fg][Rune('a')].h
  )
  window.set_size(
    cint(result.cell_size.x * result.screen.width),
    cint(result.cell_size.y * result.screen.height)
  )

# Interface
var terminal: Terminal

proc setup_term*(default_fg, default_bg: colors.Color) =
  sdl2.init(INIT_EVERYTHING)
  ttf_init()
  terminal = make_terminal(default_fg, default_bg)

proc set_cursor_pos*(x, y: int) = terminal.move_to(x, y)
proc term_write*(rune: Rune) = terminal.write(rune)
proc term_write*(chr: char) = terminal.write(chr)

proc apply_style*(cell: CharCell) =
  terminal.cursor.fg = cell.fg
  terminal.cursor.bg = cell.bg
  terminal.cursor.reverse = cell.reverse

proc apply_style*(a, b: CharCell) = b.apply_style()

proc term_refresh*() =
  discard terminal.update()

proc reset_term*() =
  terminal.destroy()
  ttf_quit()

proc read_key*(): Key =
  if terminal.key_queue.len == 0:
    return Key(kind: KeyNone)
  return terminal.key_queue.pop_first()

proc read_mouse*(): Mouse =
  if terminal.mouse_queue.len == 0:
    return Mouse(kind: MouseNone)
  return terminal.mouse_queue.pop_first()

proc terminal_width*(): int = terminal.screen.width
proc terminal_height*(): int = terminal.screen.height
