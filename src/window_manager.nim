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
import strutils, tables, unicode, sequtils, sugar, hashes, algorithm, utils, os,
    ui_utils, termdiff, buffer

type
  Window* = ref object of RootObj

  Command* = object
    name*: string
    shortcut*: seq[Key]
    cmd*: proc () {.closure, tags: [ReadDirEffect, WriteIOEffect].}

  PaneKind* = enum PaneWindow, PaneSplitH, PaneSplitV

  Pane* = ref PaneObj

  PaneObj* = object
    case kind*: PaneKind:
      of PaneWindow:
        is_dragging*: bool
        window*: Window
      of PaneSplitH, PaneSplitV:
        factor*: float64
        pane_a*: Pane
        pane_b*: Pane
        selected*: bool

  Launcher* = ref object of Window
    app: App
    list: List

  WindowConstructor* = object
    name: string
    make: proc (app: App): Window {.noSideEffect, locks: 0, tags: [].}

  AppMode* = enum AppModeNone, AppModePane, AppModeNewPane

  App* = ref object
    root_pane*: Pane
    copy_buffer*: CopyBuffer
    window_constructors*: seq[WindowConstructor]
    mode*: AppMode
    buffers*: Table[string, Buffer]

method process_key*(window: Window, key: Key) {.
    base, locks: "unknown", tags: [WriteIOEffect, ReadIOEffect, ReadDirEffect].} = quit "Not implemented: process_key"

method process_mouse*(window: Window, mouse: Mouse): bool {.
    base, locks: "unknown", tags: [ReadIOEffect, WriteIOEffect].} = discard

method render*(window: Window, box: Box, ren: var TermRenderer) {.
    base, tags: [].} = quit "Not implemented: render"

method close*(window: Window) {.base, tags: [].} = discard

method list_commands*(window: Window): seq[Command] {.base, tags: [].} = discard

func make_launcher(app: App): Window =
  Launcher(app: app, list: make_list(app.window_constructors.map_it(it.name)))

func open_window*(pane: Pane, window: Window) {.tags: [].}

func open_selected(launcher: Launcher) {.locks: "unknown", tags: [].} =
  let selected = launcher.list.selected
  let window = launcher.app.window_constructors[selected].make launcher.app
  launcher.app.root_pane.open_window window

method process_key(launcher: Launcher, key: Key) {.locks: "unknown", tags: [].} =
  case key.kind:
    of LIST_KEYS: launcher.list.process_key key
    of KeyReturn: launcher.open_selected
    else: discard

method process_mouse(launcher: Launcher, mouse: Mouse): bool =
  if mouse.x == 0 and mouse.y == 0:
    return true
  var mouse_rel = mouse
  mouse_rel.x -= 2
  mouse_rel.y -= 1
  case mouse.kind:
    of MouseUp:
      if launcher.list.process_mouse mouse_rel:
        launcher.open_selected
    else:
      discard launcher.list.process_mouse mouse_rel

method render(launcher: Launcher, box: Box, ren: var TermRenderer) =
  render_border "Launcher", 1, box, ren
  launcher.list.render Box(min: box.min + Index2d(x: 2, y: 1), max: box.max), ren

func make_window*(app: App): Window =
  app.window_constructors[0].make app

func make_window_constructor*(name: string, make: proc (
    app: App): Window {.noSideEffect, tags: [].}): WindowConstructor =
  WindowConstructor(name: name, make: make)

type CommandSearch = ref object of Window
  app: App
  prev_window: Window
  list: List
  entry: Entry
  commands: seq[Command]
  shown_commands: seq[Command]

func display(cmd: Command): string =
  result = cmd.name
  if cmd.shortcut.len > 0:
    result &= " (" & $cmd.shortcut & ")"

func update_list(cmd_search: CommandSearch) =
  cmd_search.shown_commands = cmd_search.commands.filter_it((
      $cmd_search.entry.text).toLower in it.name.toLower)

  cmd_search.list.items = cmd_search.shown_commands.map_it(it.display.to_runes)

  cmd_search.list.selected = cmd_search.list.selected.clamp(0, 
      cmd_search.list.items.len - 1)

func make_command_search(app: App, prev_window: Window): Window =
  let cmd_search = CommandSearch(
    app: app,
    prev_window: prev_window,
    list: make_list(@[""]),
    entry: make_entry(app.copy_buffer),
    commands: prev_window.list_commands
  )
  cmd_search.commands.sort((a, b) => cmp(a.name.toLower, b.name.toLower))
  cmd_search.update_list
  return cmd_search

proc run_command(cmd_search: CommandSearch) {.tags: [ReadDirEffect, WriteIOEffect].} =
  let selected = cmd_search.list.selected
  if selected >= 0 and selected < cmd_search.shown_commands.len:
    cmd_search.app.root_pane.open_window cmd_search.prev_window
    cmd_search.shown_commands[selected].cmd()

method process_mouse(cmd_search: CommandSearch, mouse: Mouse): bool {.tags: [ReadDirEffect, WriteIOEffect], locks: "unknown".} =
  if mouse.x < len("Search:") and mouse.y == 0:
    return true
  else:
    var mouse_rel = mouse
    mouse_rel.x -= 2
    mouse_rel.y -= 2
    case mouse.kind:
      of MouseUp:
        if cmd_search.list.process_mouse mouse_rel:
          cmd_search.run_command
      else:
        discard cmd_search.list.process_mouse mouse_rel

method process_key(cmd_search: CommandSearch, key: Key) {.locks: "unknown".} =
  if key.kind == KeyEscape or (key.kind == KeyChar and key.chr == 'e' and key.ctrl):
    cmd_search.app.root_pane.open_window cmd_search.prev_window
  else:
    case key.kind:
      of KeyReturn: cmd_search.run_command
      of LIST_KEYS: cmd_search.list.process_key key
      else:
        cmd_search.entry.process_key key
        cmd_search.update_list

method render(cmd_search: CommandSearch, box: Box, ren: var TermRenderer) =
  let sidebar_width = len("Search:")
  render_border("Command Search", sidebar_width, box, ren)
  ren.move_to(box.min + Index2d(y: 1))
  ren.put "Search:", bright_black(), white()
  ren.move_to(box.min + Index2d(x: sidebar_width + 1, y: 1))
  cmd_search.entry.render ren
  cmd_search.list.render Box(min: box.min + Index2d(x: sidebar_width + 1, y: 2),
      max: box.max), ren

func select_below*(pane: Pane): bool =
  case pane.kind:
    of PaneWindow: return false
    of PaneSplitH:
      if pane.selected:
        return pane.pane_b.select_below
      else:
        return pane.pane_a.select_below
    of PaneSplitV:
      if pane.selected:
        return pane.pane_b.select_below
      else:
        if not pane.pane_a.select_below:
          pane.selected = true
        return true

func select_above*(pane: Pane): bool =
  case pane.kind:
    of PaneWindow: return false
    of PaneSplitH:
      if pane.selected:
        return pane.pane_b.select_above
      else:
        return pane.pane_a.select_above
    of PaneSplitV:
      if pane.selected:
        if not pane.pane_b.select_above:
          pane.selected = false
        return true
      else:
        return pane.pane_a.select_above

func select_left*(pane: Pane): bool =
  case pane.kind:
    of PaneWindow: return false
    of PaneSplitV:
      if pane.selected:
        return pane.pane_b.select_left
      else:
        return pane.pane_a.select_left
    of PaneSplitH:
      if pane.selected:
        if not pane.pane_b.select_left:
          pane.selected = false
        return true
      else:
        return pane.pane_a.select_left

func select_right*(pane: Pane): bool =
  case pane.kind:
    of PaneWindow: return false
    of PaneSplitV:
      if pane.selected:
        return pane.pane_b.select_right
      else:
        return pane.pane_a.select_right
    of PaneSplitH:
      if pane.selected:
        return pane.pane_b.select_right
      else:
        if not pane.pane_a.select_right:
          pane.selected = true
        return true

func close_active_pane*(pane: Pane) =
  if pane.kind == PaneSplitH or pane.kind == PaneSplitV:
    if pane.selected:
      if pane.pane_b.kind == PaneWindow:
        pane.pane_b.window.close
        pane[] = pane.pane_a[]
      else:
        pane.pane_b.close_active_pane
    else:
      if pane.pane_a.kind == PaneWindow:
        pane.pane_a.window.close
        pane[] = pane.pane_b[]
      else:
        pane.pane_a.close_active_pane

func split*(pane: Pane, dir: Direction, app: App) =
  case pane.kind:
    of PaneWindow:
      case dir:
        of DirUp, DirDown:
          pane[] = PaneObj(
            kind: PaneSplitV,
            factor: 0.5,
            pane_a: Pane(kind: PaneWindow, window: if dir ==
                DirUp: app.make_window else: pane.window),
            pane_b: Pane(kind: PaneWindow, window: if dir ==
                DirDown: app.make_window else: pane.window),
            selected: dir == DirDown
          )
        of DirLeft, DirRight:
          pane[] = PaneObj(
            kind: PaneSplitH,
            factor: 0.5,
            pane_a: Pane(kind: PaneWindow, window: if dir ==
                DirLeft: app.make_window else: pane.window),
            pane_b: Pane(kind: PaneWindow, window: if dir ==
                DirRight: app.make_window else: pane.window),
            selected: dir == DirRight
          )
    of PaneSplitH, PaneSplitV:
      if pane.selected:
        pane.pane_b.split dir, app
      else:
        pane.pane_a.split dir, app


func open_window*(pane: Pane, window: Window) {.tags: [].} =
  case pane.kind:
    of PaneWindow:
      pane.window = window
    of PaneSplitH, PaneSplitV:
      if pane.selected:
        pane.pane_b.open_window window
      else:
        pane.pane_a.open_window window

func active_window*(pane: Pane): Window =
  case pane.kind:
    of PaneWindow:
      return pane.window
    else:
      if pane.selected:
        return pane.pane_b.active_window
      else:
        return pane.pane_a.active_window

func process_mouse*(pane: Pane, mouse: Mouse, box: Box): (int, int) =
  case pane.kind:
    of PaneWindow:
      if pane.is_dragging:
        if mouse.kind == MouseUp:
          pane.is_dragging = false
        return (0, 0)
      else:
        var mouse_rel = mouse
        mouse_rel.x -= box.min.x
        mouse_rel.y -= box.min.y
        if pane.window.process_mouse(mouse_rel) and mouse.kind == MouseDown:
          pane.is_dragging = true
        return (-1, -1)
    of PaneSplitH:
      let split = (box.size.x.float64 * pane.factor).int + box.min.x
      let right = Box(min: Index2d(x: split, y: box.min.y), max: box.max)
      let left = Box(min: box.min, max: Index2d(x: split, y: box.max.y))
      var res = (-1, -1)
      if mouse.kind == MouseDown or mouse.kind == MouseScroll:
        if left.is_inside mouse.pos:
          if mouse.kind == MouseDown:
            pane.selected = false
          res = pane.pane_a.process_mouse(mouse, left)
        else:
          if mouse.kind == MouseDown:
            pane.selected = true
          res = pane.pane_b.process_mouse(mouse, right)
          if res[0] != -1:
            res[0] += 1
      else:
        if pane.selected:
          res = pane.pane_b.process_mouse(mouse, right)
          if res[0] != -1:
            res[0] += 1
        else:
          res = pane.pane_a.process_mouse(mouse, left)
      if res[0] == 1:
        pane.factor = (mouse.x - box.min.x) / box.size.x
        pane.factor = pane.factor.clamp(0, 1)
        return (-1, res[1])
      return res
    of PaneSplitV:
      let
        split = (box.size.y.float64 * pane.factor).int + box.min.y
        bottom = Box(min: Index2d(x: box.min.x, y: split), max: box.max)
        top = Box(min: box.min, max: Index2d(x: box.max.x, y: split))
      var res = (-1, -1)
      if mouse.kind == MouseDown or mouse.kind == MouseScroll:
        if top.is_inside mouse.pos:
          if mouse.kind == MouseDown:
            pane.selected = false
          res = pane.pane_a.process_mouse(mouse, top)
        else:
          if mouse.kind == MouseDown:
            pane.selected = true
          res = pane.pane_b.process_mouse(mouse, bottom)
          if res[1] != -1:
            res[1] += 1
      else:
        if pane.selected:
          res = pane.pane_b.process_mouse(mouse, bottom)
          if res[1] != -1:
            res[1] += 1
        else:
          res = pane.pane_a.process_mouse(mouse, top)
      if res[1] == 1:
        pane.factor = (mouse.y - box.min.y) / box.size.y
        pane.factor = pane.factor.clamp(0, 1)
        return (res[0], -1)
      return res

proc process_key*(pane: Pane, key: Key) {.tags: [ReadDirEffect, ReadIOEffect, WriteIOEffect].} =
  case pane.kind:
    of PaneWindow:
      pane.window.process_key key
    of PaneSplitH, PaneSplitV:
      if pane.selected:
        pane.pane_b.process_key key
      else:
        pane.pane_a.process_key key

proc render*(pane: Pane, box: Box, ren: var TermRenderer) {.tags: [].} =
  case pane.kind:
    of PaneWindow:
      ren.clip box
      pane.window.render box, ren
    of PaneSplitH:
      let split = (box.size.x.float64 * pane.factor).int + box.min.x
      pane.pane_a.render(Box(min: box.min, max: Index2d(x: split,
          y: box.max.y)), ren)
      pane.pane_b.render(Box(min: Index2d(x: split, y: box.min.y),
          max: box.max), ren)
    of PaneSplitV:
      let split = int(float64(box.size.y) * pane.factor) + box.min.y
      pane.pane_a.render(Box(min: box.min, max: Index2d(x: box.max.x,
          y: split)), ren)
      pane.pane_b.render(Box(min: Index2d(x: box.min.x, y: split),
          max: box.max), ren)

proc render*(pane: Pane, ren: var TermRenderer) {.tags: [].} =
  pane.render Box(
    min: Index2d(x: 0, y: 0),
    max: Index2d(x: ren.screen.width, y: ren.screen.height)
  ), ren

func open_launcher*(app: App) =
  app.root_pane.open_window app.make_launcher

func open_command_search(app: App) =
  app.root_pane.open_window(app.make_command_search app.root_pane.active_window)

func make_app*(window_constructors: seq[WindowConstructor]): owned App =
  return App(
    copy_buffer: make_copy_buffer(),
    root_pane: nil,
    window_constructors: window_constructors,
    buffers: init_table[string, Buffer]()
  )

proc make_buffer*(app: App, path: string): Buffer {.tags: [ReadIOEffect].} =
  if app.buffers.has_key path:
    return app.buffers[path]
  result = path.make_buffer
  app.buffers[path] = result

func is_changed*(app: App, path: string): bool =
  if not app.buffers.has_key path:
    return false
  return app.buffers[path].changed

func list_changed*(app: App): seq[string] =
  for path in app.buffers.keys:
    if app.buffers[path].changed:
      result.add path

proc process_mouse*(app: App, mouse: Mouse) {.tags: [ReadIOEffect, WriteIOEffect].} =
  discard app.root_pane.process_mouse(mouse, Box(
    min: Index2d(x: 0, y: 0),
    max: Index2d(x: terminal_width(), y: terminal_height())
  ))

proc process_key*(app: App, key: Key): bool {.tags: [ReadDirEffect, ReadIOEffect, WriteIOEffect].} =
  case app.mode:
    of AppModeNewPane:
      case key.kind:
        of KeyArrowDown:
          app.root_pane.split DirDown, app
        of KeyArrowUp:
          app.root_pane.split DirUp, app
        of KeyArrowLeft:
          app.root_pane.split DirLeft, app
        of KeyArrowRight:
          app.root_pane.split DirRight, app
        of KeyNone:
          return false
        else: discard
      app.mode = AppModeNone
      return
    of AppModePane:
      case key.kind:
        of KeyChar:
          if key.chr == Rune('n'):
            app.mode = AppModeNewPane
            return
          elif key.chr == Rune('a'):
            app.open_launcher
        of KeyArrowUp: discard app.root_pane.select_above
        of KeyArrowDown: discard app.root_pane.select_below
        of KeyArrowLeft: discard app.root_pane.select_left
        of KeyArrowRight: discard app.root_pane.select_right
        of KeyNone:
          return false
        else: discard
      app.mode = AppModeNone
      return
    of AppModeNone:
      case key.kind:
        of KeyQuit:
          return true
        of KeyChar:
          if key.ctrl:
            case key.chr:
              of Rune('q'): return true
              of Rune('w'):
                app.root_pane.close_active_pane
                return
              of Rune('p'):
                app.mode = AppModePane
                return
              else: discard
        of KeyArrowDown:
          if key.alt and not key.shift and not key.ctrl:
            discard app.root_pane.select_below
            return
        of KeyArrowUp:
          if key.alt and not key.shift and not key.ctrl:
            discard app.root_pane.select_above
            return
        of KeyArrowLeft:
          if key.alt and not key.shift and not key.ctrl:
            discard app.root_pane.select_left
            return
        of KeyArrowRight:
          if key.alt and not key.shift and not key.ctrl:
            discard app.root_pane.select_right
            return
        of KeyFn:
          if key.fn == 1:
            app.open_command_search
            return
        else: discard
      app.root_pane.process_key key
      return false

proc render*(app: App, ren: var TermRenderer) {.tags: [].} =
  app.root_pane.render ren

proc quit_app*() {.noconv, tags: [].} =
  reset_term()
