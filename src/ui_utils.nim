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
import strutils, deques, unicode, hashes, sets, sequtils
import utils, termdiff

type CopyBuffer* = ref object
  history*: Deque[seq[Rune]]

func make_copy_buffer*(): owned CopyBuffer =
  CopyBuffer(history: init_deque[seq[Rune]]())

func copy*(buffer: CopyBuffer, str: seq[Rune]) =
  buffer.history.add_last str

func paste*(buffer: CopyBuffer): seq[Rune] =
  if buffer.history.len > 0:
    result = buffer.history[buffer.history.len - 1]

type
  CursorKind* = enum CursorInsert, CursorSelection

  Cursor* = object
    case kind*: CursorKind:
      of CursorInsert: pos*: int
      of CursorSelection:
        start*: int
        stop*: int

func sort*(cursor: Cursor): Cursor =
  result = cursor
  case cursor.kind:
    of CursorInsert: discard
    of CursorSelection:
      if cursor.start > cursor.stop:
        result = Cursor(kind: CursorSelection, start: cursor.stop,
            stop: cursor.start)

func move*(cursor: var Cursor, delta: int, max_pos: int) =
  case cursor.kind:
    of CursorInsert:
      cursor.pos += delta
      cursor.pos = cursor.pos.clamp(0, max_pos)
    of CursorSelection:
      cursor.start += delta
      cursor.stop += delta

      cursor.start = cursor.start.clamp(0, max_pos)
      cursor.stop = cursor.stop.clamp(0, max_pos)

func get_pos*(cursor: Cursor): int =
  case cursor.kind:
    of CursorInsert: cursor.pos
    of CursorSelection: cursor.stop

func is_under*(cursor: Cursor, pos: int): bool =
  case cursor.kind:
    of CursorInsert: pos == cursor.pos
    of CursorSelection:
      (pos >= cursor.start and pos < cursor.stop) or (pos >= cursor.stop and
          pos < cursor.start)

func hash*(cursor: Cursor): Hash =
  case cursor.kind:
    of CursorInsert: !$(cursor.kind.hash !& cursor.pos.hash)
    of CursorSelection: !$(cursor.kind.hash !& cursor.start.hash !&
        cursor.stop.hash)

func `==`*(a, b: Cursor): bool =
  if a.kind == b.kind:
    case a.kind:
      of CursorInsert: a.pos == b.pos
      of CursorSelection: a.start == b.start and a.stop == b.stop
  else: false

func merge_cursors*(cursors: seq[Cursor]): seq[Cursor] =
  var yet = init_hash_set[Cursor]()
  for cursor in cursors:
    if cursor notin yet:
      result.add cursor
      yet.incl cursor

type Entry* = object
  text*: seq[Rune]
  cursor: Cursor
  copy_buffer*: CopyBuffer

func delete_selected(entry: var Entry) =
  if entry.cursor.kind == CursorSelection:
    let
      cur = entry.cursor.sort
      before = entry.text.substr(0, cur.start - 1)
      after = entry.text.substr cur.stop
    entry.text = before & after
    entry.cursor = Cursor(kind: CursorInsert, pos: cur.start)

func update*(cursor: var Cursor, dir, max_pos: int, select: bool) =
  case cursor.kind:
    of CursorInsert:
      let p = (cursor.pos + dir).max(0).min(max_pos)
      if select:
        cursor = Cursor(kind: CursorSelection, start: cursor.pos, stop: p)
      else:
        cursor.pos = p
    of CursorSelection:
      if select:
        cursor.stop = (cursor.stop + dir).clamp(0, max_pos)
      else:
        let cur = cursor.sort
        if dir < 0:
          cursor = Cursor(kind: CursorInsert, pos: cur.start)
        else:
          cursor = Cursor(kind: CursorInsert, pos: cur.stop)

func skip(text: seq[Rune], pos, dir: int): int =
  result = 0
  if text.len > 0:
    if pos < 0:
      result = -pos
    elif pos >= text.len:
      result = -(pos - text.len + 1)
    let v = text[pos + result].is_alpha_numeric
    while pos + result >= 0 and
          pos + result < text.len and
          text[pos + result].is_alpha_numeric == v:
      result += dir

func process_mouse*(entry: var Entry, mouse: Mouse) =
  var pos = mouse.pos.x.max(0).min(entry.text.len)

  case mouse.kind:
    of MouseDown:
      if mouse.button == 0: entry.cursor = Cursor(kind: CursorInsert, pos: pos)
    of MouseMove, MouseUp:
      if (mouse.kind == MouseMove and mouse.buttons[0]) or (mouse.kind ==
          MouseUp and mouse.button == 0):
        case entry.cursor.kind:
          of CursorInsert:
            if pos != entry.cursor.pos:
              entry.cursor = Cursor(kind: CursorSelection,
                  start: entry.cursor.pos, stop: pos)
          of CursorSelection: entry.cursor.stop = pos
    else: discard

func arrow_left_key(entry: var Entry, ctrl, shift: bool) =
  let offset =
    if ctrl:
      entry.text.skip(entry.cursor.get_pos - 1, -1)
    else: -1
  entry.cursor.update offset, entry.text.len, shift

func arrow_right_key(entry: var Entry, ctrl, shift: bool) =
  let offset =
    if ctrl:
      entry.text.skip entry.cursor.get_pos, 1
    else: 1
  entry.cursor.update offset, entry.text.len, shift

func backspace_key(entry: var Entry) =
  case entry.cursor.kind:
    of CursorInsert:
      if entry.cursor.pos > 0:
        entry.text = entry.text.substr(0, entry.cursor.pos - 2) &
            entry.text.substr(entry.cursor.pos)
        entry.cursor.pos -= 1
    of CursorSelection: entry.delete_selected

func delete_key(entry: var Entry) =
  case entry.cursor.kind:
    of CursorInsert:
      if entry.cursor.pos < entry.text.len:
        entry.text = entry.text.substr(0, entry.cursor.pos - 1) &
            entry.text.substr(entry.cursor.pos + 1)
    of CursorSelection: entry.delete_selected

func ctrl_a_key(entry: var Entry) =
  entry.cursor = Cursor(kind: CursorSelection, start: 0, stop: entry.text.len)

func ctrl_v_key(entry: var Entry) =
  entry.delete_selected
  if entry.copy_buffer != nil:
    let before = entry.text.substr(0, entry.cursor.pos - 1)
    let after = entry.text.substr entry.cursor.pos
    let paste = entry.copy_buffer.paste
    entry.text = before & paste & after
    entry.cursor.pos += paste.len

func ctrl_c_key(entry: var Entry) =
  if entry.cursor.kind == CursorSelection:
    let cur = entry.cursor.sort
    entry.copy_buffer.copy entry.text.substr(cur.start, cur.stop - 1)

func ctrl_x_key(entry: var Entry) =
  if entry.cursor.kind == CursorSelection:
    let cur = entry.cursor.sort
    entry.copy_buffer.copy entry.text.substr(cur.start, cur.stop - 1)
    entry.delete_selected

func char_key(entry: var Entry, rune: Rune) =
  entry.delete_selected
  let before = entry.text.substr(0, entry.cursor.pos - 1)
  let after = entry.text.substr entry.cursor.pos
  entry.text = before & rune & after
  entry.cursor.pos += 1

func process_key*(entry: var Entry, key: Key) =
  case key.kind:
    of KeyArrowLeft: entry.arrow_left_key key.ctrl, key.shift
    of KeyArrowRight: entry.arrow_right_key key.ctrl, key.shift
    of KeyBackspace: entry.backspace_key
    of KeyDelete: entry.delete_key
    of KeyChar:
      if key.ctrl:
        case key.chr:
          of Rune('a'): entry.ctrl_a_key
          of Rune('v'): entry.ctrl_v_key
          of Rune('c'): entry.ctrl_c_key
          of Rune('x'): entry.ctrl_x_key
          else: discard
      else: entry.char_key key.chr
    else: discard

func render*(entry: Entry, ren: var TermRenderer) =
  case entry.cursor.kind:
    of CursorInsert:
      ren.put entry.text.substr(0, entry.cursor.pos - 1)
      if entry.cursor.pos < entry.text.len:
        ren.put entry.text[entry.cursor.pos], reverse = true
        ren.put entry.text.substr(entry.cursor.pos + 1)
      else:
        ren.put " ", reverse = true
    of CursorSelection:
      let cur = entry.cursor.sort
      ren.put entry.text.substr(0, cur.start - 1)
      ren.put entry.text.substr(cur.start, cur.stop - 1), reverse = true
      ren.put entry.text.substr(cur.stop), reverse = true

func make_entry*(copy_buffer: CopyBuffer = nil): owned Entry =
  Entry(text: @[], cursor: Cursor(kind: CursorInsert, pos: 0),
      copy_buffer: copy_buffer)

func make_entry*(text: seq[Rune], copy_buffer: CopyBuffer = nil): owned Entry =
  Entry(text: text, cursor: Cursor(kind: CursorInsert, pos: 0),
      copy_buffer: copy_buffer)

func render_border*(title: string, sidebar_width: int, box: Box,
    ren: var TermRenderer) =
  var shown_title = title
  let padding_len = box.size.x - sidebar_width - 1 - title.len
  if padding_len < 0:
    shown_title = shown_title.substr(0, shown_title.len - 1 + padding_len)
  let titlebar = strutils.repeat(' ', sidebar_width + 1) & shown_title &
      strutils.repeat(' ', padding_len.max(0))
  ren.move_to box.min
  ren.put titlebar

  for y in 1..<box.size.y:
    ren.move_to box.min.x, (box.min.y + y)
    ren.put repeat(' ', sidebar_width)

type List* = object
  items*: seq[seq[Rune]]
  selected*: int
  view*: int
  height*: int
  detached*: bool

func make_list*(items: seq[seq[Rune]]): List =
  List(items: items, view: 0, selected: 0)

func make_list*(items: seq[string]): List =
  makeList(items.map to_runes)

func make_list*(): List = List(view: 0, selected: 0)

func process_mouse*(list: var List, mouse: Mouse): bool =
  case mouse.kind:
    of MouseScroll:
      list.detached = true
      list.view += mouse.delta * 2
    of MouseDown, MouseMove, MouseUp:
      if mouse.buttons[0] or (mouse.kind == MouseUp and mouse.button == 0):
        let selected = mouse.y + list.view
        if selected >= 0 and selected < list.items.len:
          list.selected = selected
          return true
    else: discard

const LIST_KEYS* = {KeyArrowUp, KeyArrowDown, KeyHome, KeyEnd, KeyPageDown, KeyPageUp}

func process_key*(list: var List, key: Key) =
  list.detached = false
  case key.kind:
    of KeyArrowUp: list.selected -= 1
    of KeyArrowDown: list.selected += 1
    of KeyHome: list.selected = 0
    of KeyEnd: list.selected = list.items.len - 1
    of KeyPageDown: list.selected += list.height
    of KeyPageUp: list.selected -= list.height
    else: discard

  list.selected = list.selected.max(0).min(list.items.len - 1)

func scroll(list: var List) =
  if not list.detached:
    while list.selected - list.view >= list.height - 3:
      list.view += 1

    while list.selected - list.view < 3:
      list.view -= 1

  list.view = list.view.clamp(0, list.items.len - 1)

func render*(list: var List, box: Box, ren: var TermRenderer) =
  let prev_clip = ren.clip_area
  ren.clip box
  list.height = box.size.y
  list.scroll
  for it, item in list.items:
    ren.move_to box.min.x, (box.min.y + it - list.view)
    ren.put(item, reverse = it == list.selected)
  ren.clip prev_clip
