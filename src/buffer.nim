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
import strutils, unicode, tables, utils

type
  ActionKind* = enum ActionDelete, ActionInsert

  Action* = object
    case kind*: ActionKind:
      of ActionDelete:
        delete_pos*: int
        delete_text*: seq[Rune]
      of ActionInsert:
        insert_pos*: int
        insert_text*: seq[Rune]

  IndentStyle* = enum IndentSpaces, IndentTab

  NewlineStyle* = enum NewlineLf, NewlineCrLf

  CursorHook* = proc(start: int, delta: int) {.closure.}

  Buffer* = ref object
    file_path*: string
    text*: seq[Rune]
    lines*: seq[int]
    changed*: bool
    undo_stack: seq[seq[Action]]
    redo_stack: seq[seq[Action]]
    newline_style*: NewlineStyle

func len*(buffer: Buffer): int = buffer.text.len

func `[]`*(buffer: Buffer, index: int): Rune = buffer.text[index]

func index_lines*(text: seq[Rune]): seq[int] =
  result.add 0
  for it, chr in text.pairs:
    if chr == '\n':
      result.add it + 1

func reindex_lines*(buffer: Buffer) =
  buffer.lines = buffer.text.index_lines

func update_line_indices*(buffer: Buffer, start: int, delta: int) =
  var it = buffer.lines.len - 1
  while it >= 0:
    if buffer.lines[it] >= start:
      buffer.lines[it] += delta
      it -= 1
    else: break

func to_2d*(buffer: Buffer, index: int): Index2d =
  for it, line_index in buffer.lines:
    if line_index > index:
      return result
    else:
      result = Index2d(x: index - line_index, y: it)

func to_display_2d*(buffer: Buffer, index: int): Index2d =
  let pos = buffer.to_2d index
  var cur = buffer.lines[pos.y]
  var x = 0
  while cur < index and cur < buffer.text.len:
    case buffer.text[cur]:
    of '\t':
      x += 2
    else:
      x += 1
    cur += 1
  result = Index2d(x: x, y: pos.y)

func to_index*(buffer: Buffer, pos: Index2d): int =
  result = pos.x + buffer.lines[pos.y]
  if pos.y + 1 < buffer.lines.len:
    result = result.min(buffer.lines[pos.y + 1] - 1)
  else:
    result = result.min(buffer.text.len)

func display_to_index*(buffer: Buffer, pos: Index2d): int =
  result = buffer.lines[pos.y]
  var cur_x = pos.x
  while cur_x > 0 and result < buffer.text.len:
    let chr = buffer.text[result]
    case chr:
      of '\t': cur_x -= 2
      of '\n': break
      else: cur_x -= 1
    result += 1
  if cur_x < 0:
    result -= 1

func set_path*(buffer: Buffer, path: string) =
  buffer.file_path = path

func file_name*(buffer: Buffer): string =
  let dirs = buffer.file_path.split "/"
  if dirs.len > 0:
    result = dirs[dirs.len - 1]

func `$`*(newline_style: NewlineStyle): string =
  case newline_style:
    of NewlineLf: result = "Lf"
    of NewlineCrLf: result = "CrLf"

func display_file_name*(buffer: Buffer): string =
  if buffer.file_name == "":
    result = "*Untitled file*"
  else:
    result = buffer.file_name
    if buffer.changed: result &= "*"
  result &= " (Mlatu, "
  result &= $buffer.newline_style
  result &= ")"

proc save*(buffer: Buffer) {.tags: [WriteIOEffect].} =
  buffer.file_path.write_file $buffer.text
  buffer.changed = false

func slice*(buffer: Buffer, start, stop: int): seq[Rune] =
  buffer.text.substr(start, stop - 1)

func delete_no_undo(buffer: Buffer, start, stop: int) =
  buffer.text = buffer.text.substr(0, start - 1) & buffer.text.substr(stop)
  buffer.reindex_lines
  buffer.changed = true

func undo_frame(buffer: Buffer): var seq[Action] =
  if buffer.undo_stack.len == 0:
    buffer.undo_stack.add @[]
  result = buffer.undo_stack[buffer.undo_stack.len - 1]

func delete*(buffer: Buffer, start, stop: int) =
  let text = buffer.slice(start, stop)
  buffer.delete_no_undo start, stop
  buffer.undo_frame.add Action(kind: ActionDelete, delete_pos: start,
      delete_text: text)
  buffer.redo_stack = @[]

func insert_no_undo(buffer: Buffer, pos: int, chr: Rune) =
  let before = buffer.text.substr(0, pos - 1)
  let after = buffer.text.substr pos
  buffer.text = before & chr & after
  buffer.update_line_indices(pos + 1, 1)
  buffer.changed = true

func insert_no_undo(buffer: Buffer, pos: int, str: seq[Rune]) =
  let before = buffer.text.substr(0, pos - 1)
  let after = buffer.text.substr pos
  buffer.text = before & str & after
  buffer.reindex_lines
  buffer.changed = true

func insert*(buffer: Buffer, pos: int, chr: Rune) =
  buffer.insert_no_undo pos, chr
  buffer.undo_frame.add Action(kind: ActionInsert, insert_pos: pos,
      insert_text: @[chr])
  buffer.redo_stack = @[]

func insert*(buffer: Buffer, pos: int, str: seq[Rune]) =
  buffer.insert_no_undo pos, str
  buffer.undo_frame.add Action(kind: ActionInsert, insert_pos: pos,
      insert_text: str)
  buffer.redo_stack = @[]

func replace*(buffer: Buffer, start, stop: int, text: seq[Rune]) =
  let deleted_text = buffer.text.substr(start, stop - 1)
  let before = buffer.text.substr(0, start - 1)
  let after = buffer.text.substr stop
  buffer.text = before & text & after
  buffer.reindex_lines
  buffer.changed = true

  buffer.undo_frame.add Action(kind: ActionDelete, delete_pos: start,
      delete_text: deleted_text)
  buffer.undo_frame.add Action(kind: ActionInsert, insert_pos: start,
      insert_text: text)

func insert_newline*(buffer: Buffer, pos: int) =
  let before = buffer.text.substr(0, pos - 1)
  let after = buffer.text.substr pos
  buffer.text = before & '\n' & after
  buffer.reindex_lines
  buffer.changed = true

  buffer.undo_frame.add Action(kind: ActionInsert, insert_pos: pos,
      insert_text: @[Rune('\n')])
  buffer.redo_stack = @[]

func line_range*(buffer: Buffer, line: int): (int, int) =
  if line < 0 or line >= buffer.lines.len:
    return (-1, -1)
  var it = buffer.lines[line]
  while it < buffer.text.len:
    if buffer.text[it] == '\n':
      it += 1
      break
    it += 1
  result = (buffer.lines[line], it)

func skip*(buffer: Buffer, pos: int, dir: int): int =
  if buffer.text.len > 0:
    result = pos.clamp(0, buffer.text.len - 1)
    let v = buffer.text[result].is_alpha_numeric
    while result >= 0 and result < buffer.text.len and buffer.text[
        result].is_alpha_numeric == v:
      result += dir

func word_range*(buffer: Buffer, pos: int): (int, int) =
  (buffer.skip(pos, -1) + 1, buffer.skip(pos, 1))

func pop_non_empty[T](stack: var seq[seq[T]]): seq[T] =
  while result.len == 0:
    result = stack.pop

func redo*(buffer: Buffer) =
  if buffer.redo_stack.len > 0:
    let frame = buffer.redo_stack.pop_non_empty
    buffer.undo_stack.add frame
    for action in frame:
      case action.kind:
        of ActionInsert:
          buffer.insert_no_undo(action.insert_pos, action.insert_text)
        of ActionDelete:
          buffer.delete_no_undo(action.delete_pos, action.delete_pos +
              action.delete_text.len)

func undo*(buffer: Buffer) =
  if buffer.undo_stack.len > 0:
    let frame = buffer.undo_stack.pop_non_empty
    buffer.redo_stack.add frame
    for it in countdown(frame.len - 1, 0):
      let action = frame[it]
      case action.kind:
        of ActionDelete:
          buffer.insert_no_undo(action.delete_pos, action.delete_text)
        of ActionInsert:
          buffer.delete_no_undo(action.insert_pos, action.insert_pos +
              action.insert_text.len)

func finish_undo_frame*(buffer: Buffer) =
  if buffer.undo_stack.len > 0 and buffer.undo_stack[buffer.undo_stack.len -
      1].len > 0:
    buffer.undo_stack.add @[]

func to_runes*(newline_style: NewlineStyle): seq[Rune] =
  case newline_style:
    of NewlineLf: result = @[Rune('\n')]
    of NewlineCrLf: result = @[Rune('\r'), Rune('\n')]

func guess_newline_style(text: seq[Rune]): NewlineStyle =
  var is_cr = false
  var scores: array[NewlineStyle, int]
  for it, chr in text:
    case chr:
      of '\r':
        is_cr = true
        continue
      of '\n':
        if is_cr:
          scores[NewlineCrLf].inc
        else:
          scores[NewlineLf].inc
      else: discard
  var max_score = 0
  result = NewlineLf
  for style, score in scores.pairs:
    if score > max_score:
      result = style
      max_score = score

func make_buffer*(): Buffer =
  return Buffer(
    file_path: "",
    text: @[],
    lines: @[0],
    changed: false,
    newline_style: NewlineLf
  )

proc make_buffer*(path: string): Buffer {.tags: [ReadIOEffect].} =
  let text = path.read_file.to_runes
  let newline_style = text.guess_newline_style
  return Buffer(
    file_path: path,
    text: text,
    lines: text.index_lines,
    changed: false,
    newline_style: newline_style
  )
