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
import strutils, unicode, window_manager, termdiff, utils, ui_utils, mlatu

type
  Input = object
    entry: Entry
    output: string
    err: bool
    out_state: EvalState

  Repl* = ref object of Window
    app: App
    inputs: seq[Input]
    selected: int
    scroll: Index2d
    in_state: EvalState

func make_input(app: App): Input =
  return Input(entry: make_entry(app.copy_buffer))

func make_input(repl: Repl): Input = repl.app.make_input

func max_input_number_width(repl: Repl): int =
  ($repl.inputs.len).len + 2

method process_mouse*(repl: Repl, mouse: Mouse): bool {.locks: "unknown".}=
  if mouse.y == 0 and mouse.x < repl.max_input_number_width:
    return true

  if (mouse.y - 1) mod 3 == 0:
    if mouse.kind == MouseDown:
      repl.selected = (mouse.y - 1) div 3
      repl.selected = max(min(repl.selected, repl.inputs.len - 1), 0)
    var mouse_rel = mouse
    mouse_rel.x -= repl.max_input_number_width + 1
    mouse_rel.y = 0
    repl.inputs[repl.selected].entry.process_mouse(mouse_rel)

func get_in_state(repl: Repl, index: int): EvalState {.raises: [], tags: [].} =
  if index > 0:
    repl.inputs[index - 1].out_state
  else:
    repl.in_state

func update_input(repl: Repl, index: int, in_state: EvalState) {.raises: [], tags: [].} =
  let terms = ($repl.inputs[index].entry.text).parse
  try:
    repl.inputs[index].out_state = in_state
    var stack: Stack = new_stack()
    stack.eval(repl.inputs[index].out_state, terms)
    repl.inputs[index].output = stack.display_stack
    repl.inputs[index].err = false
    if (index + 1) < repl.inputs.len:
      repl.update_input(index + 1, repl.inputs[index].out_state)
  except EvalError as e:
    repl.inputs[index].output = e.msg
    repl.inputs[index].err = true

method process_key*(repl: Repl, key: Key) {.locks: "unknown", tags: [].} =
  case key.kind:
    of KeyReturn:
      repl.inputs.insert(repl.make_input, repl.selected + 1)
      repl.selected += 1
    of KeyArrowDown:
      repl.selected += 1
      if repl.selected >= repl.inputs.len:
        repl.selected = repl.inputs.len - 1
    of KeyArrowUp:
      repl.selected -= 1
      if repl.selected < 0:
        repl.selected = 0
    else:
      repl.inputs[repl.selected].entry.process_key key
      repl.update_input repl.selected, repl.get_in_state repl.selected

func input_number(repl: Repl, n: int): string =
  return "[" & strutils.align($n, ($repl.inputs.len).len) & "]"

method render*(repl: Repl, box: Box, ren: var TermRenderer) =
  let title = repeat(' ', repl.max_input_number_width + 1) &
            unicode.align_left("REPL", box.size.x - repl.max_input_number_width - 1)
  ren.move_to(box.min)
  ren.put title, bright_white(), black()

  for y in 0..<(box.size.y - 1):
    ren.move_to(box.min.x, box.min.y + 1 + y)
    ren.put repeat(' ', repl.max_input_number_width()), bright_white(), black()
  for it, input in repl.inputs:
    ren.move_to(box.min.x, box.min.y + it * 3 + 1)
    ren.put repl.input_number(it), bright_white(), black()
    ren.move_to(box.min.x + repl.max_input_number_width + 1, box.min.y + it * 3 + 1)
    if it == repl.selected:
      input.entry.render(ren)
    else:
      ren.put input.entry.text, bright_black(), bright_white()

    ren.move_to(box.min.x + repl.max_input_number_width + 1, box.min.y + it * 3 + 2)

    let fg = 
      if input.err: red()
      elif it == repl.selected: bright_blue()
      else: Color(base: ColorMagenta, bright: true) 

    ren.put input.output, fg

func make_repl*(app: App): Window =
  return Repl(app: app, inputs: @[app.make_input], in_state: init_state())
