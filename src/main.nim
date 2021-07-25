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
import termdiff, window_manager, repl, std/exitprocs

when isMainModule:
  setup_term()
  add_exit_proc quit_app
  var cur_screen = make_term_screen()
  var window_constructors = @[
    make_window_constructor("REPL", make_repl)
  ]
  var app = window_constructors.make_app
  var root_pane = Pane(kind: PaneWindow, window: app.make_repl)

  app.root_pane = root_pane
  block:
    var ren = cur_screen.make_term_renderer
    app.render ren
    cur_screen.show_all
  while true:
    let key = read_key()
    if key.kind == KeyMouse:
      app.process_mouse read_mouse()
    else:
      if app.process_key key:
        quit_app()
        break
    var screen = make_term_screen()
    var ren = screen.make_term_renderer
    app.render ren
    cur_screen.apply screen
    cur_screen = screen
