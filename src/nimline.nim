

import
  critbits,
  terminal,
  deques,
  sequtils,
  strutils,
  std/exitprocs,
  os

addExitProc(resetAttributes)

when defined(windows):
  proc putchr*(c: cint): cint {.discardable, header: "<conio.h>",
      importc: "_putch".}
    ## Prints an ASCII character to stdout.
  proc getchr*(): cint {.header: "<conio.h>", importc: "_getch".}
    ## Retrieves an ASCII character from stdin.
else:
  proc putchr*(c: cint) {.header: "stdio.h", importc: "putchar".} =
    ## Prints an ASCII character to stdout.
    stdout.write(c.chr)
    stdout.flushFile()

  proc getchr*(): cint =
    ## Retrieves an ASCII character from stdin.
    stdout.flushFile()
    return getch().ord.cint

# Types

type
  Key* = int ## The ASCII code of a keyboard key.
  KeySeq* = seq[Key] ## A sequence of one or more Keys.
  KeyCallback* = proc(ed: var LineEditor) {.closure, gcsafe, raises: [IOError,
      ValueError], tags: [
      WriteIOEffect].} ## A proc that can be bound to a key or a key sequence to access line editing functionalities.
  LineEditorMode* = enum ## The *mode* a LineEditor operates in (insert or replace).
    mdInsert
    mdReplace
  Line* = object ## An object representing a line of text.
    text: string
    position: int
  LineHistory* = object ## An object representing the history of all commands typed in a LineEditor.
    file: string
    tainted: bool
    position: int
    queue: Deque[string]
    max: int
  LineEditor* = object ## An object representing a line editor, used to process text typed in the terminal.
    completionCallback*: proc(ed: LineEditor): seq[string]
      {.closure, gcsafe, raises: [IOError, ValueError], tags: [].}
    history: LineHistory
    line: Line
    mode: LineEditorMode

# Internal Methods

func empty(line: Line): bool {.raises: [], tags: [].} =
  return line.text.len <= 0

func full(line: Line): bool {.raises: [], tags: [].} =
  return line.position >= line.text.len

func fromStart(line: Line): string {.raises: [], tags: [].} =
  if line.empty:
    return ""
  return line.text[0..line.position-1]

func toEnd(line: Line): string {.raises: [], tags: [].} =
  if line.empty:
    return ""
  return line.text[line.position..line.text.len-1]

proc back*(ed: var LineEditor, n = 1)
  {.raises: [IOError, ValueError], tags: [WriteIOEffect].} =
  ## Move the cursor back by **n** characters on the current line (unless the beginning of the line is reached).
  if ed.line.position <= 0:
    return
  stdout.cursorBackward(n)
  ed.line.position = ed.line.position - n

proc forward*(ed: var LineEditor, n = 1)
  {.raises: [IOError, ValueError], tags: [WriteIOEffect].} =
  ## Move the cursor forward by **n** characters on the current line (unless the beginning of the line is reached).
  if ed.line.full:
    return
  stdout.cursorForward(n)
  ed.line.position += n

func `[]`(q: Deque[string], pos: int): string {.raises: [], tags: [].} =
  var c = 0
  for e in q.items:
    if c == pos:
      result = e
      break
    c.inc

func `[]=`(q: var Deque[string], pos: int, s: string) {.raises: [], tags: [].} =
  var c = 0
  for e in q.mitems:
    if c == pos:
      e = s
      break
    c.inc

func add(h: var LineHistory, s: string, force = false) {.raises: [], tags: [].} =
  if s == "" and not force:
    return
  if h.queue.len >= h.max:
    discard h.queue.popFirst
  if h.tainted:
    h.queue[h.queue.len-1] = s
  else:
    h.queue.addLast s

func previous(h: var LineHistory): string {.raises: [], tags: [].} =
  if h.queue.len == 0 or h.position <= 0:
    return ""
  h.position.dec
  result = h.queue[h.position]

func next(h: var LineHistory): string {.raises: [], tags: [].} =
  if h.queue.len == 0 or h.position >= h.queue.len-1:
    return ""
  h.position.inc
  result = h.queue[h.position]

# Public API

proc deletePrevious*(ed: var LineEditor)
  {.raises: [IOError, ValueError], tags: [WriteIOEffect].} =
  ## Move the cursor to the left by one character (unless at the beginning of the line) and delete the existing character, if any.
  if ed.line.position <= 0:
    return
  if not ed.line.empty:
    if ed.line.full:
      stdout.cursorBackward
      putchr(32)
      stdout.cursorBackward
      ed.line.position.dec
      ed.line.text = ed.line.text[0..ed.line.text.len-2]
    else:
      let rest = ed.line.toEnd & " "
      ed.back
      for i in rest:
        putchr i.ord.cint
      ed.line.text =
        ed.line.fromStart & ed.line.text[ed.line.position+1..ed.line.text.len-2]
      stdout.cursorBackward(rest.len)

proc deleteNext*(ed: var LineEditor)
  {.raises: [IOError, ValueError], tags: [WriteIOEffect].} =
  ## Move the cursor to the right by one character (unless at the end of the line) and delete the existing character, if any.
  if not ed.line.empty:
    if not ed.line.full:
      let rest = ed.line.toEnd[1..^1] & " "
      for c in rest:
        putchr c.ord.cint
      stdout.cursorBackward(rest.len)
      ed.line.text = ed.line.fromStart & ed.line.toEnd[1..^1]

proc printChar*(ed: var LineEditor, c: int)
  {.raises: [IOError, ValueError], tags: [WriteIOEffect].} =
  ## Prints the character **c** to the current line. If in the middle of the line, the following characters are shifted right or replaced depending on the editor mode.
  if ed.line.full:
    putchr(c.cint)
    ed.line.text &= c.chr
    ed.line.position += 1
  else:
    if ed.mode == mdInsert:
      putchr(c.cint)
      let rest = ed.line.toEnd
      ed.line.text.insert($c.chr, ed.line.position)
      ed.line.position += 1
      for j in rest:
        putchr(j.ord.cint)
        ed.line.position += 1
      ed.back(rest.len)
    else:
      putchr(c.cint)
      ed.line.text[ed.line.position] = c.chr
      ed.line.position += 1

proc changeLine*(ed: var LineEditor, s: string)
  {.raises: [IOError, ValueError], tags: [WriteIOEffect].} =
  ## Replaces the contents of the current line with the string **s**.
  let text = ed.line.text
  let diff = text.len - s.len
  let position = ed.line.position
  if position > 0:
    stdout.cursorBackward(position)
  for c in s:
    putchr(c.ord.cint)
  ed.line.position = s.len
  ed.line.text = s
  if diff > 0:
    for i in 0.countup(diff-1):
      putchr(32)
    stdout.cursorBackward(diff)

proc addToLineAtPosition(ed: var LineEditor, s: string)
  {.raises: [IOError, ValueError], tags: [WriteIOEffect].} =
  for c in s:
    ed.printChar(c.ord.cint)

proc clearLine*(ed: var LineEditor)
  {.raises: [IOError, ValueError], tags: [WriteIOEffect].} =
  ## Clears the contents of the current line and reset the cursor position to the beginning of the line.
  stdout.cursorBackward(ed.line.position+1)
  for i in ed.line.text:
    putchr(32)
  putchr(32)
  putchr(32)
  stdout.cursorBackward(ed.line.text.len+1)
  ed.line.position = 0
  ed.line.text = ""

proc goToStart*(ed: var LineEditor)
  {.raises: [], tags: [WriteIOEffect].} =
  ## Move the cursor to the beginning of the line.
  if ed.line.position <= 0:
    return
  try:
    stdout.cursorBackward(ed.line.position)
    ed.line.position = 0
  except:
    discard

proc goToEnd*(ed: var LineEditor)
  {.raises: [ValueError, IOError], tags: [WriteIOEffect].} =
  ## Move the cursor to the end of the line.
  if ed.line.full:
    return
  let diff = ed.line.text.len - ed.line.position
  stdout.cursorForward(diff)
  ed.line.position = ed.line.text.len

proc historyInit*(size = 256, file: string = ""): LineHistory
  {.raises: [IOError], tags: [WriteIOEffect, ReadDirEffect, ReadIOEffect].} =
  ## Creates a new **LineHistory** object with the specified **size** and **file**.
  result.file = file
  result.queue = initDeque[string](size)
  result.position = 0
  result.tainted = false
  result.max = size
  if file == "":
    return
  if result.file.fileExists:
    let lines = result.file.readFile.split("\n")
    for line in lines:
      if line != "":
        result.add line
    result.position = lines.len
  else:
    result.file.writeFile("")

proc historyAdd*(ed: var LineEditor, force = false)
  {.raises: [IOError], tags: [WriteIOEffect].} =
  ## Adds the current editor line to the history. If **force** is set to **true**, the line will be added even if it's blank.
  ed.history.add ed.line.text, force
  if ed.history.file == "":
    return
  ed.history.file.writeFile(toSeq(ed.history.queue.items).join("\n"))

proc historyPrevious*(ed: var LineEditor)
  {.raises: [IOError, ValueError], tags: [WriteIOEffect].} =
  ## Replaces the contents of the current line with the previous line stored in the history (if any).
  ## The current line will be added to the history and the hisory will be marked as *tainted*.
  let s = ed.history.previous
  if s == "":
    return
  let pos = ed.history.position
  var current: int
  if ed.history.tainted:
    current = ed.history.queue.len-2
  else:
    current = ed.history.queue.len-1
  if pos == current and ed.history.queue[current] != ed.line.text:
    ed.historyAdd(force = true)
    ed.history.tainted = true
  if s != "":
    ed.changeLine(s)

proc historyNext*(ed: var LineEditor)
  {.raises: [IOError, ValueError], tags: [WriteIOEffect].} =
  ## Replaces the contents of the current line with the following line stored in the history (if any).
  let s = ed.history.next
  if s == "":
    return
  ed.changeLine(s)

func historyFlush*(ed: var LineEditor)
  {.raises: [], tags: [WriteIOEffect].} =
  ## If there is at least one entry in the history, it sets the position of the cursor to the last element and sets the **tainted** flag to **false**.
  if ed.history.queue.len > 0:
    ed.history.position = ed.history.queue.len
    ed.history.tainted = false

proc completeLine*(ed: var LineEditor): int
  {.raises: [IOError, ValueError], tags: [WriteIOEffect, ReadIOEffect].} =
  ## If a **completionCallback** proc has been specified for the current editor, attempts to auto-complete the current line by running **completionProc**
  ## to return a list of possible values. It is possible to cycle through the matches by pressing the same key that triggered this proc.
  ##
  ## The matches provided will be filtered based on the contents of the line when this proc was first triggered. If a match starts with the contents of the line, it
  ## will be displayed.
  ##
  ## The following is a real-world example of a **completionCallback** used to complete the last word on the line with valid file paths.
  ##
  ## .. code-block:: nim
  ##   import sequtils, strutils, ospath
  ##
  ##   ed.completionCallback = proc(ed: LineEditor): seq[string] =
  ##     var words = ed.lineText.split(" ")
  ##     var word: string
  ##     if words.len == 0:
  ##       word = ed.lineText
  ##     else:
  ##       word = words[words.len-1]
  ##     var f = word[1..^1]
  ##     if f == "":
  ##       f = getCurrentDir().replace("\\", "/")
  ##       return toSeq(walkDir(f, true))
  ##         .mapIt("\"$1" % it.path.replace("\\", "/"))
  ##     elif f.dirExists:
  ##       f = f.replace("\\", "/")
  ##       if f[f.len-1] != '/':
  ##         f = f & "/"
  ##       return toSeq(walkDir(f, true))
  ##         .mapIt("\"$1$2" % [f, it.path.replace("\\", "/")])
  ##     else:
  ##       var dir: string
  ##       if f.contains("/") or dir.contains("\\"):
  ##         dir = f.parentDir
  ##         let file = f.extractFileName
  ##         return toSeq(walkDir(dir, true))
  ##           .filterIt(it.path.toLowerAscii.startsWith(file.toLowerAscii))
  ##           .mapIt("\"$1/$2" % [dir, it.path.replace("\\", "/")])
  ##       else:
  ##         dir = getCurrentDir()
  ##         return toSeq(walkDir(dir, true))
  ##           .filterIt(it.path.toLowerAscii.startsWith(f.toLowerAscii))
  ##           .mapIt("\"$1" % [it.path.replace("\\", "/")])
  ##
  if ed.completionCallback.isNil:
    return
  let compl = ed.completionCallback(ed)
  let position = ed.line.position
  let words = ed.line.fromStart.split(" ")
  var word: string
  if words.len > 0:
    word = words[words.len-1]
  else:
    word = ed.line.fromStart
  var matches = compl.filterIt(it.toLowerAscii.startsWith(word.toLowerAscii))
  if ed.line.fromStart.len > 0 and matches.len > 0:
    for i in 0..word.len-1:
      ed.deletePrevious
  var n = 0
  if matches.len > 0:
    ed.addToLineAtPosition(matches[0])
  else:
    return -1
  var ch = getchr()
  while ch == 9:
    n.inc
    if n < matches.len:
      let diff = ed.line.position - position
      for i in 0.countup(diff-1 + word.len):
        ed.deletePrevious
      ed.addToLineAtPosition(matches[n])
      ch = getchr()
    else:
      n = -1
  return ch

func lineText*(ed: LineEditor): string {.raises: [].} =
  ## Returns the contents of the current line.
  return ed.line.text

proc initEditor*(mode = mdInsert, historySize = 256,
    historyFile: string = ""): LineEditor
    {.raises: [IOError], tags: [WriteIOEffect, ReadDirEffect, ReadIOEffect].} =
  ## Creates a **LineEditor** object.
  result.mode = mode
  result.history = historyInit(historySize, historyFile)

# Character sets
const
  CTRL* = {0 .. 31}          ## Control characters.
  DIGIT* = {48 .. 57}        ## Digits.
  LETTER* = {65 .. 122}      ## Letters.
  UPPERLETTER* = {65 .. 90}  ## Uppercase letters.
  LOWERLETTER* = {97 .. 122} ## Lowercase letters.
  PRINTABLE* = {32 .. 126}   ## Printable characters.
when defined(windows):
  const
    ESCAPES* = {0, 22, 224} ## Escape characters.
else:
  const
    ESCAPES* = {27} ## Escape characters.

# Key Names
var keynames* {.threadvar.}: array[0..31,
    string] ## The following strings can be used in keymaps instead of the correspinding ASCII codes:
 ##
 ## .. code-block:: nim
 ##    keynames[1]    =    "ctrl+a"
 ##    keynames[2]    =    "ctrl+b"
 ##    keynames[3]    =    "ctrl+c"
 ##    keynames[4]    =    "ctrl+d"
 ##    keynames[5]    =    "ctrl+e"
 ##    keynames[6]    =    "ctrl+f"
 ##    keynames[7]    =    "ctrl+g"
 ##    keynames[8]    =    "ctrl+h"
 ##    keynames[9]    =    "ctrl+i"
 ##    keynames[9]    =    "tab"
 ##    keynames[10]   =    "ctrl+j"
 ##    keynames[11]   =    "ctrl+k"
 ##    keynames[12]   =    "ctrl+l"
 ##    keynames[13]   =    "ctrl+m"
 ##    keynames[14]   =    "ctrl+n"
 ##    keynames[15]   =    "ctrl+o"
 ##    keynames[16]   =    "ctrl+p"
 ##    keynames[17]   =    "ctrl+q"
 ##    keynames[18]   =    "ctrl+r"
 ##    keynames[19]   =    "ctrl+s"
 ##    keynames[20]   =    "ctrl+t"
 ##    keynames[21]   =    "ctrl+u"
 ##    keynames[22]   =    "ctrl+v"
 ##    keynames[23]   =    "ctrl+w"
 ##    keynames[24]   =    "ctrl+x"
 ##    keynames[25]   =    "ctrl+y"
 ##    keynames[26]   =    "ctrl+z"

keynames[1] = "ctrl+a"
keynames[2] = "ctrl+b"
keynames[3] = "ctrl+c"
keynames[4] = "ctrl+d"
keynames[5] = "ctrl+e"
keynames[6] = "ctrl+f"
keynames[7] = "ctrl+g"
keynames[8] = "ctrl+h"
keynames[9] = "ctrl+i"
keynames[9] = "tab"
keynames[10] = "ctrl+j"
keynames[11] = "ctrl+k"
keynames[12] = "ctrl+l"
keynames[13] = "ctrl+m"
keynames[14] = "ctrl+n"
keynames[15] = "ctrl+o"
keynames[16] = "ctrl+p"
keynames[17] = "ctrl+q"
keynames[18] = "ctrl+r"
keynames[19] = "ctrl+s"
keynames[20] = "ctrl+t"
keynames[21] = "ctrl+u"
keynames[22] = "ctrl+v"
keynames[23] = "ctrl+w"
keynames[24] = "ctrl+x"
keynames[25] = "ctrl+y"
keynames[26] = "ctrl+z"

# Key Sequences
var keyseqs* {.threadvar.}: CritBitTree[KeySeq]
  ## The following key sequences are defined and are used internally by **LineEditor**:
##
## .. code-block:: nim
##    keyseqs["up"]         = @[27, 91, 65]      # Windows: @[224, 72]
##    keyseqs["down"]       = @[27, 91, 66]      # Windows: @[224, 80]
##    keyseqs["right"]      = @[27, 91, 67]      # Windows: @[224, 77]
##    keyseqs["left"]       = @[27, 91, 68]      # Windows: @[224, 75]
##    keyseqs["home"]       = @[27, 91, 72]      # Windows: @[224, 71]
##    keyseqs["end"]        = @[27, 91, 70]      # Windows: @[224, 79]
##    keyseqs["insert"]     = @[27, 91, 50, 126] # Windows: @[224, 82]
##    keyseqs["delete"]     = @[27, 91, 51, 126] # Windows: @[224, 83]

when defined(windows):
  keyseqs["up"] = @[224, 72]
  keyseqs["down"] = @[224, 80]
  keyseqs["right"] = @[224, 77]
  keyseqs["left"] = @[224, 75]
  keyseqs["home"] = @[224, 71]
  keyseqs["end"] = @[224, 79]
  keyseqs["insert"] = @[224, 82]
  keyseqs["delete"] = @[224, 83]
else:
  keyseqs["up"] = @[27, 91, 65]
  keyseqs["down"] = @[27, 91, 66]
  keyseqs["right"] = @[27, 91, 67]
  keyseqs["left"] = @[27, 91, 68]
  keyseqs["home"] = @[27, 91, 72]
  keyseqs["end"] = @[27, 91, 70]
  keyseqs["insert"] = @[27, 91, 50, 126]
  keyseqs["delete"] = @[27, 91, 51, 126]

# Key Mappings
var keymap* {.threadvar.}: CritBitTree[KeyCallback] ## The following key mappings are configured by default:
 ##
 ## * backspace: **deletePrevious**
 ## * delete: **deleteNext**
 ## * insert: *toggle editor mode*
 ## * down: **historyNext**
 ## * up: **historyPrevious**
 ## * ctrl+n: **historyNext**
 ## * ctrl+p: **historyPrevious**
 ## * left: **back**
 ## * right: **forward**
 ## * ctrl+b: **back**
 ## * ctrl+f: **forward**
 ## * ctrl+c: *quits the program*
 ## * ctrl+d: *quits the program*
 ## * ctrl+u: **clearLine**
 ## * ctrl+a: **goToStart**
 ## * ctrl+e: **goToEnd**
 ## * home: **goToStart**
 ## * end: **goToEnd**

keymap["backspace"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.deletePrevious()
keymap["delete"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.deleteNext()
keymap["insert"] = proc(ed: var LineEditor) {.gcsafe.} =
  if ed.mode == mdInsert:
    ed.mode = mdReplace
  else:
    ed.mode = mdInsert
keymap["down"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.historyNext()
keymap["up"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.historyPrevious()
keymap["ctrl+n"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.historyNext()
keymap["ctrl+p"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.historyPrevious()
keymap["left"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.back()
keymap["right"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.forward()
keymap["ctrl+b"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.back()
keymap["ctrl+f"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.forward()
keymap["ctrl+c"] = proc(ed: var LineEditor) {.gcsafe.} =
  quit(0)
keymap["ctrl+d"] = proc(ed: var LineEditor) {.gcsafe.} =
  quit(0)
keymap["ctrl+u"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.clearLine()
keymap["ctrl+a"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.goToStart()
keymap["ctrl+e"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.goToEnd()
keymap["home"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.goToStart()
keymap["end"] = proc(ed: var LineEditor) {.gcsafe.} =
  ed.goToEnd()

var keyMapProc {.threadvar.}: proc(ed: var LineEditor) {.gcsafe, raises: [
    IOError, ValueError], tags: [WriteIOEffect].}

proc readLine*(ed: var LineEditor, prompt = "", hidechars = false): string
  {.gcsafe, raises: [IOError, ValueError], tags: [WriteIOEffect,
      ReadIOEffect].} =
  ## High-level proc to be used instead of **stdin.readLine** to read a line from standard input using the specified **LineEditor** object.
  ##
  ## Note that:
  ## * **prompt** is a string (that *cannot* contain escape codes, so it cannot be colored) that will be prepended at the start of the line and
  ##   not included in the contents of the line itself.
  ## * If **hidechars** is set to **true**, asterisks will be printed to stdout instead of the characters entered by the user.
  stdout.write(prompt)
  stdout.flushFile()
  ed.line = Line(text: "", position: 0)
  var c = -1 # Used to manage completions
  var esc = false
  while true:
    var c1: int
    if c > 0:
      c1 = c
      c = -1
    else:
      c1 = getchr()
    if esc:
      esc = false
      continue
    elif c1 in {10, 13}:
      stdout.write("\n")
      ed.historyAdd()
      ed.historyFlush()
      return ed.line.text
    elif c1 in {8, 127}:
      keymap["backspace"](ed)
    elif c1 in PRINTABLE:
      if hidechars:
        putchr('*'.ord.cint)
        ed.line.text &= c1.chr
        ed.line.position.inc
      else:
        ed.printChar(c1)
    elif c1 == 9: # TAB
      c = ed.completeLine()
    elif c1 in ESCAPES:
      var s = newSeq[Key](0)
      s.add(c1)
      let c2 = getchr()
      s.add(c2)
      if s == keyseqs["left"]:
        keymap["left"](ed)
      elif s == keyseqs["right"]:
        keymap["right"](ed)
      elif s == keyseqs["up"]:
        keymap["up"](ed)
      elif s == keyseqs["down"]:
        keymap["down"](ed)
      elif s == keyseqs["home"]:
        keymap["home"](ed)
      elif s == keyseqs["end"]:
        keymap["end"](ed)
      elif s == keyseqs["delete"]:
        keymap["delete"](ed)
      elif s == keyseqs["insert"]:
        keymap["insert"](ed)
      elif c2 == 91:
        let c3 = getchr()
        s.add(c3)
        if s == keyseqs["right"]:
          keymap["right"](ed)
        elif s == keyseqs["left"]:
          keymap["left"](ed)
        elif s == keyseqs["up"]:
          keymap["up"](ed)
        elif s == keyseqs["down"]:
          keymap["down"](ed)
        elif s == keyseqs["home"]:
          keymap["home"](ed)
        elif s == keyseqs["end"]:
          keymap["end"](ed)
        elif c3 in {50, 51}:
          let c4 = getchr()
          s.add(c4)
          if c4 == 126 and c3 == 50:
            keymap["insert"](ed)
          elif c4 == 126 and c3 == 51:
            keymap["delete"](ed)
    elif c1 in CTRL and keymap.hasKey(keynames[c1]):
      keyMapProc = keymap[keynames[c1]]
      keyMapProc(ed)
    else:
      # Assuming unhandled two-values escape sequence; do nothing.
      if esc:
        esc = false
        continue
      else:
        esc = true
        continue

proc password*(ed: var LineEditor, prompt = ""): string
  {.raises: [IOError, ValueError], tags: [WriteIOEffect, ReadIOEffect].} =
  ## Convenience method to use instead of **readLine** to hide the characters inputed by the user.
  return ed.readLine(prompt, true)
