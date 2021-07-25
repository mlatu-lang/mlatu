import os, pathnorm, strutils

when FileSystemCaseSensitive:
  template `!=?`(a, b: char): bool = a != b
else:
  template `!=?`(a, b: char): bool = toLowerAscii(a) != toLowerAscii(b)

proc relative_path*(path, base: string): string {.extern: "nos$1", tags: [].} =
  if path.len == 0: return ""
  var base = if base == ".": "" else: base
  var path = path
  path.normalizePath
  base.normalizePath
  let a1 = isAbsolute(path)
  let a2 = isAbsolute(base)
  if a1 and not a2:
    base = absolutePath(base, get_current_dir())
  elif a2 and not a1:
    path = absolutePath(path, get_current_dir())

  when doslikeFileSystem:
    if isAbsolute(path) and isAbsolute(base):
      if not sameRoot(path, base):
        return path

  var f = default PathIter
  var b = default PathIter
  var ff = (0, -1)
  var bb = (0, -1) # (int, int)
  result = newStringOfCap(path.len)
  # skip the common prefix:
  while f.hasNext(path) and b.hasNext(base):
    ff = next(f, path)
    bb = next(b, base)
    let diff = ff[1] - ff[0]
    if diff != bb[1] - bb[0]: break
    var same = true
    for i in 0..diff:
      if path[i + ff[0]] !=? base[i + bb[0]]:
        same = false
        break
    if not same: break
    ff = (0, -1)
    bb = (0, -1)
  while true:
    if bb[1] >= bb[0]:
      if result.len > 0 and result[^1] != DirSep:
        result.add DirSep
      result.add ".."
    if not b.hasNext(base): break
    bb = b.next(base)
  while true:
    if ff[1] >= ff[0]:
      if result.len > 0 and result[^1] != DirSep:
        result.add DirSep
      for i in 0..ff[1] - ff[0]:
        result.add path[i + ff[0]]
    if not f.hasNext(path): break
    ff = f.next(path)

  if result.len == 0: result.add "."
