---
layout: default
title: Primitives
nav_order: 2
---

# The Primitives

There are six primitives of mlatu which have various effects on the terms below them.


### Removing a term

`r` or `remove` removes the term below it:

```
-------| Input |--------------| Output |-------
|          x           |          x           |
|        remove        |          z           |
|          y           |                      |
|          z           |                      |
```

### Duplicating a term

`d` or `duplicate` duplicates the term below it:

```
-------| Input |--------------| Output |-------
|          x           |          x           |
|         dup          |          y           |
|          y           |          y           |
|          z           |          z           |
```

### Swapping terms

`s` or `swap` swaps the two terms below it:

```
-------| Input |--------------| Output |-------
|          x           |          x           |
|         swap         |          z           |
|          y           |          y           |
|          z           |                      |
```

### Quoting a term

`q` or `quote` quotes the term below it:

```
-------| Input |--------------| Output |-------
|          x           |          x           |
|        quote         |        ( y )         |
|          y           |          z           |
|          z           |                      |
```

### Unquoting a quotation

`u` or `unquote` unquotes the quotation below it:

```
-------| Input |--------------| Output |-------
|          x           |          x           |
|       unquote        |          y           |
|        ( y )         |          z           |
|          z           |                      |
```

### Concatenating quotations

`c` or `concat` concaatenates the two quotations below it:

```
-------| Input |--------------| Output |-------
|          x           |          x           |
|        concat        |       ( y z )        |
|        ( y )         |                      |
|        ( z )         |                      |
```

[Onward, to the mlatu structured editor!](editor.md)

