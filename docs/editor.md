---
layout: default
title: Editor
nav_order: 3
---

# The Editor and Interface

Let's look at the structured editor and interface for mlatu and how to use them.

## The Editor

You can start start by running `mlatu edit file.mlb` (`mlb` is the extension for mlatu binary files, and `mlt` for mlatu text files). This will open or create a blank binary mlatu file and start up the editor.

You will immediately notice that there are two sides, the pattern on the left and the replacement on the right. These are the two parts of your first rule. The left and right arrow keys can be used to navigate between the two sides. 

To add a new word to the pattern, start by pressing the space key. This will replace the filename at the top with a dialog where you can type your word. Type another space to enter the word into the pattern.

You can thus insert any amount of words in the pattern and replacement sides, and navigate up and down the list by using the - you guessed it - up and down keys. You will notice that you cannot insert a quotation with this method; this is intentional. Instead of writing quotations with parentheses, you can create them with the primitive commands.

The editor's basic commands include `CTRL-W` to save the file and `ESC` to exit. But there are also six primitive commands based on the six primitives of mlatu. They have the same effects on the pattern or replacement sides as the primitves would if placed above the location of the cursor.

A new empty rule can be created by `CTRL-Space` and the rules can be navigated with the left and right arrows. `CTRL-R` will remove a rule.

## The Interface

The interface behaves very similarly to the editor, except only the left side is editable. The right side is reserved for the output of rewriting the input according to the loaded rulis.

The left side can be manipulated with the primitives just like in the editor, and additional binary or text files can be loaded on the command line.

*This is the end of the tutorial for now and it should be enough to get started with mlatu. Feel free to ask any questions or get more help on the Discord server, which is linked on the readme.*
