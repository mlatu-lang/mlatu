mlatu
=====

mlatu is a declarative and concatenative programming language. It uses term rewriting as an evalutation model.

![Lines of code](https://img.shields.io/tokei/lines/github/mlatu-lang/mlatu)
![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/mlatu-lang/mlatu?include_prereleases)
![GitHub last commit](https://img.shields.io/github/last-commit/mlatu-lang/mlatu)
![Discord](https://img.shields.io/discord/889248218460852235)

Table of Contents
-----------------

- [mlatu](#mlatu)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Installation](#installation)
  - [Usage](#usage)
  - [Known issues and limitations](#known-issues-and-limitations)
  - [Getting help](#getting-help)
  - [Contributing](#contributing)
  - [Acknowledgements](#acknowledgements)
  - [License](#license)

Introduction
------------

mlatu is a concatenative programming language; you can concatenate two programs and its result will be the concatenation of the programs' individual results. It is declarative: instead of telling the implementation what to *do*, one tells the implementation what *is*. mlatu is evaluated via a term-rewriting system and so all expressions are referentially transparent, evaluating to the same result each time they are written.

Installation
------------

First, you need to have Rust and Swi-Prolog (and Clang on Windows) installed on your system and in your `PATH`. To install Rust, follow the instructions at <https://rustup.rs>. To install Swi-Prolog, you can find a download at <https://www.swi-prolog.org/Download.html>. To install Clang on Windows, you can find a download link at <https://github.com/llvm/llvm-project/releases/tag/llvmorg-12.0.1> (make sure you set `LIBCLANG_PATH` environment variable to the `bin` directory of that download).

```bash
git clone https://github.com/mlatu-lang/mlatu 
cd mlatu 
cargo install . --path
```

Usage
-----

Running `mlatu` will start up an interactive TUI with structured input. The input is entered and manipulated on the left side, and the rewritten form will appear on the right side. where toplevel terms can be typed and their respective reductions will be printed out. If any arguments are given, they are interpreted as files containing additional rewrite-rules to load. `ESC` to close the TUI.

Running `mlatu edit <FILE>` will open a structured editor for the rules contained in that file. You will be able to see and manipulate both the left (pattern) and right (replacement) sides. You can also navigate between rules and manipulate the rules just as you would terms. `CTRL-W` to save and `ESC` to close the TUI.

Known issues and limitations
----------------------------

Currently, there is no way of doing I/O (input and output). This is a top priority and will be resolved as soon as a suitable model for interacting with the rewriting system is chosen.

Another issue is the lack of true patterns and a type system. Several solutions to this issue are currently being investigated, we would love any ideas or feedback on the Discord server.

Getting help
------------

There are a couply ways to get help if you have questions, thoughts, or issues. The GitHub issue tracker (<https://github.com/mlatu-lang/mlatu/issues>) is the best place to submit issues or bug reports. If you have a thought, suggestion, question, or just want to chat about mlatu, there is a small Discord server at <https://discord.gg/WHdnkktgKr>.

Contributing
------------

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

See [this guide](/CONTRIBUTING.md) for more specific and detailed development workflows.

Acknowledgements
----------------

mlatu is inspired by a number of pre-existing concatenative and term-rewriting languages. These include but are not limited to, Kitten, Cat, Factor, Forth, min, Joy, TXL, META II, Pure, Clean, Refal, and Prolog. The structured editor is heavily inspired by Sapling.

License
-------

mlatu is licensed under the Cooperative Software License. For the terms of this license, see [the local copy](/LICENSE.md) or [the formatted web version](https://lynnesbian.space/csl/formatted).
