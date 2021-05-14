# The Mlatu Programming Language

[![](https://tokei.rs/b1/github/brightly-salty/mlatu)](https://github.com/XAMPPRocky/tokei) [![Join the chat at https://gitter.im/mlatu-lang/community](https://badges.gitter.im/mlatu-lang/community.svg)](https://gitter.im/mlatu-lang/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Mlatu is a statically typed, stack-based purely functional programming language designed for simplicity, speed, and safety.

Very experimental: contributions welcome, but please don't use this in production. If you wish to contribute see [here](/CONTRIBUTING.md) for more information.

I make announcements, discuss the languaage, and am available to answer questions on [this Discord channel](https://discord.gg/qNQV6nnAZj) and [this Gitter community](https://gitter.im/mlatu-lang/community).

## Features

Functions are first-class values, (*higher-order functions*) can be passed around as function arguments and results, and stored in data structures exactly as other values.
The use of *algebraic data types* combined with *pattern matching* make it easy to handle very complex data structures in a natural way.
The *strong type system* and automatic *type inference* provide increased safety from programming errors.
*Parametric polymorphism* encourages the programmer to write general functions that can later be reused.

## Examples

Here's a naive recursive fibonacci function:

```
define fib (nat -> nat) {
  -> x; if (x 2 le) { 1 }
  else { x pred fib x pred pred fib +}
}

20 fib println
```

Here's the definition of `exists` in std/common/list.mlt, which demonstrates a more functional style, as well as some of the functions available out of the gate in the prelude.

```
define exists (for t . t list, (t -> bool) -> bool) {
  -> f;
  true { f call or } fold-left
}
```

See the `/examples` folder for more examples.

## Installation and Usage

### Installation

These installation instructions presume you have the Haskell Tool Stack installed. If you don't you can install it [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Via `stack`:

```
git clone https://github.com/brightly-salty/mlatu.git
cd mlatu
stack install
```

Note: `cabal v2-install exe:mlatu` should also work.

### Usage

```
The Mlatu programming language

Usage: mlatu COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  repl                     Start the interactive REPL
  check                    Checks Mlatu files for correctness without running
                           them
  fmt                      Formats Mlatu files prettily
  run                      Runs Mlatu files
  build                    Builds Mlatu files into an executable
```

Type `:help` in the interactive REPL for command options.

## Miscellany

"Mlatu" is the Lojban word for "cat", referencing [Cat](https://github.com/cdiggins/cat-language) by Christopher Diggins, [Kitten](https://kittenlang.org/) by Jon Purdy, and the fact that Mlatu is a con*cat*enative programming language.

"Mlatu" is pronounced "melatoo".

The source is based with gratitude off of [the source of Kitten](https://github.com/evincarofautumn/kitten) by Jon Purdy.

Mlatu is licensed under the Peace Public License v0.0+ available [here](LICENSE.md). The canonical version is located at https://github.com/brightly-salty/peace-license
