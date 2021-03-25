# The Mlatu Programming Language

[![](https://tokei.rs/b1/github/brightly-salty/mlatu)](https://github.com/XAMPPRocky/tokei) [![Join the chat at https://gitter.im/mlatu-lang/community](https://badges.gitter.im/mlatu-lang/community.svg)](https://gitter.im/mlatu-lang/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Mlatu is a statically typed, stack-based functional programming language designed for simplicity, speed, and safety. Mlatu features algebraic data types and algebraic effects (called permissions).

Very experimental: contributions welcome, but please don't use this in production. If you wish to contribute see [here](/CONTRIBUTING.md) for more information.

I make announcements, discuss the languaage, and am available to answer questions on [this Discord channel](https://discord.gg/qNQV6nnAZj) and [this Gitter community](https://gitter.im/mlatu-lang/community).

## Examples

Here's a naive recursive fibonacci function:

```
define fib (Int -> Int) {
  -> n;
  if (n < 2) {
    1
  }
  else {
    (n - 2) fib + (n - 1) fib
  }
}

20 fib print
```

Here's the definition of `or` in common/list.mlt, which demonstrates a more functional style, as well as some of the functions available out of the gate in the prelude.

```
define or (List[Bool] -> Bool) {
  true \(|) (fold_left)
}
```

See the /examples folder for more examples.

## Installation and Usage

### Installation

These installation instructions presume you have the Haskell Tool Stack installed. If you don't you can install it [here][Stack Installation].

Via `stack`:

```
git clone https://github.com/brightly-salty/mlatu.git
cd mlatu
stack install
```

Note: `cabal install` should also work.

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
```

Type `:help` in the interactive REPL for command options.

## Miscellany

"Mlatu" is the Lojban word for "cat", referencing [Cat][Cat GitHub] by Christopher Diggins, [Kitten][Kitten Site] by Jon Purdy, and the fact that Mlatu is a con*cat*enative programming language.

"Mlatu" is pronounced "melatoo".

The source is based with gratitude off of [the source of Kitten][Kitten GitHub] by Jon Purdy.

Mlatu is licensed under the Peace Public License v0.0+ available [here](LICENSE.md). The canonical version is located at https://github.com/brightly-salty/peace-license

[Kitten GitHub]: https://github.com/evincarofautumn/kitten

[Kitten Site]: https://kittenlang.org/

[Cat GitHub]: https://github.com/cdiggins/cat-language

[Stack Installation]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
