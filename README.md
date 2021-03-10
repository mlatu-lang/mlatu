# The Mlatu Programming Language

[![](https://tokei.rs/b1/github/brightly-salty/mlatu)](https://github.com/XAMPPRocky/tokei)

Mlatu is a statically typed, stack-based functional programming language designed for simplicity, speed, and safety. 

Very experimental: contributions welcome, but please don't use this in production. If you wish to contribute see [here](/CONTRIBUTING.md) for more information.

I make announcements, discuss the languaage, and am available to answer questions on [this Discord channel](https://discord.gg/qNQV6nnAZj).

## Examples

Here's a naive recursive fibonacci function:

```
define fib (Int -> Int):
  -> n;
  if (n < 2):
    1
  else:
    (n - 2) fib + (n - 1) fib

20 fib print
```

Here's the definition of `or` in common/list.mlt, which demonstrates a more functional style, as well as some of the functions available out of the gate in the prelude.

```
define or (List[Bool] -> Bool):
  true \(|) (fold_left)
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
mlatu

Usage: mlatu [--version] COMMAND
  The Mlatu programming language

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  fmt                      Formats Mlatu files prettily
  check                    Checks Mlatu files for errors
  run                      Interprets Mlatu files
  repl                     Starts the Mlatu REPL
```

Type `//help` in the REPL for command options in the REPL.

## Miscellany

"Mlatu" is the Lojban word for "cat", referencing [Cat][Cat GitHub] by Christopher Diggins, [Kitten][Kitten Site] by Jon Purdy, and the fact that Mlatu is a con*cat*enative programming language.

"Mlatu" is pronounced "melatoo".

The source is based with gratitude off of [the source of Kitten][Kitten GitHub] by Jon Purdy.

[Kitten GitHub]: https://github.com/evincarofautumn/kitten

[Kitten Site]: https://kittenlang.org/

[Cat GitHub]: https://github.com/cdiggins/cat-language

[Stack Installation]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
