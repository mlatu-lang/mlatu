# The Mlatu Programming Language

[![](https://tokei.rs/b1/github/brightly-salty/mlatu)](https://github.com/XAMPPRocky/tokei).

Mlatu is a statically typed, stack-based functional programming language designed for simplicity, speed, and safety. 

Very experimental: contributions welcome, but please don't use this in production.

## About the name

"Mlatu" is the Lojban word for "cat", referencing [Cat][Cat GitHub] by Christopher Diggins, [Kitten][Kitten Site] by Jon Purdy, and the fact that Mlatu is a con*cat*enative programming language.

"Mlatu" is pronounced "melatoo".

## Todo (unordered)

- [ ] Create a compiler backend that generates WASM

- [ ] Create a formatter that can format the common files

- [ ] Benchmark or profile so that performance can be optimized

- [ ] Hint on code that is repetitious of common functions

- [ ] Support generic trait instances

- [ ] Support trait constraints on parameters

## Examples

Here's a naive recursive fibonacci function:

```
define fib (UInt64 -> UInt64):
  -> n;
  if (n < 2u64):
    1u64
  else:
    (n - 2u64) fib + 
    (n - 1u64) fib

10u64 fib print
```
See the examples folder for more.

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

At the moment, there are only two ways to use `mlatu`.

First, you can use `mlatu` without any arguments to start a REPL:

```
> mlatu
Welcome to Mlatu! Type //help for help or //quit to quit
    1: 
```

Second, you can use `mlatu` to interpret files:

```
> mlatu examples/fizzbuzz.mlt
```

## Miscellany

The source is based with gratitude off of [the source of Kitten][Kitten GitHub] by Jon Purdy.

[Kitten GitHub]: https://github.com/evincarofautumn/kitten

[Kitten Site]: https://kittenlang.org/

[Cat GitHub]: https://github.com/cdiggins/cat-language

[Stack Installation]: https://docs.haskellstack.org/en/stable/install_and_upgrade/