# The Mlatu Programming Language

**This is the WIP branch that's working on a Nim interpreter. Use the Haskell version if you want something that works.**

![Mlatu logo](/assets/logo.jpg)

Mlatu is a statically-typed purely-functional concatenative high-level programming language that is interpreted and compiled to Nim.

You will need to have `nimble` installed to contribute to Mlatu (other than documentation changes). You can download both `nimble` and `nim` [here](https://nim-lang.org/install.html).

You will also need sdl2 bindings available on your system. Follow [these instructions](https://github.com/nim-lang/sdl2#pre-requisites) to install sdl2 if you don't have it already.

To build, run `nimble build`.
To build and start the repl, run `nimble run`.
To install mlatu into your $PATH, run `nimble install`.

## Contributing

Very experimental: contributions welcome, but please don't use this in production. If you wish to contribute see [here](/CONTRIBUTING.md) for more information.

I make announcements, discuss the languaage, and am available to answer questions on [this Discord server](https://discord.gg/qNQV6nnAZj).

## Miscellany

"Mlatu" is the Lojban word for "cat", referencing [Cat](https://github.com/cdiggins/cat-language) by Christopher Diggins, [Kitten](https://kittenlang.org/) by Jon Purdy, and the fact that Mlatu is a con*cat*enative programming language.

"Mlatu" is pronounced "melatoo", as best as I can tell.

The source is based with gratitude off of [the source of Kitten](https://github.com/evincarofautumn/kitten) by Jon Purdy.

Mlatu is licensed under the CNPLv6, the text of which is available [here](LICENSE).
