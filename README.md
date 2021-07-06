# The Mlatu Programming Language

**This is the WIP branch that's working on a Mercury compiler. Use the Haskell version if you want something that works.**

![Mlatu logo](/logo.jpg)

Mlatu is a statically-typed purely-functional concatenative high-level programming language that compiles to Assembly.

To build the current version, run 
```sh
$ cd src 
$ make
```
which will output the maximum amount of warnings and errors and output an unoptimized build to `./mlatu`. To install an optimized build to `~/.local/bin/mlatu`, run `make install`. To clean this output, run `make clean`.
## Contributing

Very experimental: contributions welcome, but please don't use this in production. If you wish to contribute see [here](/CONTRIBUTING.md) for more information.

I make announcements, discuss the languaage, and am available to answer questions on [this Discord server](https://discord.gg/qNQV6nnAZj).

## Miscellany

"Mlatu" is the Lojban word for "cat", referencing [Cat](https://github.com/cdiggins/cat-language) by Christopher Diggins, [Kitten](https://kittenlang.org/) by Jon Purdy, and the fact that Mlatu is a con*cat*enative programming language.

"Mlatu" is pronounced "melatoo", as best as I can tell.

The source is based with gratitude off of [the source of Kitten](https://github.com/evincarofautumn/kitten) by Jon Purdy.

Mlatu is licensed under the CNPLv6, the text of which is available [here](LICENSE).
