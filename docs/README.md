mlatu
=====

mlatu is a purely-functional concatenative programming language. 

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

mlatu is a concatenative (sometimes known as stack-oriented, although mlatu isn't) programming language. It is purely functional in that it is evaluated via a term-rewriting system and so all expressions are referentially transparent.

mlatu's type system is pluggable, which means that if you define a type-checking plugin and install it, mlatu will check all your code with that type system. This enables adjusting the amount of type safety you desire by installing more or less rigorous type system.

Installation
------------

Assuming you have `git` and `npm` installed and in your `$PATH`, `install.sh` will install `mlatu` into `/usr/local/bin/`. It will install `esy` if not already present, and all other dependencies will be installed into a local sandbox.

```bash
git clone https://github.com/mlatu-lang/mlatu 
cd mlatu 
./install.sh
```

Usage
-----

Running `mlatu` will start up a REPL (Read-Eval-Print-Loop) where toplevel terms can be typed and their respective reductions will be printed out. If any arguments are given, they are interpreted as files containing additional rewrite-rules to load.

```console
$ mlatu
Loading files...
> 1 2 +
= 3 
> 1 dup 2 pop 3 
= 1 1 3
```

Known issues and limitations
----------------------------
Currently no way of doing I/O (input and output). This is a top priority and will be resolved as soon as a suitable model for interacting with the rewriting system is chosen.

Getting help
------------

There are a couply ways to get help if you have questions, thoughts, or issues. The GitHub issue tracker (https://github.com/mlatu-lang/mlatu/issues) is the best place to submit issues or bug reports. If you just have a thought or question, I have a small Discord server at https://discord.gg/WHdnkktgKr . 

Contributing
------------
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

See [this guide](/CONTRIBUTING.md) for more specific and detailed development workflows.

Acknowledgements
----------------
mlatu is inspired by a number of pre-existing concatenative languages, including but not limited to Forth, Joy, Factor, Cat, Kitten, Min, Om, Enchilada, and Popr.

License
-------

mlatu is licensed under the Cooperative Nonviolent Public License v7+ (CNPLv7+). For the terms of this license, see [the local copy](/LICENSE.md) or [the canonical version](https://git.pixie.town/thufie/npl-builder/src/branch/main/cnpl.md).
