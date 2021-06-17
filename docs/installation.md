# How to install Mlatu

You can install Mlatu by downloading a binary release or by building from source.

## Downloading a binary release

The easiest way to try out Mlatu is to download a nightly binary from the GitHub page at https://github.com/brightly-salty/mlatu/releases . There is a new nightly published every day the source changes meaningfully, and one is provided for Windows, Linux, and macOS. To install, download the file for your platform, unzip it, and move it to your PATH.

## Building from source

If your operating system is not yet supported in the binary release (if so, please make an issue, I'd love to support it) or you want to contribute in the future, you might want to build Mlatu from source.

To build Mlatu from source, you will need either `cabal` or `stack`.

### Building with Stack

([How to install `stack`](https://docs.haskellstack.org/en/stable/install_and_upgrade/))

 To install with `stack`, run the following, which will build Mlatu and symlink/copy the executable to `~/.local/bin`. You may need to add `$HOME/.local/bin` to your `$PATH` if its not already there.

```sh
git clone https://github.com/brightly-salty/mlatu.git
cd mlatu
stack install
```

### Building with Cabal

To install with `cabal`, run the following, which will build Mlatu and symlink/copy the executable in `~/.cabal/bin`. You may need to add `$HOME/.cabal/bin` to your `$PATH` if its not already there.

```sh
git clone https://github.com/brightly-salty/mlatu.git
cd mlatu
cabal v2-install exe:mlatu
```

## Other dependencies

To compile the Erlang programs which Mlatu outputs into BEAM bytecode, you will need `erlc` (the Erlang compiler) and to run the BEAM bytecode files, you will need `escript`. If you don't already have these installed, refer to https://adoptingerlang.org/docs/development/setup/.

[Back to index](/index.md)
