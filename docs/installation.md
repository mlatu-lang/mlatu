# Installing Mlatu

There are several ways to install Mlatu, depending on your needs and system's capabilities.



## Installing  a binary release

The easiest way to try out Mlatu is to install a nightly binary from the GitHub page at https://github.com/brightly-salty/mlatu/releases . There is a new nightly published every day the source changes meaningfully, and one is provided for Windows, Linux, and macOS.

## Building from source

If you want to contribute in the future, the best way to install Mlatu is by building from source.

### Stack

([How to install `stack`](https://docs.haskellstack.org/en/stable/install_and_upgrade/))

 To install with `stack`, run the following, which will build Mlatu and symlink/copy the executable in `~/.local/bin` (you may need to add `$HOME/.local/bin` to your `$PATH`)

```sh
git clone https://github.com/brightly-salty/mlatu.git
cd mlatu
stack install
```

### Cabal

To install with `cabal`, run the following, which will build Mlatu and symlink/copy the executable in `~/.cabal/bin` (you may need to add `$HOME/.cabal/bin` to your `$PATH`)

```sh
git clone https://github.com/brightly-salty/mlatu.git
cd mlatu
cabal v2-install exe:mlatu
```

## Before you use Mlatu

You will need to have the executables `erlc` and `escript` in your `PATH` variable so that `mlatu` can call them.

If you don't already have these installed, refer to https://adoptingerlang.org/docs/development/setup/.
