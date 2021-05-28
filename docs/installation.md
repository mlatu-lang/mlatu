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

Nightly Rust is expected to be available. If you don't have it installed already, run (only works on Unix)
```sh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup install nightly
```

To be able to compile Mlatu programs offline and decrease build times, prefetch `smallvec` by running
```sh
cargo install cargo-prefetch
cargo prefetch smallvec
```

If you are on macOS and do not have `zld` installed, run (requires Xcode to be installed)
```sh
brew install michaeleisel/zld/zld
```
