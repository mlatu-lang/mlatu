# Mlatu

Mlatu is a statically-typed purely-functional concatenative high-level programming language on the BEAM VM.

## Examples

Factorial:

```ml
define fact (nat -> nat) {
  dup match | zero { succ } | succ { fact * }
}
```

Fibonacci:

```ml
define fib (nat -> nat) {
  match | zero { 1 } | succ { -> x; match (x) | zero { 1 } | succ { fib x fib + } }
}
```
## Explore:

[What is Mlatu, and why should you use it?](/intro.md)

[How to install Mlatu](/installation.md)