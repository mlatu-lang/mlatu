# Mlatu

Factorial

```ml
define fact (nat -> nat) {
  dup                         // duplicates the input so we can use it later
  match                       // matches on the input, a natural number
  | zero { succ }             // if zero, increment by one
  | succ { fact * }           // otherwise, take factorial of one less and multiply
}
```

Fibonacci

```ml
define fib (nat -> nat) {
  -> x; match (x 2 le) | true { 1 } | false { x pred fib x pred pred fib + }
}

20 fib println
```

## What is Mlatu?

Mlatu is a statically-typed purely-functional concatenative high-level programming language on the BEAM VM. That's a bit of a mouthful, so let's break that down.

### Statically typed

Mlatu has a strong static type system like many programming languages in the ML family. Every word you define needs a type signature but local variables and everything else gets its type inferred. You can define your own algebraic data types and record types, and then pattern match over them.

### Purely functional

Mlatu is purely functional, like Haskell and Idris. All side effects in Mlatu are tracked automatically through type inference and the algebraic effect system. You can give functions permission to do certain side effects and revoke those permissions for other functions. You don't have to understand monads to do pure functional programming in Mlatu.

### Concatenative

Mlatu is concatenative, like Forth. You program and read in the same direction data flows, and composition is the basis instead of application. This means point-free functional programming is the default in Mlatu, making it require more work to write in a point-ful style though it is still possible.

### High-level

Mlatu is high-level - you don't have to worry about bits and null when you program in Mlatu. You can write in a way that maps well to the abstractions in your head without caring about performance because the compiler will worry about that for you.


### On the BEAM VM

Mlatu source programs compile into BEAM bytecode - the same VM used by Erlang, Elixir, and other languages in the BEAM ecosystem. This means that Mlatu can easily take advantage of Erlang concurrency primitives and can be used as a pure typed frontend to conventional Erlang or Elixir applications.

## Now what?

Now that you've seen what Mlatu is, what are you waiting for? 

[Install Mlatu](/installation.md)

Then play around in the REPL or run the examples, and see for yourself how nice Mlatu makes programming!
