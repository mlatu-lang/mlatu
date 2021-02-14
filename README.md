# The Mlatu Programming Language

Mlatu is a statically typed, stack-based functional programming language designed for simplicity, speed, and safety. 

Very experimental: contributions welcome, but please don't use this in production.

## About the name

"Mlatu" is the Lojban word for "cat", referencing [Cat][Cat GitHub] by Christopher Diggins, [Kitten][Kitten Site] by Jon Purdy, and the fact that Mlatu is a con*cat*enative programming language.

"Mlatu" is pronounced "melatoo".

## Todo (unordered)

- [ ] Generate Rust code from IR

- [ ] Format IR back into Mlatu

- [ ] Rework syntax into something a little bit nicer

- [ ] Write a TextMate grammar for syntax highlighting

## Examples

Here's the always-popular FizzBuzz problem:

```
define divisible (Int32, Int32 -> Bool +Fail):
  (%) 0 (=)
define fizzbuzz (Int32 -> List[Char]):
  -> n;
  do (with (+Fail)):
    n 5 divisible
    n 3 divisible
  if:
    if: "FizzBuzz"
    else: "Fizz"
  else:
    if: "Buzz"
    else: n show
define fizzbuzzes (Int32, Int32 -> +IO):
  -> c, m;
  c fizzbuzz say
  if (c < m): (c + 1) m fizzbuzzes
1 100 fizzbuzzes
```

Here's a basic fibonacci function:
```
define fib (UInt64 -> UInt64):
  -> n;
  if (n <= 1u64):
    1u64
  else:
    (n - 2u64) fib + (n - 1u64) fib
```


## Miscellany

The source is based with gratitude off of [the source of Kitten][Kitten GitHub] by Jon Purdy.



[Kitten GitHub]: https://github.com/evincarofautumn/kitten

[Kitten Site]: https://kittenlang.org/

[Cat GitHub]: https://github.com/cdiggins/cat-language