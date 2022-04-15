# an informal tutorial for mlatu

mlatu is a language that rewrites some terms to other terms  
these terms are either whitespace-delimited atoms (unless they're one of the 6 primitives, in which case they're always parsed as one character), or quotations (`quotation ::= '(' term* ')'`)  
a quotation is a collection of AST nodes (the same as quotations in joy, and a list of symbols in lisp terms)  

there are 6 primitive rewriting functions: `<>,~+-,` which all act on quotes (see [here](https://hackmd.io/pgI_ipgfSMWc-MQvSgmC2g#The-Church-Rosser-theorem) (note: is this even necessary when mlatu, by design, already doesn't follow the Church-Rosser theorem? I don't know))  
the symbol `|->` means "rewrites to" and was completely made up by ISO  
if we have quotes `x` and `y`,
```
x+    |->  x x
x-    |->  
x>    |->  (x)
x y~  |->  y x
```
if you are even slightly competent at math, you will notice that that is only 4 out of the 6 functions. here are the remaining two:  
`<` unwraps a quote. that means that `(foo bar baz)<` will rewrite to `foo bar baz`  
`,` catenates two quotes. `(foo bar) (baz) ,` will rewrite to `(foo bar baz)`  

you probably have realized that mlatu is RPN. here is the full evaluation order:  
```
1. find the longest rewrite rule starting at the first term  
  a. if it was found, rewrite it, and goto 1.  
  b. if it was not found, look for the longest rewrite rule starting at the second term  
    i.   if it was found, rewrite it, and goto 1.  
    ii.  if it was not found, continue looking with the third term etc.  
    iii. if no rule is found that matches anywhere, the rewriting is finished  
```

you can also define rules yourself:
```
a b = c;
```
is a rule that matches on `a b`, and will rewrite it to `c`.
user-defined rules may not match on quotes.  

for more examples of defined rules, see [better-nat.mlt](https://github.com/mlatu-lang/rebel-libraries/blob/main/better-nat.mlt), an implementation of peano numerals
