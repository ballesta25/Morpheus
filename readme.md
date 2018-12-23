# Morpheus

The intention with this is to create a programming language that deals natively with derivational morphemes.


As an example of what that means, consider the `exec` family of Linux syscalls: it's clear to a human what `execvpe` does from the definitions of `exec`, `execp`, `execv`, and `exece`, but the function needs a separate definition so that the compiler can tell.  With derivational morphemes, you would be able to define what the suffix `-p` does and then the meaning of `execvp` could be automatically derived from the meaning of `execv`.


The language is stack-based, and until I implement more, is essentially a glorified RPN calculator.


## Syntax

### Basics

All tokens are separated by whitespace (this includes after opening- and before closing-parentheses).


Basic arithmetic is done in stack-based (RPN) order:
```
2 3 +
```
pushes `2` and `3` to the stack, then uses the `+` operator to pop both and then push the result of adding them, giving `5`.


```
7 1 -
```
yields `6`.


```
2 4 1 + -
```
gives `-3` (it computes as 2 - (4 + 1)).

### Names and Bindings

Any alphanumeric string preceded by a `'` is considered a name literal.  You can bind values to names with the `bind` keyword.  For example, to bind the name `var` to `2` run
```
2 'var bind
```
after that, in the same scope whenever Morpheus sees the token `var` it will have the value `3`

### Quotations

A string of Morpheus code is treated as a single object if it is stored in a Quotation.  To write a quotation, simply enclose the code in `[` square brackets `]`, remembering to leave a space both before and after each bracket.  You can execute the statements in a Quotation with the `exec` keyword.


```
[ 1 + ] 'inc bind
```
will bind the name `inc` to a quotation that increments a number.  You can then use inc like a function:
```
4 inc exec
```
yields `5`.


### Example: the Omega Combinator

The omega combinator:

&#969; = (&#955; x . x x) (&#955; x . x x)

is a lambda calculus term that diverges: reducing it one step yields the original expression.

We can write the omega combinator in Morpheus as
```
[ [ 'x bind x x exec ] [ 'x bind x x exec ] exec ] 'omega bind
```

After this definition, running `omega exec` sends Morpheus into an infinite loop.  (Eventually, the interpreter will run out of memory, since it maintains the scope of each function call: I haven't implemented proper tail calls.)