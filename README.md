#Morpheus

The intention with this is to create a programming language that deals natively with derivational morphemes.


As an example of what that means, consider the `exec` family: it's clear to a human what `execvpe` does from the other definitions, but the function needs a separate definition so that the compiler can tell.  With derivational morphemes, you would be able to define what the suffix `-p` does and then the meaning of `execvp` could be automatically derived from the meaning of `execv`.


At the momemt though, it only works as an RPN calculator.