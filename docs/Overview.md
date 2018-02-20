# Overview

Implementation of untyped lambda calculus in Haskell using only functions available from the standard prelude.

The design is layered, with each layer building on the previous. The layers are described below.

### Lamba Expressions
The base layer contains lambda expressions along with certain key functions related to α and β conversion. Lambda Expressions are represented as a Haskell data type:

````haskell
data Λ = V Var | A Λ Λ | Λ Var Λ
````

Lambda expressions are anonymous - there is no concept of a named expression. This means that an expression can only be re-used by literally copying it in its entirety (since it can't be referred to by name). This limitation is remedied by *Named Expressions*.

Lambda expressions are defined in [Terms.hs](../src/Terms.hs)

### Named Expressions
A named expression is similar to a lambda expression except that it has a name and its body may itself contain references to other named expressions. This allows complex expressions to be defined conveniently by re-using earlier definitions, but it's really no more than syntactic sugar because all references are translated into raw anonymous lambda expressions before evaluation. (This translation is particularly interesting when [simple](./SimpleRecursion.md) or [mutual](./MutualRecursion.md) recursion is involved)

##### Environment
Named expressions live inside an environment. References are translated into lambda expressions by looking them up in the environment. 

Named expressions and environments are defined in [Env.hs](../src/Env.hs)

### Parser syntax
A simple [parser](./Syntax.md) allows named expressions to be created from scripts that resemble a primitive programming language. The parser is implemented using the approach described by Erik Meijer in his [Haskell course] (https://www.edx.org/course/introduction-functional-programming-delftx-fp101x-0).

The parser is defined in [Parse.hs](../src/Parse.hs). 

### REPL
The final layer is a [REPL](./Repl.md) which allows a user to interactively define and evaluate expressions. These can be typed directly into the repl or loaded from scripts files.

The REPL is defined in [Repl.hs](../src/Repl.hs)
