# Parser
A simple parser, implemented from scratch, allows scripts to be written in a very primitive programming language. The parser converts text into named expressions and adds them to an environment. Later, expressions can be evaluated against that environment.

The parser is used by the [repl](./Repl.md).

### Syntax

_This section describes the syntax rules enforced by the parser. These go beyond what is required by the underlying data in some respects. For instance, the names of named expressions and lambda terms are merely strings as far as the data constructors are concerned. But the parser requires that the names of named expressions begin with a capital letter, whereas lambda variables must begin with lower-case letters._

Simple named expressions are introduced with the `let` keyword and end with a semi-colon. Note that expression names must begin with a capital letter, while lambda terms begin with lower-case letters.

````haskell
# this is a comment...
# you can use either a 'λ' or a '\' to begin a lambda abstraction
let True  = λ x y . x;
let False = \ x y . y;
````

Abstraction is right-associative, while application is left-associative:
````haskell
# these abstractions are equivalent
let True  = λ x .  λ y . x;
let True  = λ x . (λ y . x);

# this shorthand version is the same too
let True  = λ x y . x;

# these applications are equivalent
let Double = λ n .  Plus n  n;
let Double = λ n . (Plus n) n;
````

Expressions may refer in their bodies to *previously* defined items:
````haskell
let App = λ x y . x y;

# Y's body refers to previously defined 'App'
let Y = App (λ x . x) (λ x . x);
````

Expressions can refer to 'private' definitions. These have higher precedence that 'public' definitions and may therefore override them. There is no restriction on private definitions - they can be simple or recursive (see below) and may have private definitions of their own. 

Expressions with private definitions do **not** end with a semi-colon. Instead, a colon signals the presence of private definitions, which are enclosed within braces.

````haskell
# One and Two are private definitions visible only in the body of Three
let Three = Plus One Two :
{
    let One = Succ Zero;
    let Two = Succ One;
}

# ERROR! the definition of Two is not visible here!
let Four = Plus Two Two;
````

A [recursive definition](./SimpleRecursion.md) cannot be made using a `let` statement because its body contains references not *previously* defined, instead a `letrec` expression must be used. `letrec` definitions may also have private definitions.

````haskell
# ERROR! this doesn't work because 'Factorial' is used in the body
let    Factorial = λn . IfThenElse (IsZero n) One (Mult n (Factorial (Pred n))) : {let One = Succ Zero;}

# this is OK because it begins with 'letrec'
letrec Factorial = λn . IfThenElse (IsZero n) One (Mult n (Factorial (Pred n))) : {let One = Succ Zero;}
````
There are *block* versions of both `let` and `letrec` expressions which allow multiple definitions to share private definitions. Also, in the case of a `letrec` block, the definitions may be [mutually recursive](./MutualRecursion.md). Note that while the block as a whole may have private definitions, individual definitions in the block must be simple `let` definitions *without* private definitions.

````haskell
# 'And' 'Or' 'Not' all share private definitions of 'T' 'F' 'ITE'
let
{
    # must be let expressions wihtout private definitions
    let And = λ x y . ITE x y F;
    let Or  = λ x y . ITE x T y;
    let Not = λ x   . ITE x F T;
} :
{
    # private definitions - can be recursive and have their own private definitions
    let T = True;
    let F = False;
    let ITE = IfThenElse;
}

# mutual recursion - (1) IsEven calls IsOdd (2) IsOdd calls IsEven
letrec
{
    # must be let expressions without private definitions
    let IsEven = \ n . (IsZero n) True  (IsOdd  (Pred n));
    let IsOdd  = \ n . (IsZero n) False (IsEven (Pred n));
}
````
