# REPL
The repl allows a user to create and evaluate lambda expressions interactively.

### starting the REPL

After the project has been built (with `stack build`), the repl can be started by running the executable (`lam-exe`) from a terminal:
````text
.stack-work/install/x86_64-linux/lts-9.21/8.0.2/bin/lam-exe
````
The repl understands a small set of commands, which are described below.

### REPL commands

| Command | Description | Example |
| -------- | -------- | -------- |
| `readDefs` | enter new definitions| `readDefs` let True  = λ x y . x; let False = λ x y . y;  |
| `loadDefs` | load new definitions from file | `loadDefs` ./scripts/core.txt |
| `intDef`   | creates a Church numeral for a natural number | `intDef` 5 |
| `eval`     | beta-reduces an expression completely | `eval` (λ x y . x) a b |
| `evalAll`  | same as eval but shows all reductions | `evalAll` (λ x y . x) a b |
| `evalInt`  | same as eval but interprets result as number | `evalInt` Succ Zero |
| `evalBool`  | same as eval but interprets result as boolean | `evalBool` Not True |
| `evalIntAll` | same as evalInt but shows all reductions | `evalIntAll` Succ Zero |
| `evalBoolAll` | same as evalBool but shows all reductions | `evalBoolAll` Not True |
| `clearEnv` | clears all definitions from session | `clearEnv` |
| `quit`   | exit the repl | `quit` |


Notes:

* new definitions must adhere to the syntax expected by the [parser](./Syntax.md)
* the `intDef` command creates a new reference e.g `intDef 5` will create an expression named `N5`

### Examples

Basic input and evaluation:

````haskell
Enter cmd:
readDefs let true = \x y . x                         
> !!! Could not parse input !!!                    // let expressions end with a semi-colon
Enter cmd:
readDefs let true = \x y . x;
> !!! Could not parse input !!!                    // expression names begin with capital letters
Enter cmd:
readDefs let True = \x y . x;
> Defintions added to environment.
Enter cmd:
readDefs let False = \ x y . y;
> Defintions added to environment.
Enter cmd:
readDefs let IfThenElse = \ p t f . p t f;         // define a ternary boolean operator
> Defintions added to environment.
Enter cmd:
eval IfThenElse True foo bar                       // foo and bar are free lambda terms
> Expression evaluates to:
foo
Enter cmd:
eval IfThenElse False foo bar
> Expression evaluates to:
bar
Enter cmd:
eval Not True
> !!! Could not resolve reference: Not !!!         // Not hasn't been defined yet
Enter cmd:
readDefs let Not = \ b . IfThenElse b False True;
> Defintions added to environment.
Enter cmd:
eval Not True
> Expression evaluates to:
λx y⋅y                                             // right answer but not easy to interpret
Enter cmd:
evalBool Not True
> Expression evaluates to:
λx y⋅y
which is recognized as: False                      // same answer but 'cast' to boolean
> end.
Enter cmd:
quit
````

Loading definitions from files:

````haskell
Enter cmd:
loadDefs ./scripts/factorial.txt                   // load definitions from file
> Defintions added to environment.
Enter cmd:
intDef 3                                           // equivalent to: let N3 = λfx⋅f (f (f x));
> Defintion added to environment: 
let N3 = λfx⋅f (f (f x));
Enter cmd:
eval Factorial N3
> !!! Could not resolve reference: IfThenElse !!!  // fails because of missing definition
Enter cmd:
loadDefs ./scripts/core.txt                        // 'core' contains missing definitions
> Defintions added to environment.
Enter cmd:
eval Factorial N3
> !!! Could not resolve reference: IfThenElse !!!  // still broken - must load factorial.txt again
Enter cmd:
loadDefs ./scripts/factorial.txt                   // now factorial can see 'core' definitions
> Defintions added to environment.
Enter cmd:
eval Factorial N3
> Expression evaluates to:
λf x⋅f (f (f (f (f (f x)))))                       // right answer but hard to interpret
Enter cmd:
evalInt Factorial N3
> Expression evaluates to:
λf x⋅f (f (f (f (f (f x)))))
which is recognized as: 6                          // same answer but cast to integer
> end.
Enter cmd:
evalIntAll Factorial N3                            // same calculation by showing reductions
> Expression evaluates to:
step 0:
(λf⋅(λx⋅f (x x)) (λx⋅f (x x))) (λf' n⋅(λp x y⋅p x y) ((λn⋅n (λx x y⋅y) (λx y⋅x)) n) (λf x⋅f x) ((λx y⋅y ((λm
n f⋅(λf g x⋅f (g x)) (m f) (n f)) x) (λf x⋅x)) n (f' ((λn⋅(λp⋅p (λx y⋅x)) (n (λx⋅(λx y f⋅f x y) ((λp⋅p (λx
y⋅y)) x) ((λm n f⋅(λf g x⋅f (g x)) (m f) (n f)) (λf x⋅f x) ((λp⋅p (λx y⋅y)) x))) ((λx y f⋅f x y) (λf x⋅x) (λf
x⋅x)))) n)))) (λf x⋅f (f (f x)))

step 1:
(λx⋅(λf' n⋅(λp x y⋅p x y) ((λn⋅n (λx x y⋅y) (λx y⋅x)) n) (λf x⋅f x) ((λx y⋅y ((λm n f⋅(λf g x⋅f (g x)) (m f)
(n f)) x) (λf x⋅x)) n (f' ((λn⋅(λp⋅p (λx y⋅x)) (n (λx⋅(λx y f⋅f x y) ((λp⋅p (λx y⋅y)) x) ((λm n f⋅(λf g x⋅f
(g x)) (m f) (n f)) (λf x⋅f x) ((λp⋅p (λx y⋅y)) x))) ((λx y f⋅f x y) (λf x⋅x) (λf x⋅x)))) n)))) (x x))
(λx⋅(λf' n⋅(λp x y⋅p x y) ((λn⋅n (λx x y⋅y) (λx y⋅x)) n) (λf x⋅f x) ((λx y⋅y ((λm n f⋅(λf g x⋅f (g x)) (m f)
(n f)) x) (λf x⋅x)) n (f' ((λn⋅(λp⋅p (λx y⋅x)) (n (λx⋅(λx y f⋅f x y) ((λp⋅p (λx y⋅y)) x) ((λm n f⋅(λf g x⋅f
(g x)) (m f) (n f)) (λf x⋅f x) ((λp⋅p (λx y⋅y)) x))) ((λx y f⋅f x y) (λf x⋅x) (λf x⋅x)))) n)))) (x x)) (λf
x⋅f (f (f x)))

step 2:
(λf' n⋅(λp x y⋅p x y) ((λn⋅n (λx x y⋅y) (λx y⋅x)) n) (λf x⋅f x) ((λx y⋅y ((λm n f⋅(λf g x⋅f (g x)) (m f) (n
f)) x) (λf x⋅x)) n (f' ((λn⋅(λp⋅p (λx y⋅x)) (n (λx⋅(

etc
````

The file [`core.txt`](../scripts/core.txt) (which was used in the above example) contains some standard lambda calculus encodings for logic, lists and arithmetic. It's based on material from the [Futurelearn Haskell mooc](https://www.futurelearn.com/courses/functional-programming-haskell) and the wikipedia page on [lambda calculus] (https://en.wikipedia.org/wiki/Lambda_calculus).

Note that it's important to load files in the right order because expressions may only refer to previously defined epressions in their bodies. Furthermore no checks are made when files are loaded - it is only when a calculation is attempted that unresolved references are discovered.

Calculations may not terminate, in which case the program must be stopped with `ctl'c`. This will always happen if you attempt to evaluate a bare recursive function (i.e. without supplying its arguments) So:
````haskell
evalInt Factorial N3                             // this is OK

eval Factorial                                   // this won't terminate
````
Even if calculation terminates it may not do so in a reasonable amount of time. Doing `eval Factorial N5` should return quickly, but `evalAll Factorial N5` might take a while. Use only small numbers in calculations, and stick to short lists.