# *Y = λf.(λx.f(x x)) (λx.f(x x))*
This repo contains my experiments with untyped lambda calculus implemented in Haskell. I got the idea from the [Futurelearn Haskell mooc](https://www.futurelearn.com/courses/functional-programming-haskell), which I did a while back. It's all implemented from first principles using only stuff available from the Haskell prelude.

There are some notes on the implementation [here](./docs/Overview.md)

## Installation
1. install [stack](https://docs.haskellstack.org/en/stable/README/) - the haskell build tool
2. ensure stack is up-to-date: 
````
$ stack upgrade
````
3. clone this repo:
````
$ git clone https://github.com/BillyBadBoy/lambda.git
````
4. cd into the newly created directory: 
````
$ cd lambda
````
5. compile the executable (`lam-exe`): 
````
$ stack install
````

## Running the repl
Once the installation is complete you can run a repl session.

Start the repl:
````haskell
~/haskell/lambda $ lam-exe
Enter cmd:
````

Load some basic definitions:

````haskell
Enter cmd:
loadDefs ./scripts/core.txt
> Defintions added to environment.
Enter cmd:
eval True
> Expression evaluates to:
λx y⋅x
````

Evaluate `Not True`:

````haskell
Enter cmd:
evalBoolAll Not True
> Expression evaluates to:
step 0:
(λx⋅(λp x y⋅p x y) x (λx y⋅y) (λx y⋅x)) (λx y⋅x)
step 1:
(λp x y⋅p x y) (λx y⋅x) (λx y⋅y) (λx y⋅x)
step 2:
(λx y⋅(λx y⋅x) x y) (λx y⋅y) (λx y⋅x)
step 3:
(λy⋅(λx y⋅x) (λx y⋅y) y) (λx y⋅x)
step 4:
(λx y⋅x) (λx y⋅y) (λx y⋅x)
step 5:
(λy x y⋅y) (λx y⋅x)
step 6:
λx y⋅y
which is recognized as: False
> end.
````
Evaluate 1 + 1:

````haskell
Enter cmd:
evalIntAll Plus One One
> Expression evaluates to:
step 0:
(λm n f⋅(λf g x⋅f (g x)) (m f) (n f)) (λf x⋅f x) (λf x⋅f x)
step 1:
(λn f⋅(λf g x⋅f (g x)) ((λf x⋅f x) f) (n f)) (λf x⋅f x)
step 2:
λf⋅(λf g x⋅f (g x)) ((λf x⋅f x) f) ((λf x⋅f x) f)
step 3:
λf⋅(λg x⋅(λf x⋅f x) f (g x)) ((λf x⋅f x) f)
step 4:
λf x⋅(λf x⋅f x) f ((λf x⋅f x) f x)
step 5:
λf x⋅(λx⋅f x) ((λf x⋅f x) f x)
step 6:
λf x⋅f ((λf x⋅f x) f x)
step 7:
λf x⋅f ((λx⋅f x) x)
step 8:
λf x⋅f (f x)
which is recognized as: 2
> end.
Enter cmd:
quit
````
More information about using the repl can be found [here](./docs/Repl.md)
