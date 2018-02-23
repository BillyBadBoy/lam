# Simple Recursion
##### Implementation notes

A named expression is simply recursive if its body includes references to the expression itself. For example a named expression `Fact` for the factorial function might be defined as:
````haskell
# assume IfThenElse, IsZero, One, Mult & Pred already defined

Fact = λ n . IfThenElse (IsZero n) One (Mult n (Fact (Pred n)))
                                                ^^^^
````
This is simply recursive because `Fact`'s body includes a reference to `Fact`. To convert this to a raw lambda expression each reference (`IfThenElse`, `IsZero` etc) is replaced by its definition. This is straightforward except for `Fact`because its definition includes a reference to `Fact`. We would therefore be stuck in a loop - forever replacing a reference to `Fact` by its defintion, only to discover that we have introduced yet another reference to `Fact`.

The solution is to re-formulate the definition of `Fact` and use the Y-combinator.

Begin by re-writing the definition of `Fact` as 
````haskell
Fact = (λ f . λ n . IfThenElse (IsZero n) One (Mult n (f (Pred n))))  Fact
                                                                      ^^^^
````
This doesn't change anything - the reference to `Fact` has been abstracted out, but then the abstraction is applied to `Fact` so we're back where we started. (For comparison `log 2` could be re-written as `(λ f . f 2) log` without changing its meaning)

This is still recursive, of course, so we still have a problem, but notice how the recursion has been isolated in a single reference to `Fact` at the very end of the definition. This reveals an interesting property of the `Fact` function, which becomes clearer if we re-name `(λ f . λ n . IfThenElse (IsZero n) One (Mult n (f (Pred n)))))` as `g`. (Note that `g` is non-recursive - it doesn't include any references to `Fact`.) The re-naming gives us this:
````haskell
Fact = (λ f . λ n . IfThenElse (IsZero n) One (Mult n (f (Pred n))))  Fact
       ^^^^^^^^^^^^^^^^^^^^^^^^  rename as g  ^^^^^^^^^^^^^^^^^^^^^^

Fact = g Fact
````
`Fact` is revealed to be a fixed-point of the non-recursive function `g`. It turns out that we can find fixed-points of functions using the famous [Y-combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator), so that:
````haskell
Fact = fixed-point-of g = Y g
````
where `y` is the Y-combinator and `g` is as defined above. This solves our problem - we now have a definition of `Fact` i.e. `Y g` that is non-recursive.

Generalising from the above example gives this procedure for dealing with simply-recursive functions:
````haskell
# F is a simply-recursive defintion (because its body contains 1+ references to F)
# (the x's stand for any non-recusive content)
F = xxxFxxxFxxx  

# abstract the F's out of the body & apply to F
# (new variable f is chosen so as not to clash with any free variables in the body)
F = (λ f. xxxfxxxfxxx) F

# use the Y-combinator
F = Y (λ f. xxxfxxxfxxx)
````

This is implemented in [Env.hs](../src/Env.hs) in the `fix` function:
````haskell
-- γ combinator: λf.(λx.f(x x)) (λx.f(x x))
γ :: Λ'
γ = Λ' "f" $ A' fxx fxx
  where
    xx  = A' (V' "x") (V' "x")
    fxx = Λ' "x" $ A' (V' "f") xx

-- fixes a recursive formula: (..f..f..) => Y (λf'.(..f'..f'..))
fix :: Var -> Λ' -> Λ'
fix s λ = A' γ $ Λ' "f'" $ sub (V' "f'") s λ
````
In the `fix` function `s` is the name of the function e.g. `Fact` and `λ` is its body e.g. `λ n . IfThenElse (IsZero n) One (Mult n (Fact (Pred n))))`. The result is a non-recursive definition of λ.
