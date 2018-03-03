# Mutual Recursion
##### Implementation notes

Simple recursion occurs when the definition of a named expression includes references to the expression itself - the expression depends on itself. By contrast, mutual recursion is where 2 or more named expressions form a dependency cycle. For example, these definitions of `IsEven` and `IsOdd` are mutually dependent:
````haskell
# assume IsZero, True, False & Pred already defined

IsEven = \ n . (IsZero n) True  (IsOdd  (Pred n));
                                 ^^^^^   

IsOdd  = \ n . (IsZero n) False (IsEven (Pred n));
                                 ^^^^^^
````
`IsEven` depends on `IsOdd` which depends on `IsEven` which depends on... etc

We would like to re-use the approach taken with simple recursion - but that only works for a single self-referencing definition. Therefore we must somehow combine all the mutually recursive definitions (2 in the above example) into a single simply-recursive definition. Furthermore, the original functions (`IsEven` and `IsOdd`) must still be recoverable from the combined function. This process is described below after a quick note about combining things using lambda calculus.

-----
*Aside: combining arbitrary items*

*Any list of items, e.g. `a`, `b` and `c`, can be combined into a single expression like this: `λ f . f a b c`. Note that the `f` parameter is a function, so this is a higher-order function. This can be considered as a crude list. The list elements can be recovered by applying the list to an accessor function. For example `(λ x y z . y)` accesses the middle item from a 3 item list because it reduces like this:*
````haskell
 =====list====  ==accessor==   
(λ f ⋅ f a b c) (λ x y z ⋅ y)
(λ x y z ⋅ y) a b c
(λ y z ⋅ y) b c
(λ z ⋅ b) c
b
````
*These are all the accessors for a 3 item list:*
````haskell
 =====list====  ==accessor==   
(λ f . f a b c) (λ x y z . x) = a         // acessing the 1st item of list [a, b, c]
(λ f . f a b c) (λ x y z . y) = b         // acessing the 2nd item of list [a, b, c]
(λ f . f a b c) (λ x y z . z) = c         // acessing the 3rd item of list [a, b, c]
````
*Thus we can easily combine a list of expressions into a single expression and recover them later using an appropriate accessor. Obviously this approach can be applied to lists of any fixed finite length*

-----
Returning to our problem, begin by combining the definitions of `IsEven` and `IsOdd` into a 2 element list called `F`:
````haskell
F = λ f . f (λn.(IsZero n) True (IsOdd (Pred n)))  (λn.(IsZero n) False (IsEven (Pred n)))
             ^^^^^^^^^^^^^^IsEven^^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^IsOdd^^^^^^^^^^^^^^^^^^
````
The 1st element of `F` is the definition of`IsEven` and the 2nd element of `F` is the definition of `IsOdd`. As noted in the aside above, the elements of list `F` can be retrieved using accessors, giving:
````text
IsEven = F (λ x y . x)      // 1st element of F
IsOdd  = F (λ x y . y)      // 2nd element of F
````
These are definitions of `IsEven` and `IsOdd` in terms of `F`, which we can substitute back into the definition of `F` (to remove the explicit references to `IsEven` and `IsOdd`) :
````haskell
F = λ f . f (λn.(IsZero n) True (F (λ x y . y) (Pred n))) (λn.(IsZero n) False (F (λ x y . x) (Pred n)))
                                 ^^^^^^^^^^^^^                                  ^^^^^^^^^^^^^
````
Notice that there are no references to `IsEven` or `IsOdd` in this equation. This is a definition of `F` that includes references to `F` in its body - i.e. this is an example of simple recursion.

Now we produce a non-recursive definition of `F` by following the procedure for [simple recursion](./SimpleRecursion.md) - let's call the result `F'`. The final step is to re-use the equations above for `IsEven` and `IsOdd` (but with `F'` instead of the equivalent F`) :
````haskell
IsEven = F' (λ x y . x)
IsOdd  = F' (λ x y . y)
````
The process is complete: we started with mutually recursive definitions of `IsEven` and `IsOdd` and finished with non-recursive definitions. Generalising from the above example gives this procedure for dealing with mutually-recursive functions:
````haskell
# start with n mutualy recursive named expressions:
# each body term stands for an expression that may include mutiple references to E1 .. En
E1 = bodyE1
E2 = bodyE2
 :     :
En = bodyEn

# combine into a single expression:
F = λ f . f (bodyE1) (bodyE2) ... (bodyEn)

# replace every reference Ei in the right-hand side bodies with: F (λ x1 x2 .. xn . xi)
# call the transformed bodies: bodyE1' bodyE2' ... bodyEn', giving:
F = λ f . f (bodyE1') (bodyE2') ... (bodyEn')

# F is simply-recursive and can be re-defined non-recursively as, say, F'

# finally define:
E1 = F' (λ x1 x2 ... xn . x1)
E2 = F' (λ x1 x2 ... xn . x2)
 :     :
En = F' (λ x1 x2 ... xn . xn)
````
