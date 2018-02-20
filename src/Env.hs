module Env where

import Terms

----------------------------- formula data type --------------------------------

data Λ' = V' Var | A' Λ' Λ' | Λ' Var Λ' | R' Var deriving (Eq)

instance Show Λ' where
  show = shBare where

    shBare :: Λ' -> String              -- show without brackets
    shBare (R' x) = "<" ++ x ++ ">"
    shBare (V' x) = x
    shBare (A' m n) = shAppl m ++ " " ++ shBrac n
    shBare (Λ' x m) = "λ" ++ x ++ shBody m

    shBrac :: Λ' -> String              -- show within brackets
    shBrac (R' x) = "<" ++ x ++ ">"
    shBrac (V' x) = x
    shBrac m = "(" ++ shBare m ++ ")"

    shAppl :: Λ' -> String
    shAppl m@(A' _ _) = shBare m        -- left-associativity of application
    shAppl m = shBrac m

    shBody :: Λ' -> String
    shBody (Λ' x m) = x ++ shBody m     -- right-associativity of abstraction
    shBody m = "⋅" ++ shBare m

--------------------------- recursion utilities --------------------------------

-- γ combinator: λf.(λx.f(x x)) (λx.f(x x))
γ :: Λ'
γ = Λ' "f" $ A' fxx fxx
  where
    xx  = A' (V' "x") (V' "x")
    fxx = Λ' "x" $ A' (V' "f") xx

-- fixes a recursive formula: (..f..f..) => Y (λf'.(..f'..f'..))
fix :: Var -> Λ' -> Λ'
fix s λ = A' γ $ Λ' "f'" $ sub (V' "f'") s λ

-- substitute formula for all occurrences of ref in formula
sub :: Λ' -> Var -> Λ' -> Λ'
sub s r = go where
    go (R' x) | r == x    = s
              | otherwise = R' x
    go (V' x) = V' x
    go (A' m n) = A' (go m) (go n)
    go (Λ' x m) = Λ' x (go m)

-------------------------- assignments/environments ----------------------------

-- A Γ is basically a linked list of assignments
-- An assignment is a name-formula association.
-- The 2nd Γ in the constructor is private definitions used in the assignment
data Γ = Nil | Γ Var Λ' Γ Γ deriving Show

-- concatenate environments
(+:) :: Γ -> Γ -> Γ
(+:) Nil e = e
(+:) (Γ v f e1 e2) e3 = Γ v f (e1 +: e3) e2

-- retrieve a formula by name from the env
findΛ' :: Γ -> Var -> Maybe Λ'
findΛ' Nil _ = Nothing
findΛ' (Γ w f e _) v | v == w = Just f
                     | otherwise = findΛ' e v

-- retrieve a formula by name from the env and convert to a λ
findΛ :: Γ -> Var -> Either String Λ
findΛ Nil v = Left $ "Could not resolve reference: " ++ v 
findΛ (Γ w f e e') v | v == w = deref (e' +: e) f
                     | otherwise = findΛ e v

deref :: Γ -> Λ' -> Either String Λ
deref e (R' v) = findΛ e v
deref e (V' v) = Right $ V v
deref e (A' m n) = do m' <- deref e m
                      n' <- deref e n
                      return $ A m' n'
deref e (Λ' x m) = do m' <- deref e m
                      return $ Λ x m'
