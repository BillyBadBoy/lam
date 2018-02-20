module Terms (Λ (..), Var, (≡), β,  βnf, vars, newVar) where

---------------------------------- λ-terms -------------------------------------

data Λ = V Var | A Λ Λ | Λ Var Λ deriving (Eq)

instance Show Λ where
  show = shBare where

    shBare :: Λ -> String                  -- show without brackets
    shBare (V x) = x
    shBare (A m n) = shAppl m ++ " " ++ shBrac n
    shBare (Λ x m) = "λ" ++ x ++ shBody m

    shBrac :: Λ -> String                  -- show within brackets
    shBrac (V x) = x
    shBrac m = "(" ++ shBare m ++ ")"

    shAppl :: Λ -> String
    shAppl m@(A _ _) = shBare m            -- left-associativity of application
    shAppl m = shBrac m

    shBody :: Λ -> String
    shBody (Λ x m) = " " ++ x ++ shBody m  -- right-associativity of abstraction
    shBody m = "⋅" ++ shBare m

-------------------------------- substitution ----------------------------------

-- substitute λ n for free x in another λ
σ :: Λ -> Var -> Λ -> Λ
σ n x (V a)
    | x == a    = n                  -- [N/x] x  = N
    | otherwise = V a                -- [N/x] a  = a
σ n x (A p q) = A p' q'              -- [N/x] PQ = ([N/x] P) ([N/x] Q)
  where
    p' = σ n x p
    q' = σ n x q
σ n x m@(Λ y p)
    | x == y = m                     -- [N/x] λx.P = λx.P
    | x ∉ fp = m                     -- [N/x] λy.P = λy.P      (x not free in P)
    | y ∉ fn    = Λ y  $ σ n x p     -- [N/x] λy.P = λy.[N/x]P (y not free in N)
    | otherwise = Λ y' $ σ n x p'    -- [N/x] λy.P = λz.[N/x][z/y]P    (fresh z)
  where
    fp = fvars p
    fn = fvars n
    y' = newVar $ fn ++ fp
    p' = σ (V y') y p

-- check if 2 λs are α-equivalent
(≡) :: Λ -> Λ -> Bool
(V   x) ≡ (V   y) = x == y               -- x  ≡ y    ↔ x = y
(A p q) ≡ (A r s) = (p ≡ r) && (q ≡ s)   -- PQ ≡ RS   ↔ P ≡ R & Q ≡ S
(Λ x p) ≡ (Λ y q)
    | x == y    = p  ≡  q                -- λxP ≡ λxQ ↔ P  ≡ Q
    | otherwise = p' ≡  q'               -- λxP ≡ λyQ ↔ P' ≡ Q' where
  where                                  -- P'=[z/x]P, Q'=[z/y]Q
    z  = newVar $ fvars p ++ fvars q
    p' = σ (V z) x p
    q' = σ (V z) y q
_ ≡ _  = False

-- perform single left-most β reduction on λ
β :: Λ -> Maybe Λ
β (A (Λ x p) q) = Just $ σ q x p          -- β reduction
β (V   x) = Nothing                       -- terms cannot contain redux
β (Λ x p) = Λ x <$> β p                   -- reduce abstraction by reducing body
β (A p q) = case β p of
    Just p' -> Just $ A p' q              -- redux found in left-hand term
    Nothing -> case β q of
        Just q' -> Just $ A p q'          -- redux found in right-hand term
        Nothing -> Nothing                -- no redux in either left or right

-- perform multiple left-most β reductions on λ
βnf :: Λ -> [Λ]
βnf λ = λ : case β λ of
    Just λ' -> βnf λ'
    Nothing -> []

---------------------------------- variables -----------------------------------

type Var = String

-- find free variables in λ-term
fvars :: Λ -> [Var]
fvars = f [] [] where
  -- f :: bound -> free -> λ -> free
  f bs fs (Λ x m) = f (bs ?+ x) fs m    -- add free vars from m with x bound
  f bs fs (A m n) = f bs (f bs fs m) n  -- add free vars from m & n
  f bs fs (V x) | x ∈ bs    = fs        -- x not free since within λx binding
                | otherwise = fs ?+ x   -- x free since not within λx binding

-- find variables in list of λ-terms
vars :: [Λ] -> [Var]
vars  = foldl f [] where
  f :: [Var] -> Λ -> [Var]
  f vs (V   x) = vs ?+ x
  f vs (A m n) = f (f vs m)  n
  f vs (Λ x m) = f (vs ?+ x) m

-- pick new variable not in given list of (free) variables
newVar :: [Var] -> Var
newVar fvs =
  let vs  = ['v':show i | i <- [0..]]   -- vars: v0, v1, v2, etc
  in  head [ v | v <- vs, v ∉ fvs ]     -- first not in list of free vars

------------------------------------ util  -------------------------------------
(∈) :: Eq t => t -> [t] -> Bool
(∈) = elem

(∉) :: Eq t => t -> [t] -> Bool
(∉) = (not .) . (∈)

-- add item to list if not already present
(?+) :: Eq t => [t] -> t -> [t]
(?+) xs x | x ∈ xs    =   xs
          | otherwise = x:xs
