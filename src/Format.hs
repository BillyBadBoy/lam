module Format where

import Terms

asInt :: Λ -> Maybe Int
asInt λ = getFAndX λ >>= countFs where
    getFAndX (Λ f (Λ x λ)) = Just (f, x, λ)
    getFAndX _ = Nothing
    countFs (f, x, A (V f') λ) | f == f' = (+1) <$> countFs (f, x, λ)
                               | otherwise = Nothing
    countFs (f, x, V x') | x == x' = return 0
                         | otherwise = Nothing
    countFs _ = Nothing

asBool :: Λ -> Maybe Bool
asBool λ =
    let
      true  = Λ "x" (Λ "y" (V "x"))
      false = Λ "x" (Λ "y" (V "y"))
    in
      if λ ≡ true  then Just True  else
      if λ ≡ false then Just False else
      Nothing
