module Parse where

import Terms
import Env

import Debug.Trace

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance Monad Parser where
  return a  = Parser $ \s -> [(a, s)]
  (>>=) p f = Parser $ \s -> [ e | (a, t) <- parse p s, e <- parse (f a) t ]

instance Functor Parser where
  fmap f p = p >>= (return . f)

instance Applicative Parser where
  pure = return
  f <*> p = f >>= (`fmap` p)

--------------------------------------------------------------------------------

failureP :: Parser a
failureP = Parser $ const []

orP :: Parser a -> Parser a -> Parser a
orP p1 p2 = Parser $ \s ->
  let a1 = parse p1 s
      a2 = parse p2 s
  in  if null a1 then a2 else a1

manyP ::  Parser a -> Parser [a]
manyP p = someP p `orP` return []

someP :: Parser a -> Parser [a]
someP p = do a  <- p
             as <- manyP p
             return $ a : as

anyCharP :: Parser Char
anyCharP = Parser f where
  f "" = []
  f (c:cs) = [(c, cs)]

predP :: Parser a -> (a -> Bool) -> Parser a
predP p f = Parser $ \s -> [ (a, s') | (a, s') <- parse p s, f a]

charP :: Char -> Parser Char
charP c = predP anyCharP (== c)

stringP :: String -> Parser ()
stringP k = Parser $ \s -> [ ((), drop (length k) s) | k == take (length k) s ]

spaceP :: Parser ()
spaceP = do manyP $ predP anyCharP (`elem` [' ','\t','\n'])
            return ()

varCharP :: Parser Char
varCharP = predP anyCharP (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

upperCharP :: Parser Char
upperCharP = predP anyCharP (`elem` ['A'..'Z'])

lowerCharP :: Parser Char
lowerCharP = predP anyCharP (`elem` ['a'..'z'])

upperName :: Parser String
upperName = do c  <- upperCharP
               cs <- manyP varCharP
               return $ c : cs

lowerName :: Parser String
lowerName = do c  <- lowerCharP
               cs <- manyP varCharP
               return $ c : cs

spacedP :: Parser a -> Parser a
spacedP p = do
    _ <- spaceP
    a <- p
    _ <- spaceP
    return a

bracedP :: Parser a -> Parser a
bracedP p = do
    _ <- spacedP $ charP '('
    a <- p `orP` bracedP p
    _ <- spacedP $ charP ')'
    return a

bracketedP :: Parser a -> Parser a
bracketedP p = do
    _ <- spacedP $ charP '{'
    a <- p
    _ <- spacedP $ charP '}'
    return a

spacedOrBraced :: Parser a -> Parser a
spacedOrBraced p = spacedP p `orP` bracedP p

--------------------------------------------------------------------------------

formulaP :: Parser Λ'
formulaP = appP `orP` varP `orP` refP `orP` absP

argP :: Parser Var
argP = spacedP lowerName

varP :: Parser Λ'
varP = V' <$> spacedOrBraced lowerName

refP :: Parser Λ'
refP = R' <$> spacedOrBraced upperName

lhsP :: Parser Var
lhsP = spacedP upperName

appP :: Parser Λ'
appP = foldl1 A' <$> someP
    (varP `orP` refP `orP` bracedP appP `orP` bracedP absP)

absP :: Parser Λ'
absP = do
    _    <- spacedP $ charP 'λ' `orP` charP '\\'
    args <- someP argP
    _    <- spacedP $ charP '.'

    body <- formulaP
    return $ foldr Λ' body args

--------------------------------------------------------------------------------

type EnvItem = (Var, Λ', Γ)
type BlockItem = (Var, Λ')

envP :: Parser Γ
envP = do
    is <- manyP envItemP
    return $ join is

envItemP :: Parser [EnvItem]
envItemP = letExpP `orP` letrecExpP `orP` letBlockP `orP` letrecBlockP `orP` commentP

commentP :: Parser [EnvItem]
commentP = do
    _ <- spaceP
    _ <- charP '#'
    _ <- manyP $ predP anyCharP (/= '\n')
    _ <- charP '\n'
    return []

itemAndWhereP :: Parser (Var, Λ', Γ)
itemAndWhereP = do
    v <- lhsP
    _ <- spacedP $ stringP "="
    f <- formulaP
    e <- whereP
    return (v, f, e)

letExpP :: Parser [EnvItem]
letExpP = do
    _ <- spacedP $ stringP "let "
    (v, f, e) <- itemAndWhereP
    return [(v, f, e)]

letrecExpP :: Parser [EnvItem]
letrecExpP = do
    _ <- spacedP $ stringP "letrec "
    (v, f, e) <-  itemAndWhereP
    return [(v, fix v f, e)]

letrecBlockP :: Parser [EnvItem]
letrecBlockP = do
    _  <- spacedP $ stringP "letrec"
    (bs, e) <- blockItemsAndWhereP
    return $ fixAll (bs, e)

letBlockP :: Parser [EnvItem]
letBlockP = do
    _  <- spacedP $ stringP "let"
    (bs, e) <- blockItemsAndWhereP
    return [ (v,f,e) | (v,f) <- bs ]

blockItemsAndWhereP :: Parser ([BlockItem], Γ)
blockItemsAndWhereP = do
    bs <- bracketedP $ manyP blockItemP
    e  <- whereP
    return (bs, e)

fixAll :: ([BlockItem], Γ) -> [EnvItem]
fixAll (bs, e) = defs where
    defs :: [EnvItem]
    defs = map go vGetter

    go :: (Var, Λ') -> EnvItem
    go (v, g) = (v, A' unifiedFn g, e)

    unifiedFn :: Λ'
    unifiedFn = ( fix "F'" . substFGetters) fnList

    fnList :: Λ'
    fnList = Λ' "i'" (foldl A' (V' "i'") (snd <$> bs))

    -- replace references to 'F3' with (A' F' get3rd)
    substFGetters :: Λ' -> Λ'
    substFGetters formula = foldr f formula vGetter where
        f :: (Var, Λ') -> Λ' -> Λ'
        f (v, g) = sub (A' (R' "F'") g) v

    -- [ ('F0',getOth), ('F1',get1st), .., ('Fn',getNth) ]
    vGetter :: [(Var, Λ')]
    vGetter = [ (v, getter i) | ((v, _), i) <- zip bs [0..] ]

    -- builds acessor for ith element: λ v0 v1 … vn . vi
    getter :: Int -> Λ'
    getter i = foldr Λ' v_last v_list
      where v_last = V' ('v' : show i)
            v_list = [   'v' : show j | j <- [0 .. length bs - 1] ]

blockItemP :: Parser BlockItem
blockItemP = do
    _ <- spacedP $ stringP "let "
    v <- lhsP
    _ <- spacedP $ charP '='
    f <- formulaP
    _ <- spacedP $ charP ';'
    return (v, f)

whereP :: Parser Γ
whereP = (spacedP (charP ';') >> return Nil)
          `orP`
         (spacedP (charP ':') >> bracketedP envP)
          `orP`
          return Nil

join :: [[EnvItem]] -> Γ
join is = foldr (\(v, f, e2) e1 -> Γ v f e1 e2) Nil (reverse $ concat is)
