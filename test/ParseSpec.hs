module ParseSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Terms
import Parse
import Env

parseTests =
    testGroup
        "parser tests"
        [basicSyntaxTest, booleanTest, listBasicsTest, listFoldsTest,
         listMapTest, listFilterTest]

basicSyntaxTest =
    testCase "syntax basics" $ do
        assertEqual "simple let" (Just $ Λ' "x" x)
            (findΛ' (readEnv "let ID = \\x.x;") "ID")
        assertEqual "lambda symbol" (Just $ Λ' "x" x)
            (findΛ' (readEnv "let ID = λx.x;") "ID")
        assertEqual "simple let with where" (Just $ R' "X2")
            (findΛ' (readEnv "let X1 = X2 : {let X2 = x;}") "X1")
        assertEqual "where defs invisible" Nothing
            (findΛ' (readEnv "let X1 = X2 : {let X2 = x;}") "X2")
        assertEqual "search earlier defs" (Right $ V "x")
            (findΛ  (readEnv "let B = x; let A = B;") "A")
        assertEqual "search where before earlier" (Right $ V "y")
            (findΛ  (readEnv "let B = x; let A = B : {let B = y;}") "A")
        assertEqual "simple recursive expression"
            (Just $ A' γ (Λ' "f'" (A' f' x)))
            (findΛ'  (readEnv "letrec F = F x;") "F")
        assertEqual "letrec expression with where"
            (Just $ A' γ (Λ' "f'" (A' f' (R' "X"))))
            (findΛ'  (readEnv "letrec F = F X : { let X = x }") "F")
        assertEqual "let block without where"
            (Just x)
            (findΛ'  (readEnv "let {let X = x; let Y = y; }") "X")
        assertEqual "let block with where"
            (Right $ V "y")
            (findΛ   (readEnv "let {let X = Y;} : {let Y = y;}}") "X")
        where x  = V' "x"
              y  = V' "y"
              f' = V' "f'"

booleanTest =
  testCase "logical values" $ do
    e <- loadEnv "./test/prelude.txt"
    assertEqual "True  def" (Just $ Λ' "x" $ Λ' "y" (V' "x")) (findΛ' e "True")
    assertEqual "False def" (Just $ Λ' "x" $ Λ' "y" (V' "y")) (findΛ' e "False")

    assertEqual "If true  test" (V "x") (eval e "IfThenElse True  x y")
    assertEqual "If false test" (V "y") (eval e "IfThenElse False x y")

    assertBool "BoolEq T T" (eval e "BoolEq True  True"  ≡ eval e "True")
    assertBool "BoolEq T F" (eval e "BoolEq True  False" ≡ eval e "False")
    assertBool "BoolEq F T" (eval e "BoolEq False True"  ≡ eval e "False")

    assertBool "BoolEq F F" (eval e "BoolEq False False" ≡ eval e "True")

listBasicsTest =
  testCase "list primitives" $ do
    e <- loadEnv "./test/prelude.txt"
    assertBool "Nil is Null"
        (eval e "Null Nil" ≡ eval e "True")
    assertBool "Pair isn't Null"
        (eval e "Null (Pair True Nil)" ≡ eval e "False")
    assertBool "First gets head"
        (eval e "First  (Pair a (Pair b Nil))" ≡ eval e "a")
    assertBool "Second gets tail"
        (eval e "First (Second (Pair a (Pair b Nil)))" ≡ eval e "b")


listFoldsTest =
  testCase "list folds" $ do
    e <- loadEnv "./test/prelude.txt"
    assertBool "Foldr Pair is identity"
        (eval e "Foldr Pair Nil (Pair a Nil)" ≡ eval e "Pair a Nil")
    assertBool "Foldr True  takes first"
        (eval e "Foldr True Nil  (Pair a (Pair b Nil))" ≡ eval e "a")
    assertBool "Foldr False takes final"
        (eval e "Foldr False c   (Pair a (Pair b Nil))" ≡ eval e "c")

listMapTest =
  testCase "list map" $ do
    e <- loadEnv "./test/prelude.txt"
    assertBool "Map Not over [Bool]"
        (eval e "Map Not (Pair False Nil)" ≡ eval e "(Pair True Nil)")
    assertBool "Map (Const c) over list"
        (eval e "Map (Const c)(Pair a (Pair b Nil))" ≡ eval e "(Pair c (Pair c Nil))")

listFilterTest =
  testCase "list filtering" $ do
    e <- loadEnv "./test/prelude.txt"
    assertBool "Filter [Bool] using Id"
        (eval e "Filter Id (Pair False Nil)" ≡ eval e "Nil")
    assertBool "Filter [Bool] using Not"
        (eval e "Filter Not (Pair False Nil)" ≡ eval e "(Pair False Nil)")

eval :: Γ -> String -> Λ
eval e s = last $ βnf lambda
    where (Right lambda) = deref e (fst $ head $ parse formulaP s)

readEnv :: String -> Γ
readEnv s = fst $ head $ parse envP s

loadEnv :: String -> IO Γ
loadEnv s = readEnv <$> readFile s
