module TermsSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Terms

termsTests =
    testGroup
        "λ term tests"
        [printingTermsTest, alphaEquivalenceTest, betaReductionTest]

printingTermsTest =
    testCase "show instance" $ do
        -- simple cases
        assertEqual "variable"    "x"     (show $ V "x")
        assertEqual "abstraction" "λx⋅x"  (show $ Λ "x" $ V "x")
        assertEqual "application" "x y"   (show $ A (V "x") (V "y"))
        -- associativity
        assertEqual "abstraction associativity"
            "λx y z⋅a"  (show $ Λ "x" $ Λ "y" $ Λ "z" $ V "a")
        assertEqual "application associativity"
            "a b c"     (show $ A (A (V "a") (V "b")) (V "c"))
        assertEqual "application associativity"
            "a (b c)"   (show $ A (V "a") (A (V "b") (V "c")))

alphaEquivalenceTest =
    testCase "alpha equivalence" $ do
        assertBool "equal terms"         $ V "x" ≡ V "x"
        assertBool "unequal terms" $ not $ V "x" ≡ V "y"
        assertBool "abstraction"   $ Λ "x" (V "x") ≡ Λ "y" (V "y")

betaReductionTest =
    testCase "beta reduction" $ do
        assertEqual "term" Nothing $ β x
        assertEqual "abstraction"
            (Just y) (β $ A id y)
        assertEqual "abstraction"
            (Just y) (β $ A id y)
        assertEqual "protect fv's"
            (Just $ Λ "v0" y) (β $ A (Λ "x" $ Λ "y" x) y)
        assertEqual "leftmost redux"
            (Just $ A y (A id y)) (β $ A (A id y) (A id y))
        assertEqual "leftmost redux"
            (Just $ A (A id y) (A id y)) (β $ A xx (A id y))
            where id = Λ "x" x
                  xx = Λ "x" (A x x)
                  x  = V "x"
                  y  = V "y"
