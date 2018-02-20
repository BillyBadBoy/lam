module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TermsSpec
import ParseSpec

main :: IO ()
main = defaultMain $ testGroup
    "parser tests"
    [termsTests, parseTests]
