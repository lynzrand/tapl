module Tapl.Tests.Ch6 where

import Tapl.Ch6
import Test.HUnit

assertUnnamedEqual :: Maybe UTerm -> Assertion
assertUnnamedEqual term =
  assertEqual
    "Unnamed term equals"
    term
    (term >>= restoreNames >>= removeNames)

assertTerms :: [Test]
assertTerms =
  map
    (TestCase . assertUnnamedEqual . removeNames)
    [ tru,
      fls,
      tand,
      tor,
      scc,
      scc',
      lone,
      lzero,
      churchNum 10
    ]

tests :: [Test]
tests = assertTerms
