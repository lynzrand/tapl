module Tapl.Tests.Ch6 where

import Data.Bimap qualified as Bimap
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

testGlobalEnv :: NamedEnv
testGlobalEnv = Bimap.fromList [("a", 0), ("b", 1)]

testRemoveNames :: Term -> Maybe UTerm
testRemoveNames = removeNamesImpl testGlobalEnv

-- Subst

data SubstEnv = SubstEnv
  { subst :: String,
    with :: Term,
    using :: Term,
    expected :: Term
  }

substTestCases :: [SubstEnv]
substTestCases =
  [ SubstEnv
      { subst = "b",
        with = "a",
        using = ["b", l ["x", "y"] "b"],
        expected = ["a", l ["x", "y"] "a"]
      },
    SubstEnv
      { subst = "b",
        with = ["a", l ["z"] "a"],
        using = ["b", l ["x"] "b"],
        expected = [["a", l ["z"] "a"], l ["x"] ["a", l ["z"] "a"]]
      },
    SubstEnv
      { subst = "b",
        with = "a",
        using = l ["b"] ["b", "a"],
        expected = l ["b"] ["b", "a"]
      },
    SubstEnv
      { subst = "b",
        with = "a",
        using = l ["a"] ["b", "a"],
        expected = l ["a'"] ["a", "a'"]
      }
  ]

runSubst :: SubstEnv -> Assertion
runSubst (SubstEnv {subst = substitute, with, using, expected}) =
  let res = do
        -- remove names for all terms
        substId <- getVarName substitute testGlobalEnv
        withUTerm <- testRemoveNames with
        usingUTerm <- testRemoveNames using
        let substResult = subst substId withUTerm usingUTerm
        expectedUTerm <- testRemoveNames expected
        Just (substResult, expectedUTerm)
   in case res of
        Nothing -> assertBool "Failed" False
        (Just (x, y)) -> assertEqual "Substitute results should match" y x

substTests :: [Test]
substTests = map (TestCase . runSubst) substTestCases

-- Eval

type EvalCase = (Term, Term)

evalCases :: [EvalCase]
evalCases =
  [ (tru, tru),
    (fls, fls),
    ([tor, tru, fls], tru),
    ([tand, tru, fls], fls),
    ([scc, lzero], lone)
  ]

runEval :: EvalCase -> Assertion
runEval (term, expected) =
  let res = do
        term' <- testRemoveNames term
        expected' <- testRemoveNames expected
        let result = eval normalOrderStep term'
        Just (result, expected')
   in case res of
        Nothing -> assertBool "Failed" False
        (Just (x, y)) -> assertEqual "Eval results should match" y x

evalTests :: [Test]
evalTests = map (TestCase . runEval) evalCases

tests :: [Test]
tests = assertTerms ++ substTests ++ evalTests
