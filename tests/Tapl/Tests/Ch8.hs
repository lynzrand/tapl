module Tapl.Tests.Ch8 where

import Tapl.Ch8
import Test.HUnit

testTypes :: [(Term, Maybe Ty)]
testTypes =
  [ -- true
    ( TBool True,
      Just TyBool
    ),
    -- \x:Bool . x
    ( TLambda "x" TyBool (TVar "x"),
      Just (TyBool ->> TyBool)
    ),
    -- if true then (\x:Bool . x) else (\x:Bool . not x)
    ( TIf
        (TBool True)
        (TLambda "x" TyBool (TVar "x"))
        (TLambda "x" TyBool (TApply (TVar "not") (TVar "x"))),
      Just (TyBool ->> TyBool)
    ),
    -- \x:(Bool -> Bool) . \y:Bool if x true then y else not y
    ( TLambda
        "x"
        (TyBool ->> TyBool)
        ( TLambda
            "y"
            TyBool
            ( TIf
                (TApply (TVar "x") (TBool True))
                (TVar "y")
                (TNot (TVar "y"))
            )
        ),
      Just ((TyBool ->> TyBool) ->> TyBool ->> TyBool)
    ),
    -- Let's give it a failing example
    -- \x:Bool . x x
    ( TLambda "x" TyBool (TApply (TVar "x") (TVar "x")),
      Nothing
    )
  ]

runTyping :: [(Term, Maybe Ty)] -> [Test]
runTyping = map runTyping'
  where
    env = [("not", TyBool ->> TyBool)]
    runTyping' (term, expected) = TestCase $ assertEqual "" expected (getType env term)

typingTests :: [Test]
typingTests = runTyping testTypes

tests :: [Test]
tests = typingTests
