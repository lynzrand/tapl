module Tapl.Tests.Ch3 where

import Tapl.Ch3
import Test.HUnit

test1 =
  TestCase $
    assertEqual
      "Simple if"
      (fulleval (TIf TTrue TTrue TFalse))
      (Ok TTrue)

test2 =
  TestCase $
    assertEqual
      "IfZero"
      (fulleval (TIf (TIsZero TZero) TTrue TFalse))
      (Ok TTrue)

test3 =
  TestCase $
    assertEqual
      "Type mismatch"
      (fulleval (TIf (TIsZero TFalse) TTrue TFalse))
      (Stuck (TIf (TIsZero TFalse) TTrue TFalse))

tests =
  [ TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3
  ]
