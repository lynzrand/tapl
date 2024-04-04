module Tapl.Tests.Ch3 where

import Tapl.Ch3
import Test.HUnit

test1 :: Test
test1 =
  TestCase $
    assertEqual
      "Simple if"
      (smalleval (TIf TTrue TTrue TFalse))
      (Ok TTrue)

test2 :: Test
test2 =
  TestCase $
    assertEqual
      "IfZero"
      (smalleval (TIf (TIsZero TZero) TTrue TFalse))
      (Ok TTrue)

test3 :: Test
test3 =
  TestCase $
    assertEqual
      "Type mismatch"
      (smalleval (TIf (TIsZero TFalse) TTrue TFalse))
      (Stuck (TIf (TIsZero TFalse) TTrue TFalse))

bigtest :: Term -> Test
bigtest t =
  TestCase $ assertEqual "big and small coincides" (bigeval t) (smalleval t)

bigtest0 :: Test
bigtest0 = bigtest (TIf TTrue TTrue TFalse)

bigtest1 :: Test
bigtest1 = bigtest (TIf (TIsZero TZero) TTrue TFalse)

bigtest2 :: Test
bigtest2 = bigtest (TIf (TIsZero TFalse) TTrue TFalse)

tests :: [Test]
tests =
  [ TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "bigtest0" bigtest0,
    TestLabel "bigtest1" bigtest1,
    TestLabel "bigtest2" bigtest2
  ]
