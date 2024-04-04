import Tapl.Tests.Ch3 as Ch3
import Test.HUnit

allTests = TestList Ch3.tests

main :: IO ()
main = runTestTT allTests >>= print
