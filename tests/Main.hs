import Tapl.Tests.Ch3 as Ch3
import Tapl.Tests.Ch6 as Ch6
import Test.HUnit

allTests :: Test
allTests = TestList (Ch3.tests ++ Ch6.tests)

main :: IO ()
main = runTestTT allTests >>= print
