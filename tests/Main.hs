import Tapl.Tests.Ch3 as Ch3
import Tapl.Tests.Ch6 as Ch6
import Tapl.Tests.Ch8 as Ch8
import Test.HUnit

allTests :: Test
allTests = TestList (Ch3.tests ++ Ch6.tests ++ Ch8.tests)

main :: IO ()
main = runTestTT allTests >>= print
