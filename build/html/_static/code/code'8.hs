import Test.HUnit  
import Control.Exception

frac :: Int -> Int 
frac n  
    | n == 0 = 1 
    | n > 0 = n * frac (n - 1)
    | otherwise = undefined

borderTest :: Test
borderTest = TestCase $ 
    assertEqual "0! /= 1" (frac 0) 1

commonTest :: Test
commonTest = TestList $ TestCase <$> [
    assertEqual "1! /= 1" 1 (frac 1),
    assertEqual "2! /= 2" 2 (frac 2),
    assertEqual "3! /= 6" 6 (frac 3),
    assertEqual "4! /= 24" 24 (frac 4),
    assertEqual "5! /= 120" 120 (frac 5)]

undefinedTest :: Test 
undefinedTest = TestCase $ do 
    result <- try (evaluate (frac (-1))) :: IO (Either SomeException Int)
    case result of 
        Left _ -> return ()
        Right _ -> assertFailure "Tests beyond border pass"


testFrac :: Test
testFrac = TestLabel "testFrac" $ 
    TestList [
        TestLabel "borderTest" borderTest,
        TestLabel "commonTest" commonTest,
        TestLabel "undefinedTest" undefinedTest
    ]


main :: IO Counts
main = runTestTT testFrac 