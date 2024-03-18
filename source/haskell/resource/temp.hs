
import Data.Time (getCurrentTime)
import Data.Time.Clock (diffUTCTime)
import Control.DeepSeq (force)
import Debug.Trace (trace)
import qualified GHC.Types

heron's_formula :: Double -> Double -> Double -> Double 
heron's_formula a b c = let p = (a + b + c)/2
                            in sqrt (p * (p - a) * (p - b) * (p - c))

helper :: Double -> Double -> Double -> Double 
helper a b c = (a + b + c) /2

heron's_formula' :: Double -> Double -> Double -> Double 
heron's_formula' a b c = sqrt ((helper a b c) * (helper a b c - a) * (helper a b c - b) * (helper a b c - c))



fib = ((map fib' [0 ..]) !!)
   where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = trace(show(n)) fib (n - 1) + fib (n - 2)

fib1 x =
  let  fib' 0 = 0
       fib' 1 = 1
       fib' n = trace(show(n)) fib1 (n-1) + fib1 (n-2)
   in (map fib' [0..] !! x)

main :: IO()
main = do 
    startTime1 <- getCurrentTime
    let !x1 = fib1 10
    endTime1 <- getCurrentTime 
    let diff1 = diffUTCTime endTime1 startTime1
    print $ (show diff1)
    startTime2 <- getCurrentTime
    let !x2 = fib 10
    endTime2 <- getCurrentTime
    let diff2 = diffUTCTime endTime2 startTime2
    print $ (show diff2)


{-
main :: IO ()
main = do 
    startTime1 <- getCurrentTime
    let !result1 =  sum [ heron's_formula 3 4 5 | i <- [1..1000000]] 
    --print result1
    endTime1 <- getCurrentTime 
    let diff1 = diffUTCTime endTime1 startTime1
    print $ ( "using let in " ++ (show diff1))
    startTime2 <- getCurrentTime
    let !result2 =  sum [ heron's_formula' 3 4 5 | i <- [1..1000000]] 
    --print result2
    endTime2 <- getCurrentTime 
    let diff2 = diffUTCTime endTime2 startTime2
    print $ ("without using let in " ++ (show diff2))
    print $ diff2 - diff1 
-}