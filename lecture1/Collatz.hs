module Collatz where

collatz :: Integer -> Integer
collatz 1 = 0
collatz n = 1 + collatz (step n)

step :: Integer -> Integer
step n | n `mod` 2 == 0 = n `div` 2
       | otherwise = 3 * n + 1

longest :: Integer -> Integer
longest n = maximum $ map collatz [1..n]
