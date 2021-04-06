module Chapter3 where

import Data.List (tails)

applyTo1 :: (Int -> Int) -> Int
applyTo1 f = f 1

addThree :: Int -> Int
addThree x = x + 3

doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

makeCool :: String -> String
makeCool s = "WOW " <> s <> "!"

palindrome :: String -> Bool
palindrome s = s == reverse s

palindromes :: Int -> [String]
palindromes n = filter palindrome . map show $ [1..n]

countAWords :: String -> Int
countAWords = length . filter (\w -> head w == 'a') . words

substringsOfLength :: Int -> String -> [String]
substringsOfLength n s | length s < n = []
                       | otherwise = take n s : substringsOfLength n (tail s)

whatFollows :: Char -> Int -> String -> [String]
whatFollows c k s = map tail . filter (\w -> head w == c) $ substringsOfLength (k + 1) s

between :: Integer -> Integer -> Integer -> Bool
between low high x = x > low && x < high

findSubstring :: String -> String -> String
findSubstring chars = takeWhile (\c -> elem c chars) . dropWhile (\c -> not $ elem c chars)

descend :: Int -> [Int]
descend 0 = []
descend n = n : descend (n - 1)

split :: Char -> String -> [String]
split c [] = []
split c ds = filter (\w -> length w > 0) $ takeWhile (/= c) ds : split c (drop 1 (dropWhile (/= c) ds))

-- custom operators

infixr 6 <+>
(<+>) :: [Int] -> [Int] -> [Int]
xs <+> ys = zipWith (+) xs ys

infixr 6 +++
(+++) :: String -> String -> String
xs +++ ys = xs <> " " <> ys