{-# LANGUAGE TemplateHaskell #-}

module Chapter4 where

import qualified Data.Map as Map
import Data.Array

import Test.QuickCheck

--- tuples

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

sumIf :: [(Bool, Int)] -> Int
sumIf [] = 0
sumIf ((True, d) : xs) = d + sumIf xs
sumIf ((False, _) : xs) = sumIf xs

--- folding

sumNumbers :: [Int] -> Int
sumNumbers [] = 0
sumNumbers (n : ns) = n + sumNumbers ns

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (n : ns) = max n (myMaximum ns)

countNothings :: [Maybe a] -> Int
countNothings [] = 0
countNothings (Nothing : xs) = 1 + countNothings xs
countNothings (Just _ : xs) = countNothings xs

sumNumbers' :: [Int] -> Int
sumNumbers' = foldr (+) 0

prop_sumNumbers :: [Int] -> Bool
prop_sumNumbers ns = sumNumbers ns == sumNumbers' ns

-- maps and arrays

withdraw :: String -> Int -> Map.Map String Int -> Map.Map String Int
withdraw account amount bank = 
  case Map.lookup account bank of
    Nothing -> bank
    Just sum -> Map.insert account (sum - amount) bank

type Bank = Map.Map String Int
type Account = String
type Amount = Int

withdraw' :: Account -> Amount -> Bank -> Bank
withdraw' account amount bank = Map.adjust (\curr -> curr - amount) account bank

myArray :: Array Int String
myArray = array (7, 11) [(7, "seven"), (8,"eight"), (9, "nine"), (10, "ten"), (11, "eleven")]

anotherArray :: Array Int String
anotherArray = listArray (7, 11) ["seven", "eight", "nine", "ten", "eleven"]

return []
main = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 1000 })