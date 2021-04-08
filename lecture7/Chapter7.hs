module Chapter7 where

import Data.Ord (comparing)
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NonEmpty

-- common patterns


-- boxed types

data Plate = Plate String
  deriving (Eq, Show)

parsePlate :: String -> Maybe Plate
parsePlate str | validPlaterNumber str = Just $ Plate str
               | otherwise = Nothing

validPlaterNumber :: String -> Bool
validPlaterNumber str | length str >= 5 && length str <= 8 = True
                      | otherwise = False

data Money = Money Int deriving Show

renderMoney :: Money -> String
renderMoney (Money cents) = show (fromIntegral cents / 100)

(+!) :: Money -> Money -> Money
(+!) (Money m1) (Money m2) = Money (m1 + m2)

scale :: Money -> Double -> Money
scale (Money cents) s = Money (round (fromIntegral cents * s))

addVat :: Money -> Money
addVat m = m +! scale m 0.24

-- modelling with cases

data Person = MkPerson {
                        name :: String
                      , age:: Int
                       }
                       deriving Show

data SortOrder = Ascending | Descending
data SortField = Name | Age

sortByField :: SortField -> [Person] -> [Person]
sortByField Name = sortBy (comparing name) 
sortByField Age = sortBy (comparing age)

sortPersons :: SortField -> SortOrder -> [Person] -> [Person]
sortPersons field Ascending ps = sortByField field ps
sortPersons field Descending ps = reverse (sortByField field ps)

persons :: [Person]
persons = [MkPerson { name = "Fridolf", age = 73 }, MkPerson { name = "Greta", age = 60 }, MkPerson { name = "Hans", age = 65 }]

nonEmpty :: [a] -> Maybe (NonEmpty.NonEmpty a)
nonEmpty [] = Nothing
nonEmpty (x : xs) = Just (x NonEmpty.:| xs)

neHead :: NonEmpty.NonEmpty a -> a
neHead (x NonEmpty.:| _) = x

neLast :: NonEmpty.NonEmpty a -> a
neLast (x NonEmpty.:| []) = x
neLast (_ NonEmpty.:| xs) = last xs


-- semigroups and monoids

  {-
    class Semigroup a where
      (<>) :: a -> a -> a

  -}

data MySum a = MySum a deriving Show

instance Num a => Semigroup (MySum a) where
  MySum x <> MySum y = MySum (x + y)

data MyProduct a = MyProduct a deriving Show

instance Num a => Semigroup (MyProduct a) where
  MyProduct x <> MyProduct y = MyProduct (x * y)

  {-
    class Semigroup a => Monoid a where
      mempty :: a
  -}

instance Num a => Monoid (MySum a) where
  mempty = MySum 0

instance Num a => Monoid (MyProduct a) where
  mempty = MyProduct 1