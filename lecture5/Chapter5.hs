module Chapter5 where

data Color = Red | Blue | Green

rgb :: Color -> (Double, Double, Double)
rgb Red = (1, 0, 0)
rgb Blue = (0, 1, 0)
rgb Green = (0, 0, 1)

type Id = Int
type Title = String
type Body = String

data Report = ConstructReport Id Title Body deriving Show

reportContents :: Report -> String
reportContents (ConstructReport _ _ body) = body

setReportContents :: String -> Report -> Report
setReportContents contents (ConstructReport id title _) = ConstructReport id title contents

data Card = Joker
          | Heart Int
          | Club Int
          | Spade Int
          | Diamond Int
          deriving Show

data Described a = Describe a String

getValue :: Described a -> a
getValue (Describe x _) = x

getDescription :: Described a -> String
getDescription (Describe _ desc) = desc

data List a = Nil | Cons a (List a) 
  deriving Show

lhead :: List a -> a
lhead Nil = error "empty list!"
lhead (Cons x _) = x

ltail :: List a -> List a 
ltail Nil = error "empty list!"
ltail (Cons _ xs) = xs

llength :: List a -> Int
llength Nil = 0
llength (Cons _ xs) = 1 + llength xs

data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show

example :: Tree Int
example = Node (Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf)) 0 (Node Leaf 4 Leaf)

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node l _ r) = 1 + max (treeHeight l) (treeHeight r)

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l v r) | x <= v = Node (insert x l) v r
                      | otherwise = Node l v (insert x r)

fromList :: Ord a => [a] -> Tree a
fromList = foldl (\acc x -> insert x acc) Leaf 

treeLookup :: Ord a => a -> Tree a -> Bool
treeLookup x Leaf = False
treeLookup x (Node l v r) | x < v = treeLookup x l
                          | x == v = True
                          | otherwise = treeLookup x r

--- record syntax

  {-
type Name = String
type Age = Int
type City = String
type Country = String
type Profession = String

data Person = MkPerson Name Age City Country Profession deriving Show
-}

data Person = MkPerson {
                          name :: String
                         , age :: Int
                         , city :: String
                         , country :: String
                         , profession :: String
                       }
                       deriving Show

people :: [Person]
people = [MkPerson "Jane Doe" 21 "Houston" "USA" "Engineer", 
         MkPerson "Maija Meikalainen" 35 "Rovaniemi" "Finland" "Engineer",
         MkPerson "Mauno Mutikainen" 27 "Turku" "Finland" "Mathematician"]

--query :: [Person] -> [Person]
--query = filter (\(MkPerson _ _ _ country profession) -> country == "Finland" && profession == "Engineer") 

query :: [Person] -> [Person]
query = filter (\p -> country p == "Finland" && profession p == "Engineer")


