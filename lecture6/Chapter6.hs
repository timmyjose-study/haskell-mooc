module Chapter6 where

data Color = Black | White deriving Show

instance Eq Color where
  Black == Black = True
  White == White = True
  _ == _ = False

class Size a where
  empty :: a
  size :: a -> Int
  sameSize :: a -> a -> Bool

instance Size Int where
  empty = 0
  size  = abs 
  sameSize x y = size x == size y

instance Size [a] where
  empty = []
  size = length
  sameSize xs ys = size xs == size ys

instance Size (Maybe a) where
  empty = Nothing
  
  size Nothing = 0
  size (Just _) = 1

  sameSize mx my = size mx == size my

class Example a where
  example :: a
  examples :: [a]
  examples = [example] -- default implementation

instance Example Int where
  example = 0
  examples = [1, 2, 3, 4, 5]

instance Example Bool where
  example = True

class Combine a where
  combine :: a -> a -> a

combine3 :: Combine a => a -> a -> a -> a
combine3 x y z = combine x (combine y z)

data IntPair = IntPair Int Int deriving Show

instance Eq IntPair where
  IntPair p1 p2 == IntPair p3 p4 = p1 == p3 && p2 == p4

instance Ord IntPair where
  IntPair p1 p2 <= IntPair p3 p4 
    | p1 < p3 = True
    | p1 > p3 = False
    | otherwise = p2 <= p4

data Person = Dead | Alive String Int deriving (Eq, Show, Ord)

-- hierarchies

data Pair a = MkPair a a
  deriving Show

instance Eq a => Eq (Pair a) where
  MkPair x1 x2 == MkPair y1 y2 = x1 == y1 && x2 == y2

class Check a where
  check :: a -> Bool

instance Check Int where
  check = (> 0)

checkAll :: Check a => [a] -> Bool
checkAll = and . map check

instance Check a => Check [a] where
  check = and . map check

class Size a => SizeBoth a where
  sizeBoth :: a -> a -> Int
  sizeBoth x y = size x + size y