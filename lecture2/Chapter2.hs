module Chapter2 where

repeatString :: Int -> String -> String
repeatString n str  = repeatHelper n str ""

repeatHelper :: Int -> String -> String -> String
repeatHelper 0 str acc = acc
repeatHelper n str acc = repeatHelper (n - 1) str (acc <> str)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

readInt :: String -> Either String Int
readInt "0" = Right 0
readInt "1" = Right 1
readInt s = Left ("Unsupported string: " <> s)

iWantAString :: Either String Int -> String
iWantAString (Left str) = str
iWantAString (Right n) = show n

