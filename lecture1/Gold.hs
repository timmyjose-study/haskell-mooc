module Gold where

phi :: Double
phi = (sqrt 5 + 1) / 2

polynomial :: Double -> Double
polynomial x = x^2 - x - 1

f :: Double -> Double
f x = polynomial (polynomial x)

main :: IO ()
main = do print (polynomial phi)
          print (f phi)