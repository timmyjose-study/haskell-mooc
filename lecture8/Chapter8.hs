module Chapter8 where

printTwoThings :: IO ()
printTwoThings = do 
  putStrLn "Hello!"
  putStrLn "How are you?"

greet :: IO ()
greet = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn $ "Nice to meet you, " <> name <> "!"

-- run as: `ghc -main-is Chapter8 Chapter8.hs && ./Chapter8`
main :: IO ()
main = do print "This is the end of part 1!"

