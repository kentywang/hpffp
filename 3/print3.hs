-- print3.hs
module Print3 where

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
    where secondGreeting =
            concat [hello, " ", world]
  
myGreeting :: [Char]
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"
