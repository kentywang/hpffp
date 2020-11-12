import Debug.Trace 

blah' :: IO String
blah' = trace "outer trace" (return "blah")

woot :: IO String
woot = return (trace "inner trace" "woot")

main :: IO () 
main = do
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w
