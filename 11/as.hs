import Data.Char ( toUpper )

isSubseqOf :: (Eq a)
           => [a] -- "blah"
           -> [a] -- "wboloath"
           -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf sub@(x:xs) (y:ys)
  | x == y    = isSubseqOf xs ys
  | otherwise = isSubseqOf sub ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = go $ words s
  where go [] = []
        go (w@(c:cs):ws) = (w, toUpper c : cs) : go ws

main :: IO ()
main = do
  print $ isSubseqOf "blah" "wootblah" == True
  print $ isSubseqOf "blah" "wboloath" == True
  print $ isSubseqOf "blah" "halbwoot" == False
  print $ capitalizeWords "hello world" ==
    [("hello", "Hello"), ("world", "World")]