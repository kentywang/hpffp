import Data.Char ( toUpper )
import Data.Text ( splitOn, intercalate, unpack, pack )

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = toUpper c : cs

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph x =
  joinOnPunkts .
  map f .
  splitOnPunkts $ x
  where f (' ':xs) = ' ' : f xs
        f xs = capitalizeWord xs

-- Learned of splitOn, intercalate, unpack, pack
-- All because I was too lazy to write my own String
-- versions. Not efficient though...
splitOnPunkts :: String -> [String]
splitOnPunkts =
  map unpack . splitOn (pack ".") . pack

joinOnPunkts :: [String] -> String
joinOnPunkts =
  unpack . intercalate (pack ".") . map pack

main :: IO ()
main = do
  print $ capitalizeWord "chortle" == "Chortle"
  print $ capitalizeParagraph s == "Blah. Woot ha."
    where s = "blah. woot ha."