cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: [Char] -> [Char] -> [Char]
flippy = flip cattyConny

appedCatty :: [Char] -> [Char]
appedCatty = cattyConny "woops"

frappe :: [Char] -> [Char]
frappe = flippy "haha"

main :: IO ()
main = do
  -- "woops mrow woohoo!"
  putStrLn $ appedCatty "woohoo!"
  -- "1 mrow haha"
  putStrLn $ frappe "1"
  -- "woops mrow 2 mrow haha"
  putStrLn $ frappe (appedCatty "2")
  -- "woops mrow blue mrow haha"
  putStrLn $ appedCatty (frappe "blue")
  -- "pink mrow haha mrow green mrow woops mrow blue"
  putStrLn $ cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
  -- "are mrow Pugs mrow awesome"
  putStrLn $ cattyConny (flippy "Pugs" "are") "awesome"