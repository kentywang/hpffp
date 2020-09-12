isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip mayybee id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList _ = []

catMaybes :: [Maybe a] -> [a]
catMaybes = map helper . filter isJust
  where helper (Just a) = a  -- Beware, no otherwise case!

-- | This was harder than just filtering out Nothings
-- first and then unwrapping the Justs.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:_) = Nothing
flipMaybe ((Just x):xs) =
  case (rest) of
    Nothing -> Nothing
    Just ys -> Just (x : ys)
  where rest = flipMaybe xs

main :: IO ()
main = do
  print $ isJust (Just 1) -- True
  print $ isJust Nothing -- False
  print $ isNothing (Just 1) -- False
  print $ isNothing Nothing -- True
  print $ mayybee 0 (+1) Nothing -- 0
  print $ mayybee 0 (+1) (Just 1) -- 2
  print $ fromMaybe 0 Nothing -- 0
  print $ fromMaybe 0 (Just 1) -- 1
  print $ listToMaybe [1, 2, 3] -- Just 1
  print $ listToMaybe ([] :: [()]) -- Nothing
  print $ maybeToList (Just 1) -- [1]
  print $ maybeToList (Nothing :: Maybe ()) -- []
  print $ catMaybes [Just 1, Nothing, Just 2] -- [1, 2]
  print $ catMaybes $
          take 3 $
          repeat (Nothing :: Maybe ()) -- []
  print $ flipMaybe [Just 1, Just 2, Just 3] -- Just [1, 2, 3]
  print $ flipMaybe [Just 1, Nothing, Just 3]  -- Nothing
