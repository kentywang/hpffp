-- 1.
data Person = Person Bool
  deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2.
data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown x = if x == Woot  -- Infers x must be Mood
                then Blah
                else x

-- 4.
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"  -- Compiles, but is partially-applied function
s2 = Sentence "Julie" "loves" "dogs"