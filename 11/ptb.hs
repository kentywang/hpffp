-- bring Int8 in scope
import Data.Int

data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)

-- Cardinality: 2 + 2 = 4

data NumberOrBool =
    Numba Int8  -- Range is -128 to 127
  | BoolyBool Bool deriving (Eq, Show)

-- Cardinality: 256 + 2 = 258

-- parentheses due to syntactic
-- collision between (-) minus
-- and the negate function
myNumba = Numba (-129)  -- Overflows to become 127
myNumba2 = Numba (128)  -- Overflows to become -128

data Person =
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)

-- sample data
jm = Person "julie" 108
ca = Person "chris" 16

-- How to get the name without record syntax
-- namae :: Person -> String namae (MkPerson s _) = s
