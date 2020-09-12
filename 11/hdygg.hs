-- data FlowerType = Gardenia
--                 | Daisy
--                 | Rose
--                 | Lilac
--                 deriving Show

type Gardener = String

-- data Garden =
--   Garden Gardener FlowerType
--   deriving Show

-- sum of products normal form
data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show

data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b } 
                deriving (Eq, Show)

newtype NumCow = NumCow Int
  deriving (Eq, Show)

newtype NumPig = NumPig Int
  deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' =
  Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
  
data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo deriving (Eq, Show)
-- Alternately
type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)