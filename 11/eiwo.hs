data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

convert :: Quantum -> Bool
-- 1
convert _ = True

-- 2
convert _ = False

-- 3
convert Yes = True
convert _ = False

-- 4
convert Yes = False
convert _ = True

-- 5
convert No = True
convert _ = False

-- 6
convert No = False
convert _ = True

-- 7
convert Both = True
convert _ = False

-- 8
convert Both = False
convert _ = True

-- Looks like there are 2^3 possible implementations as predicted
-- by the function typing.