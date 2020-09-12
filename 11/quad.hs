data Quad = One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- how many different forms can this take?
eQuad :: Either Quad Quad
-- 4 * 4 = 16 forms

prodQuad :: (Quad, Quad)
-- 16

funcQuad :: Quad -> Quad
-- 4 ^ 4 = 256

prodTBool :: (Bool, Bool, Bool)
-- 4 * 4 * 4 = 64

gTwo :: Bool -> Bool -> Bool
-- 2 ^ 2 ^ 2 = 2 ^ (2 * 2) = 16

fTwo :: Bool -> Quad -> Quad
-- 4 ^ 4 ^ 2 = 4 ^ (4 * 2) = 65536