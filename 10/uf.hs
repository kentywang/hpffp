-- foldl (flip (*)) 1 [1..3]
-- foldl (flip *) (1 * 1) [2,3]
-- foldl (flip *) ((1 * 1) * 2) [3]
-- foldl (flip *) (((1 * 1) * 2) * 3) []
-- (((1 * 1) * 2) * 3)

-- foldr (||) False [False, True]

e = foldl (\acc -> (acc++) . show) "" [1..5]

f = foldr const 'a' ['1'..'5']

g = foldr const 0 [7,4,3,0,5]