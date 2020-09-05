functionC x y = case (x > y) of
                  True  -> x
                  False -> y

ifEvenAdd 2 n = case (even n) of
                  True -> (+) n 2
                  _    -> n

nums x = case compare x 0 of
          LT -> -1
          GT -> 1
          EQ -> 0
