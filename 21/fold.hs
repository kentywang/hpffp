import Data.Monoid ( Sum )
import Data.Functor.Constant ( Constant(Constant) )

xs :: [Sum Integer]
xs = [1, 2, 3, 4, 5]

y :: Constant (Sum Integer) [b]
y = traverse (Constant . (+1)) xs

{- 
traverse :: (a -> f b) -> t a -> f (t b)

-------------------------------------------------------------------------------
Constant . (+1)               :: Sum Integer -> Constant (Sum Integer) b     [1]
                                 ^^^^^^^^^^^    ^^^^^^^^^^^^^^^^^^^^^^
                                (     a      ->           f            b)

[1, 2, 3, 4, 5]               :: [] Sum Integer                              [2]
                                 ^^ ^^^^^^^^^^^
                                  t      a

traverse (Constant . (+1)) xs :: Constant (Sum Integer) ([] b)               [3]
                                 ^^^^^^^^^^^^^^^^^^^^^^  ^^ ^
                                           f            ( t b)

So with Constant, the term-level value is part of the structure, meaning it is
is mappended with other structures when we apply a Constant to another Constant
with <*>. So

[Constant (Sum Integer) b]   (Think of this as the step between [2] and [3])

when flipped becomes

Constant (Sum Integer) [b]

Since the structures are smashed together (possible since f is Applicative),
the left type of Constant is mappended (possible since Constant's
Applicative instance constrains the left type with Monoid).

-}