module ListyInstances where

import Data.Monoid
import Listy

instance Monoid (Listy a) where
  mempty = Listy []

instance Semigroup (Listy a) where
  Listy l <> Listy l' = Listy $ l <> l'