module Main where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO as SIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

-- Will also trim that file down to fit it in memory
dictWords :: IO String
dictWords =
  SIO.readFile "/Users/kentywang/Documents/Repos/hpffp/13/hangman/data/dict.txt"

dictWordsT :: IO T.Text
dictWordsT =
  TIO.readFile "/Users/kentywang/Documents/Repos/hpffp/13/hangman/data/dict.txt"

dictWordsTL :: IO TL.Text
dictWordsTL =
  TLIO.readFile "/Users/kentywang/Documents/Repos/hpffp/13/hangman/data/dict.txt"

main :: IO ()
main = do
  replicateM_ 10 (dictWords >>= print)
  replicateM_ 10 (dictWordsT >>= TIO.putStrLn)
  replicateM_ 10 (dictWordsTL >>= TLIO.putStrLn)