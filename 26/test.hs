import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = 
  case M.lookup k m of
    Nothing -> (M.insert k 1 m, 1)
    Just a -> (M.insert k (a + 1) m, (a + 1))
