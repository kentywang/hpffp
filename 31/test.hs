import           Control.Concurrent
import           Control.Monad
import           System.IO

main = do
  forkIO (replicateM_ 10 (putChar 'A'))
  replicateM_ 10 (putChar 'B')