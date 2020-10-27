import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

-- 1
rDec :: Num a => Reader a a
rDec = reader $ subtract 1

-- 3
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

-- 5
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT f
  where f a = do
          print $ "Hi: " ++ show a
          return $ a + 1 

-- 6
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT f
  where f a = do
          let a' = show a
          print $ mconcat ["Hi: ", a']
          return (a', a + 1)

main :: IO ()
main = do
  print $ runIdentity $ runReaderT rDec 1
  print $ fmap (runReader rDec) [1..10]

  print $ runReader rShow 1
  print $ fmap (runReader rShow) [1..10]

  runReaderT rPrintAndInc 1
  traverse (runReaderT rPrintAndInc) [1..10]

  runStateT sPrintIncAccum 10
  mapM (runStateT sPrintIncAccum) [1..5]

  return ()