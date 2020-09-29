module HttpStuff where

import Data.ByteString.Lazy hiding (map)
import Data.Foldable ( fold )
import Network.Wreq
import Control.Lens

-- replace with other websites
-- if desired or needed
urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5" ]

mappingGet :: [IO (Response ByteString)] 
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls

main = do
  responseList <- traversedUrls
  print $ map (^. responseBody) responseList