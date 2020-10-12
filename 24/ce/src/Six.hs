{-# LANGUAGE TypeApplications #-}

module Six where

import Control.Applicative
import Data.Bits
import Data.Either ( rights )
import Data.List ( intersperse )
import Data.Word ( Word32, Word64 )
import Text.Trifecta
import Numeric ( showHex )

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord)

data IPAddress6 = 
  IPAddress6 Word64 Word64
  deriving (Eq, Ord)

instance Show IPAddress where
  -- Yay! This feels like a very Haskell-esque solution.
  show (IPAddress w32) =
    mconcat $ intersperse "." $ show . (.&. mask) . shiftR w32 <$> [24, 16, 8, 0]
    where mask = shiftR (complement zeroBits) 24

instance Show IPAddress6 where
  show (IPAddress6 w64_1 w64_2) =
    mconcat $ intersperse ":" $
      fmap (flip showHex "" . (.&. mask)) $ -- shiftR isn't part of the compose because it needs 2 args. Still possible w/ pointfree?
        shiftR <$> [w64_1, w64_2] <*> [48, 32, 16, 0] -- Using <*> to cross-product-apply
    where mask = shiftR (complement zeroBits) 48

integersToW32 :: [Integer] -> Word32
integersToW32 [a, b, c, d] = 
  fromInteger $ shiftL a 24 +
                shiftL b 16 +
                shiftL c 8 +
                d

parseIpv4 :: Parser IPAddress
parseIpv4 = do
  [a, b, c, d] <- (:) <$> integer <*> count 3 (char '.' *> integer)
  case and ((liftA2 (&&) (>=0) (<256)) <$> [a, b, c, d]) of
    True -> pure $ IPAddress $ integersToW32 [a, b, c, d]
    False -> fail "Decimal integer representing octet exceeds range."

parseHexDigitsAsInt :: Parser Integer
parseHexDigitsAsInt = do
  hexString <- some hexDigit
  let x = valueHexInteger hexString
  if liftA2 (&&) (>=0) (<16^4) x
    then pure x
    else fail "Hexadecimal integer representing quartet exceeds range."

eightIntsToIpv6 :: [Integer] -> IPAddress6
eightIntsToIpv6 [a, b, c, d, e, f, g, h] =
  IPAddress6 
    (fromInteger (shiftL a 48 +
                  shiftL b 32 +
                  shiftL c 16 +
                  d))
    (fromInteger (shiftL e 48 +
                  shiftL f 32 +
                  shiftL g 16 +
                  h))

-- | This version doesn't handle the collapsed format for IPv6.
parseIpv6 :: Parser IPAddress6
parseIpv6 = do
  xs <- liftA2 (:) parseHexDigitsAsInt (count 7 (char ':' *> parseHexDigitsAsInt))
  pure $ eightIntsToIpv6 xs

-- Lifted from tasty-kat
valueHexInteger :: String -> Integer
valueHexInteger s = read ("0x" ++ s)

twoW64sToInt :: Word64 -> Word64 -> Integer
twoW64sToInt a b =
  let a' = toInteger a
      b' = toInteger b
  in (shiftL a' 64) + b'

ipv6ToInteger :: IPAddress6 -> Integer
ipv6ToInteger (IPAddress6 a b) = twoW64sToInt a b

data Colon = Colon deriving Show

-- | This handles the collapsed format for IPv6.
-- There's a bunch of code duplication in these functions.
parseIpv6' :: Parser IPAddress6
parseIpv6' = do
  x <- some hexDigit <|> string ":"
  xs <- if x == ":"
            -- This `return []` "catches" the failure 
            -- should there be no more hexadecimals to parse.
            then (parseIpv6ColonEncountered 7 <|> return []) >>= return . (Left Colon :)
            else do
              let hexInt = valueHexInteger x
              if liftA2 (&&) (>=0) (<16^4) hexInt
                then parseIpv6ColonUnencountered 7 >>= return . (Right hexInt :)
                else fail "Hexadecimal integer representing quartet exceeds range."
  (pure . eightIntsToIpv6 . colonIntsListToInts) xs

-- | Tries for at most `n` quartets
parseIpv6ColonEncountered :: Int -> Parser [Either Colon Integer]
parseIpv6ColonEncountered 0 = return []
parseIpv6ColonEncountered n = do
  hexInt <- parseHexDigitsAsInt
  let m = n - 1
  -- This `return []` "catches" the failure should the hexadecimal just parsed
  -- was the last.
  tail <- (char ':' *> parseIpv6ColonEncountered m) <|> return []
  return $ Right hexInt : tail

-- | Tries for at most `n` quartets
parseIpv6ColonUnencountered :: Int -> Parser [Either Colon Integer]
parseIpv6ColonUnencountered 0 = return []
parseIpv6ColonUnencountered n = do
  char ':'
  x <- some hexDigit <|> string ":"
  let m = n - 1
  if x == ":"
    then (parseIpv6ColonEncountered m <|> return []) >>= return . (Left Colon :)
    else do
      let hexInt = valueHexInteger x
      if liftA2 (&&) (>=0) (<16^4) hexInt
        then parseIpv6ColonUnencountered m >>= return . (Right hexInt :)
        else fail "Hexadecimal integer representing quartet exceeds range."

-- Maybe not the right data type to represent this, since we only have
-- one colon in the list. Is this a bad pattern?
colonIntsListToInts :: [Either Colon Integer] -> [Integer]
colonIntsListToInts ns = go ns 0
  where go :: [Either Colon Integer] -> Int -> [Integer]
        go (Left Colon : ns) i = padWithZeroes (rights ns) i
        go (Right n : ns) i = n : go ns (i + 1)
        go [] _ = []

padWithZeroes :: [Integer] -> Int -> [Integer]
padWithZeroes ns i = replicate noOfZeroes 0 ++ ns
  where noOfZeroes = 8 - (i + length ns)

-- Ignoring "IPv4-mapped IPv6 unicast address" approach.
ipv4ToIpv6 :: IPAddress -> IPAddress6
ipv4ToIpv6 (IPAddress w32) = IPAddress6 0 $ fromIntegral $ toInteger w32

ipv6ToIpv4 :: IPAddress6 -> Maybe IPAddress
ipv6ToIpv4 addr =
  let ipv6Integer = ipv6ToInteger addr
      maxW32Bound = toInteger $ maxBound @Word32
  in if ipv6Integer > maxW32Bound
     then Nothing
     else Just $ IPAddress $ fromIntegral ipv6Integer

main = do
  let f = parseString parseIpv4 mempty
      g = parseString parseIpv6 mempty
      h = parseString parseIpv6' mempty
  print $ f "172.16.254.1" -- 2886794753
  print $ f "204.120.0.15" -- 3430416399

  -- print $ fmap ipv6ToInteger $ g "0:0:0:0:0:ffff:ac10:fe01"
  -- -- 281473568538113 
  -- print $ fmap ipv6ToInteger $ g "0:0:0:0:0:ffff:cc78:f"
  -- -- 281474112159759
  -- print $ fmap ipv6ToInteger $ g "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
  -- -- 338288524927261089654163772891438416681

  print $ h "0:0:0:0:0:ffff:ac10:fe01"
  -- 281473568538113 
  print $ h "0:0:0:0:0:ffff:cc78:f"
  -- 281474112159759
  print $ h "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
  -- 338288524927261089654163772891438416681
  print $ h "FE80::0202:B3FF:FE1E:8329"
  -- 338288524927261089654163772891438416681
  print $ h "2001:DB8::8:800:200C:417A"
  -- 42540766411282592856906245548098208122

  print $ ipv6ToIpv4 . ipv4ToIpv6 <$> f "172.16.254.1"
  print $ (fmap . fmap) ipv4ToIpv6 $ ipv6ToIpv4 <$> h "0:0:0:0:0:0:ac10:fe01"
  print $ (fmap . fmap) ipv4ToIpv6 $ ipv6ToIpv4 <$> h "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"

  -- TODO: DOT language problem