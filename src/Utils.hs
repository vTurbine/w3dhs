module Utils
    ( bytesToString
    , bytesToInt
    , ltoa10
    , bsToListOfWord16
    , bsToListOfWord8
    , encodeWord16le
    , getWord8Remain
    ) where

import            Data.Binary.Strict.Get
import            Data.Bits
import qualified  Data.ByteString as B
import            Data.Char             (chr)
import            Data.Word


-- | Converts a list of `Word8` into list of chars
--
bytesToString :: [Word8] -> String
bytesToString = takeWhile (/= '\NUL') . map (chr . fromIntegral)


-- | Converts a list of `Word8` into `Int` value
--
bytesToInt :: [Word8] -> Int
bytesToInt xs = foldr (\(v,p) s -> s + (fromIntegral v) * (256 ^ p)) 0 $ zip xs [0..]


-- | Converts a decimal number into string
--   Equivalent to `ltoa(n, str, 10)`
ltoa10 :: Int -> String
ltoa10 n
    | n <  10   = show n
    | otherwise = ltoa10 (n `div` 10) ++ show (n `mod` 10)


-- | Converts BS into list of `Word16`
--
bsToListOfWord16 :: B.ByteString -> [Word16]
bsToListOfWord16 bs = do
  let res = runGet getWord16leRemain bs
    in case res of
         (Right ls, _) -> ls
         (Left err, _) -> error $ "Utils: unable to construct list : " ++ (show $ B.length bs)


-- | Converts BS into list of `Word16`
--
bsToListOfWord8 :: B.ByteString -> [Word8]
bsToListOfWord8 bs = do
  let res = runGet getWord8Remain bs
    in case res of
         (Right ls, _) -> ls
         (Left err, _) -> error $ "Utils: unable to construct list : " ++ (show $ B.length bs)


-- | Converts a `Word16` into two `Word8`
--
encodeWord16le :: Word16 -> (Word8, Word8)
encodeWord16le x = flip (,) (fromIntegral $ (x .&. 0x00ff))
                            (fromIntegral $ (x .&. 0xff00) `shiftR` 8)

-- |Consumes parser's input to the end by `Word16le`
--
getWord16leRemain :: Get [Word16]
getWord16leRemain = do
  e <- isEmpty
  if e
     then return []
     else do
       b  <- getWord16le
       bs <- getWord16leRemain
       return $
         (b : bs)

-- |Consumes parser's input to the end by `Word8`
--
getWord8Remain :: Get [Word8]
getWord8Remain = do
  e <- isEmpty
  if e
     then return []
     else do
       b  <- getWord8
       bs <- getWord8Remain
       return $
         (b : bs)
