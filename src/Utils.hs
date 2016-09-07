module Utils
    ( bytesToString
    , bytesToInt
    , ltoa10
    ) where

import Data.Char (chr)
import Data.Word


-- |Converts a list of `Word8` into list of chars
--
bytesToString :: [Word8] -> String
bytesToString = takeWhile (/= '\NUL') . map (chr . fromIntegral)


-- |Converts a list of `Word8` into `Int` value
--
bytesToInt :: [Word8] -> Int
bytesToInt xs = foldr (\(v,p) s -> s + (fromIntegral v) * (256 ^ p)) 0 $ zip xs [0..]


-- |Converts a decimal number into string
-- Is equivalent to `ltoa(n, str, 10)`
ltoa10 :: Int -> String
ltoa10 n
    | n <  10   = show n
    | otherwise = ltoa10 (n `div` 10) ++ show (n `mod` 10)
