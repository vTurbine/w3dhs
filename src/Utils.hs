module Utils
    ( bytesToString
    , bytesToInt
    ) where

import Data.Char (chr)
import Data.Word

-- |Converts a list of 'Word8' into list of chars
--
bytesToString :: [Word8] -> String
bytesToString = takeWhile (/= '\NUL') . map (chr . fromIntegral)

-- |Converts a list of 'Word8' into 'Int' value
--
bytesToInt :: [Word8] -> Int
bytesToInt xs = foldr (\(v,p) s -> s + (fromIntegral v) * (256 ^ p)) 0 $ zip xs [0..]
