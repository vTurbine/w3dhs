module Utils
    ( bytesToString
    ) where

import Data.Char (chr)
import Data.Word

-- |Converts a list of Word8 into list of chars
--
bytesToString :: [Word8] -> String
bytesToString = takeWhile (/= '\NUL') . map (chr . fromIntegral)