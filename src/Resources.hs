module Resources
    ( loadPalette
    , loadSignOn
    ) where

import Data.Word
import Graphics.UI.SDL

import Resources.OMF as OMF
import Settings

-- |The 'loadPalette' returns a palette stored into
-- GAMEPAL.OBJ file. Parsing handled by OMF loader.
--
loadPalette :: IO [Color]
loadPalette = do
    dat <- OMF.findResource "_gamepal" (gameSrcPath ++ "OBJ/GAMEPAL.OBJ")
    return $ wordToColor dat
    where
        wordToColor [] = []
        wordToColor (r:g:b:res) = (Color (fromIntegral r) -- @todo palette adjustment (8bit)
                                         (fromIntegral g)
                                         (fromIntegral b)) : wordToColor res

-- |Loads signOn screen from the SIGNON.OBJ file.
-- Parsing handled by OMF loader.
--
loadSignOn :: IO [Word8]
loadSignOn =  OMF.findResource "_signon" (gameSrcPath ++ "OBJ/SIGNON.OBJ")
