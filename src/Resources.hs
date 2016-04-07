module Resources
    ( loadPalette
    ) where

import Graphics.UI.SDL

-- |The 'loadPalette' returns a palette stored into
-- GAMEPAL.OBJ file. Parsing handled by OMF loader.
loadPalette :: IO [Color]
loadPalette = undefined