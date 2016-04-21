module Game.Graphics where

import Control.Exception (bracket_)
import Data.Char (ord)
import Data.Word
import Graphics.UI.SDL
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr

import Resources
import Settings

-- |A wrapper around the `fillRect`.
-- Similar to original Wolf's API
--
vwb_Bar :: Surface -> Rect -> Word32 -> IO ()
vwb_Bar s r px = do
    _ <- fillRect s (Just r) (Pixel px)
    return ()


--
--
vw_DrawPropString :: Rect -> String -> Font -> IO ()
vw_DrawPropString r@(Rect x y w h) str f = do
    -- @todo render the string
    -- @kludge rework it ASAP!
    let
        ofsts = map (\c -> ((glyphOfs f) !! (ord c) - 770, (glyphWidths f) !! (ord c) * (glyphHeight f))) str
        glyphs = map (\(o,s) -> take s . drop o $ (glyphsData f)) ofsts
        sdata = concat glyphs

    -- create surface with size of rect
    surf <- (createRGBSurfaceEndian [SWSurface] w h scrBpp) >>= displayFormat

    -- copy data to the surface
    setSurfaceData surf sdata

    -- blit it on the screen
    screen <- getVideoSurface
    _ <- blitSurface surf Nothing screen (Just r)

    freeSurface surf


-- |Copies the list of `Word8` into a surface
--
setSurfaceData :: Surface -> [Word8] -> IO ()
setSurfaceData s ids = do
    pxs <- surfaceGetPixels s
    bracket_
        (lockSurface s)
        (unlockSurface s)
        (pokeArray (castPtr pxs) ids)
