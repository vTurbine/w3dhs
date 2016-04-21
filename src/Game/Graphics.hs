module Game.Graphics where

import Control.Exception (bracket_)
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
vw_DrawPropString :: Surface -> Rect -> String -> Font -> IO ()
vw_DrawPropString s r@(Rect x y w h) str f = do
    -- @todo render the string
    let sdata = replicate (w * h) 0 :: [Word8]

    -- create surface with size of rect
    (Just surf) <- tryCreateRGBSurfaceEndian [SWSurface] w h scrBpp

    -- copy data to the surface
    setSurfaceData surf sdata

    -- blit it on the screen
    blitSurface surf Nothing s (Just r)
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
