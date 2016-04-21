module Game.Graphics where

import Control.Exception (bracket_)
import Data.Word
import Graphics.UI.SDL
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr

import Resources

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
vw_DrawPropString = undefined


-- |Copies the list of `Word8` into a surface
--
setSurfaceData :: Surface -> [Word8] -> IO ()
setSurfaceData s ids = do
    pxs <- surfaceGetPixels s
    bracket_
        (lockSurface s)
        (unlockSurface s)
        (pokeArray (castPtr pxs) ids)
