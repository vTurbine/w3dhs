module Game.Graphics where

import Control.Exception (bracket_)
import Data.Word
import Graphics.UI.SDL
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr

import Resources

-- |A wrapper around the `fillRect`.
-- Similar to original Wolf's API
-- @todo get rectange as 'Rect' record
--
vwb_Bar :: Surface -> Int -> Int -> Int -> Int -> Word32 -> IO ()
vwb_Bar s r0 r1 r2 r3 px = do
    _ <- fillRect s (Just (Rect r0 r1 r2 r3)) (Pixel px)
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
