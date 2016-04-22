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


-- @todo too many black magic there:
-- - why do I need to add 3x0 after each line if surface width *exactly* the same as line width?
-- - how to make this code simplier and pretty? Now it looks like piece of crap
-- - need to change color
-- - need to add alpha
--
vw_DrawPropString :: Rect -> String -> [Glyph] -> IO ()
vw_DrawPropString r@(Rect x y w h) str f = do
    -- render the string
    let
        sdata = take (w * h) $ concatMap (\i -> (concatMap (foo i) str)++[0,0,0]) [0..(h - 1)]
            where
                h = gHeight (f !! 0)
                foo i c = take w . drop (i * w) $ gData (f !! (ord c))
                    where w = gWidth (f !! (ord c))

    print $ length sdata

    -- create surface with size of rect
    surf <- (createRGBSurfaceEndian [SWSurface] (w) (h) scrBpp) >>= displayFormat

    -- copy data to the surface
    setSurfaceData surf sdata

    print $ surfaceGetWidth surf
    print $ surfaceGetHeight surf

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
