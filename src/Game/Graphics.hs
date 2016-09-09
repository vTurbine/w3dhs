module Game.Graphics where

import Control.Exception (bracket_)
import Data.Char (ord)
import Data.Word
import Graphics.UI.SDL
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr

import Resources
import Settings

data Point = Point Int Int
type GameColor = Word8


getLumpNum :: GraphicNums -> Int
getLumpNum n = fromEnum n + (12 - 3)


-- |
--
vwb_DrawPic :: Point -> Lump -> IO ()
vwb_DrawPic (Point x y) (Lump w h pxs) = do

    -- create surface with size of sprite
    surf <- (createRGBSurfaceEndian [HWSurface] w h scrBpp) >>= displayFormat

    -- copy data to the surface
    setSurfaceData surf pxs

    -- blit it on the screen
    screen <- getVideoSurface
    _ <- blitSurface surf Nothing screen (Just (Rect x y w h))

    freeSurface surf


-- |A wrapper around the `fillRect`.
-- Similar to original Wolf's API
--
vwb_Bar :: Surface -> Rect -> Word32 -> IO ()
vwb_Bar s r px = do
    _ <- fillRect s (Just r) (Pixel px)
    return ()


-- |Draws a horizontal line with predefined width
-- @todo handle the `linewidth` parameter
vlHLin :: Surface -> Point -> Int -> GameColor -> IO ()
vlHLin s (Point x y) w c = vwb_Bar s (Rect x y (x + w) (y + 1)) $ fromIntegral c


-- |Draws a vertical line with predefined width
-- @todo handle the `linewidth` parameter
vlVLin :: Surface -> Point -> Int -> GameColor -> IO ()
vlVLin s (Point x y) h c = vwb_Bar s (Rect x y (x + 1) (y + h)) $ fromIntegral c


-- |A wrapper around the `vlHlin`
--
vwb_Hlin :: Surface -> Int -> Int -> Int -> GameColor -> IO ()
vwb_Hlin s x1 x2 y c = vlHLin s (Point x1 y) (x2 - x1 + 1) c


-- |A wrapper around the `vlVLin`
--
vwb_Vlin :: Surface -> Int -> Int -> Int -> GameColor -> IO ()
vwb_Vlin s y1 y2 x c = vlVLin s (Point x y1) (y2 - y1 + 1) c


--
--
vwb_Plot (Point x y) color = undefined


-- |
--
fadeIn :: IO ()
fadeIn = return ()


-- |
--
fadeOut :: IO ()
fadeOut = return ()


-- @todo too many black magic there:
-- - why do I need to add 3x0 after each line if surface width *exactly* the same as line width?
-- - how to make this code simplier and pretty? Now it looks like piece of crap
--
vw_DrawPropString :: Rect -> String -> [Glyph] -> Word8 -> IO ()
vw_DrawPropString r@(Rect x y w h) str f col = do
    -- render the string
    let
        sdata = take (w * h) $ concatMap (\i -> (concatMap (foo i) str)++[0,0,0]) [0..(h - 1)]
            where
                h = gHeight (f !! 0)
                foo i c = take w . drop (i * w) $ gData (f !! (ord c))
                    where w = gWidth (f !! (ord c))

    -- create surface with size of rect
    surf <- (createRGBSurfaceEndian [HWSurface] w h scrBpp) >>= displayFormat

    -- set transparent color for blitting
    _ <- setColorKey surf [SrcColorKey] (Pixel 0)

    -- copy data to the surface
    -- @todo I believe it may be much more pretty
    setSurfaceData surf (map (\p -> if p == 0 then 0 else col) sdata)

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
