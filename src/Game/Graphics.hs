{-
 |

-}

module Game.Graphics
    ( vwbDrawPic
    , vwbBar
    , vwbVlin
    , vwbHlin
    , vwbPlot
    , vwDrawPropString
    , setSurfaceData
    , Point(..)
    , GameColor(..)
    ) where

import              Control.Monad.Trans.State
import              Control.Monad.Trans (liftIO)
import              Control.Exception (bracket_)
import              Data.Char (ord)
import              Data.Word
import              Foreign.Marshal.Array (pokeArray)
import              Foreign.Ptr
import              Graphics.UI.SDL

-- Internal modules import
import              Game.State
import              Resources
import              Settings


data Point      = Point Int Int
type GameColor  = Word8


getLumpNum :: GraphicNums -> Int
getLumpNum n = fromEnum n + (12 - 3)


-- |Copies specified lump to main surface
--
vwbDrawPic :: Point -> GraphicNums -> StateT GameState IO ()
vwbDrawPic (Point x y) n = do
    -- get current game state
    gstate <- get

    let
        gdata          = (gameData gstate)
        (Lump w h pxs) = (lumps gdata) !! getLumpNum n

    liftIO $ do
        -- create surface with size of sprite
        surf <- (createRGBSurfaceEndian [HWSurface] w h scrBpp) >>= displayFormat

        -- copy data to the surface
        setSurfaceData surf pxs

        -- blit it on the screen
        _ <- blitSurface surf Nothing (screen gstate) (Just (Rect x y w h))

        freeSurface surf


-- |A wrapper around the `fillRect`.
-- Similar to original Wolf's API
--
vwbBar :: Rect -> GameColor -> StateT GameState IO ()
vwbBar r c = do
    -- get current game state
    gstate <- get

    _ <- liftIO $ fillRect (screen gstate) (Just r) (Pixel $ fromIntegral c)
    return ()


-- |Draws a horizontal line with predefined width
-- @todo handle the `linewidth` parameter
vlHLin :: Point -> Int -> GameColor -> StateT GameState IO ()
vlHLin (Point x y) w c = vwbBar (Rect x y w 1) c


-- |Draws a vertical line with predefined width
-- @todo handle the `linewidth` parameter
vlVLin :: Point -> Int -> GameColor -> StateT GameState IO ()
vlVLin (Point x y) h c = vwbBar (Rect x y 1 h) c


-- |A wrapper around the `vlHlin`
--
vwbHlin :: Int -> Int -> Int -> GameColor -> StateT GameState IO ()
vwbHlin x1 x2 y c = vlHLin (Point x1 y) (x2 - x1 + 1) c


-- |A wrapper around the `vlVLin`
--
vwbVlin :: Int -> Int -> Int -> GameColor -> StateT GameState IO ()
vwbVlin y1 y2 x c = vlVLin (Point x y1) (y2 - y1 + 1) c


-- |Draws a point with specified color at given coordinates
--
vwbPlot :: Point -> GameColor -> StateT GameState IO ()
vwbPlot (Point x y) c = return () -- @fixme fillRect x y 1 1 seems don't work


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
vwDrawPropString :: Rect -> String -> [Glyph] -> Word8 -> StateT GameState IO ()
vwDrawPropString r@(Rect x y w h) str f col = do
    gstate <- get
    -- render the string
    let
        sdata = take (w * h) $ concatMap (\i -> (concatMap (foo i) str)++[0,0,0]) [0..(h - 1)]
            where
                h = gHeight (f !! 0)
                foo i c = take w . drop (i * w) $ gData (f !! (ord c))
                    where w = gWidth (f !! (ord c))

    liftIO $ do
        -- create surface with size of rect
        surf <- (createRGBSurfaceEndian [HWSurface] w h scrBpp) >>= displayFormat

        -- set transparent color for blitting
        _ <- setColorKey surf [SrcColorKey] (Pixel 0)

        -- copy data to the surface
        -- @todo I believe it may be much more pretty
        setSurfaceData surf (map (\p -> if p == 0 then 0 else col) sdata)

        -- blit it on the screen
        _ <- blitSurface surf Nothing (screen gstate) (Just r)

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
