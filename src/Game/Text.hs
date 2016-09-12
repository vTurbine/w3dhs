module Game.Text where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Char
import Graphics.UI.SDL

import Game.Graphics
import Game.State
import Resources
import Settings


-- |Sets font and background color
--
setFontColor :: (GameColor, GameColor) -> StateT GameState IO ()
setFontColor (fc, bc) = do
    gameState <- get
    put $ gameState { fontColor = fc, backColor = bc }
    return ()


-- |Sets position for text output
--
setTextPos :: Point -> StateT GameState IO ()
setTextPos (Point px py) = do
    gameState <- get
    put $ gameState { printX = px, printY = py }
    return ()


-- |Returns size of the string in pixels
--
vwlMeasureString :: String -> StateT GameState IO (Int, Int)
vwlMeasureString str = do
    gstate <- get

    let
        gdata = gameData gstate
        font  = startFont gdata
        width = sum $ map (\c -> gWidth (font !! ord c)) str

    return (width, gHeight (font !! 0)) -- we have the height for all glyphs


-- | Prints a string in the current window. Newlines are supported
-- @todo
usPrint :: String -> StateT GameState IO ()
usPrint str = return ()


-- |Prints a string centered on the current line
--
usCPrint :: String -> StateT GameState IO ()
usCPrint str = do
    gstate <- get

    (sWidth, sHeight) <- vwlMeasureString str

    let
        gdata = gameData gstate
        font  = startFont gdata
        px = round $ fromIntegral ((windowX gstate) + (scrWidth - sWidth)) / 2
        py = printY gstate
        col = fromIntegral $ fontColor gstate

    -- draw the string on the surface
    vwDrawPropString (Rect px py sWidth sHeight) str font col

    put $ gstate { printY = py + sHeight }
    return ()
