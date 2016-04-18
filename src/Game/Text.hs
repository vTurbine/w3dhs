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
setFontColor :: (Int, Int) -> StateT GameState IO ()
setFontColor (fc, bc) = do
    gameState <- get
    put $ gameState { fontColor = fc, backColor = bc }
    return ()


-- |Sets position for text output
--
setTextPos :: (Int, Int) -> StateT GameState IO ()
setTextPos (px, py) = do
    gameState <- get
    put $ gameState { printX = px, printY = py }
    return ()


-- |Returns size of the string in pixels
--
vwl_MeasureString :: String -> StateT GameState IO (Int, Int)
vwl_MeasureString str = do
    gstate <- get

    let
        gdata = gameData gstate
        font  = startFont gdata
        width = sum $ map (\c -> (glyphWeights font) !! ord c) str

    return (width, glyphHeight font)


-- |Prints a string centered on the current line
--
us_CPrint :: String -> StateT GameState IO ()
us_CPrint str = do
    gstate <- get

    (sWidth, sHeight) <- vwl_MeasureString str

    let
        gdata = gameData gstate
        font  = startFont gdata
        px = round $ fromIntegral ((windowX gstate) + (scrWidth - sWidth)) / 2
        py = printY gstate

    -- draw the string on the surface
    liftIO $ vw_DrawPropString (Rect px py sWidth sHeight) str font

    put $ gstate { printY = py + sHeight }
    return ()
