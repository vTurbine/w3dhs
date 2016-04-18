module Game.Text where

import Control.Monad.Trans.State

import Game.State

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


-- |Prints a string centered on the current line
--
us_CPrint :: String -> StateT GameState IO ()
us_CPrint = undefined
