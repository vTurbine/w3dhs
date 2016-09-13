module Game.Text where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Char


import Game.Graphics
import Game.State
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
setTextPos (Point px py) = modify (\s -> s { printX = px
                                           , printY = py
                                           })


-- |Returns text position
--
getTextPos :: StateT GameState IO Point
getTextPos = do
    gstate <- get
    return (Point (printX gstate) (printY gstate))



-- |Prints a string in the current window. Newlines are supported
--
usPrint :: String -> StateT GameState IO ()
usPrint str = do
    gstate <- get

    printOne (windowX gstate) $ lines str
        where
            printOne w (s:[]) = do
                (Point px py) <- getTextPos
                vwDrawPropString px py s

                modify (\s -> s { printX = printX s + w })

            printOne _ (s:ss) = do
                (Point px py) <- getTextPos
                vwDrawPropString px py s

                (w, h) <- vwlMeasureString s
                modify (\s -> s { printX = windowX s
                                , printY = printY s + h })
                printOne w ss


-- |A wrapper around the `usCPrintLine`
--
usCPrint :: String -> StateT GameState IO ()
usCPrint = usCPrintLine


-- |Prints a string centered on the current line
-- @fixme
usCPrintLine :: String -> StateT GameState IO ()
usCPrintLine str = do
    gstate <- get

    (w, h) <- vwlMeasureString str

    if w > (windowW gstate)
        then error "usCPrintLine: string exceeds width"
        else return ()

    let
        px = round $ fromIntegral ((windowX gstate) + (scrWidth - w)) / 2
        py = printY gstate

    vwDrawPropString px py str

    put $ gstate { printY = printY gstate + h }
