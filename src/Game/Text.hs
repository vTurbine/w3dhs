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

  printOne $ lines str
    where
      printOne     [] = return ()
      printOne (s:ss) = do
        (Point px py) <- getTextPos
        vwDrawPropString px py s

        (w, h) <- vwlMeasureString s

        liftIO $ print $ "W: " ++ (show w)
        liftIO $ print $ "H: " ++ (show h)
        liftIO $ print s

        modify (\s -> s { printX = windowX s
                        , printY = printY s + h })
        printOne ss


-- | Prints a string in the current window.
--   Newlines are supported
--
usCPrint :: String -> StateT GameState IO ()
usCPrint s = (mapM_ usCPrintLine $ lines s) >> updateScreen


-- | Prints a string centered on the current line
--   Newlines are not supported
--
usCPrintLine :: String -> StateT GameState IO ()
usCPrintLine s = do
  gstate <- get

  (w, h) <- vwlMeasureString s

  if w > (windowW gstate)
    then error "usCPrintLine: string exceeds width"
    else return ()

  let
    px = round $ fromIntegral ((windowX gstate) + (windowW gstate - w)) / 2
    py = printY gstate

  vwDrawPropString px py s

  put $ gstate { printY = printY gstate + h }
