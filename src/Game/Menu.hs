{-
 |
-}

module Game.Menu
    ( mainMenu
    , mainMenuLoop
    ) where


import              Control.Monad.Trans.State
import              Control.Monad.Trans (liftIO)
import              Data.Word
import              Graphics.UI.SDL as SDL

-- Internal modules import
import              Game.Graphics
import              Game.State
import              Resources


bordColor, bord2Color, deactive, bkgdColor, stripe :: GameColor
bordColor  = 0x29
bord2Color = 0x23
deactive   = 0x2b
bkgdColor  = 0x2d
stripe     = 0x2c

menuX, menuY, menuW, menuH :: Int
menuX =  76
menuY =  55
menuW = 178
menuH = 13 * 10 + 6


-- |Clears Menu screen to dark red
-- @todo SOD case
clearMScreen :: StateT GameState IO ()
clearMScreen = vwbBar (Rect 0 0 320 200) bordColor


-- |
-- @todo spear case
drawStripes :: Int -> StateT GameState IO ()
drawStripes y = do
    gstate <- get

    let
        by = if isSpear gstate
                then 24
                else 22
        w  = if isSpear gstate
                then 22
                else 23
        c  = if isSpear gstate
                then stripe
                else 0

    vwbBar  (Rect 0 y 320 by) 0
    vwbHlin 0 319 (y + w) c


-- |
--
drawOutline :: Rect -> GameColor -> GameColor -> StateT GameState IO ()
drawOutline (Rect x y w h) c1 c2 = do
    vwbHlin x (x + w)      y  c2
    vwbVlin y (y + h)      x  c2
    vwbHlin x (x + w) (y + h) c1
    vwbVlin y (y + h) (x + w) c1


-- |
--
drawWindow :: Rect -> GameColor -> StateT GameState IO ()
drawWindow r wc = do
    vwbBar r wc
    drawOutline r bord2Color deactive


{-
drawMenu :: Surface -> IO ()
drawMenu s = return ()


handleMenu :: StateT GameState IO ()
handleMenu = do
    gstate <- get

    let
        gdata = gameData gstate
        curs1 = (lumps gdata) !! getLumpNum C_CURSOR1PIC

    liftIO $ vwb_DrawPic (Point 10 10) curs1 -- @fixme

-}
-- |
--
drawMainMenu :: StateT GameState IO ()
drawMainMenu = do
    clearMScreen

    vwbDrawPic (Point 112 184) C_MOUSELBACKPIC
    drawStripes 10
    vwbDrawPic (Point  84   0) C_OPTIONSPIC

    drawWindow (Rect (menuX - 8) (menuY - 3) menuW menuH) bkgdColor
    --drawMenu


-- |
--
mainMenuLoop :: StateT GameState IO ()
mainMenuLoop = do
    gstate <- get

    --handleMenu
    -- @todo

    let
        nextState = if startGame gstate || loadedGame gstate
                    then RestartGame
                    else MainMenuLoop

    put $ gstate { nextSteps = [nextState] }


-- |
--
usControlPanel :: Maybe SDLKey -> StateT GameState IO ()
usControlPanel key = do

    --cpCheckQuick
    --startCPMusic MENUSONG

    -- @todo handle scancode

    drawMainMenu
    -- menuFadeIn

    mainMenuLoop


-- |
--
mainMenu :: StateT GameState IO ()
mainMenu = do
    gstate <- get

    usControlPanel Nothing

    put $ gstate { nextSteps = [MainMenuLoop] }
