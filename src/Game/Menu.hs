{-
 |
-}

module Game.Menu
    ( us_ControlPanel
    , mainMenu
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


bordColor, bord2Color, deactive, bkgdColor, stripe :: Word32
bordColor  = 0x29
bord2Color = 0x23
deactive   = 0x2b
bkgdColor  = 0x2d
stripe     = 0x2c

menuX, menuY, menuW, menuH :: Int
menuX =  76
menuY =  55
menuW = 178
menuH = 13 * 9 + 6

{-
-- | Clear Menu screen to dark red
-- @todo spear case
clearMScreen :: Surface -> IO ()
clearMScreen s = do
    surf <- getVideoSurface
    vwb_Bar s (Rect 0 0 320 200) bordColor


-- |
-- @todo spear case
drawStripes :: Surface -> Int -> IO ()
drawStripes s y = do
    vwb_Bar  s (Rect 0 y 320 22) 0
    vwb_Hlin s 0 319 (y + 23) 0


drawOutline :: Surface -> Rect -> Word32 -> Word32 -> IO ()
drawOutline s r c1 c2 = do
    return ()


-- |
--
drawWindow :: Surface -> Rect -> Word32 -> IO ()
drawWindow s r wc = do
    vwb_Bar s r wc
    drawOutline s r bord2Color deactive


drawMenu :: Surface -> IO ()
drawMenu s = return ()


handleMenu :: StateT GameState IO ()
handleMenu = do
    gstate <- get

    let
        gdata = gameData gstate
        curs1 = (lumps gdata) !! getLumpNum C_CURSOR1PIC

    liftIO $ vwb_DrawPic (Point 10 10) curs1 -- @fixme


-- |
--
mainMenu_draw :: GameData -> Bool -> IO ()
mainMenu_draw gdata ingame = do

    s <- getVideoSurface

    clearMScreen s

    let
        mouseLBack = (lumps gdata) !! getLumpNum C_MOUSELBACKPIC
        options    = (lumps gdata) !! getLumpNum C_OPTIONSPIC

    vwb_DrawPic (Point 112 184) mouseLBack
    drawStripes s 10
    vwb_DrawPic (Point  84   0) options

    drawWindow s (Rect (menuX - 8) (menuY - 3) menuW menuH) bkgdColor
    drawMenu s
-}

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
us_ControlPanel :: Maybe SDLKey -> StateT GameState IO ()
us_ControlPanel key = do
    gstate <- get

    -- @todo

    -- liftIO $ mainMenu_draw (gameData gstate) (inGame gstate)
    -- menuFadeIn

    mainMenuLoop


-- |
--
mainMenu :: StateT GameState IO ()
mainMenu = do
    gstate <- get

    us_ControlPanel Nothing

    put $ gstate { nextSteps = [MainMenuLoop] }
