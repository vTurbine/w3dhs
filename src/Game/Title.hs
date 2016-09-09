{-

-}

module Game.Title
    ( pg13
    , titlePage
    , creditsPage
    ) where


import              Control.Monad (forM_)
import              Control.Monad.Trans.State
import              Control.Monad.Trans (liftIO)
import              Graphics.UI.SDL
import              Data.Word

-- Internal modules import
import              Game.Graphics
import              Resources.Gfxv_wl6      as WL6
import              Game.State
import              Game.Text
import              Resources


--
--
pg13 :: StateT GameState IO ()
pg13 = do
    -- get current game state
    gstate <- get

    -- startCPMusic INTROSONG

    -- clear the screen
    vwbBar (Rect 0 0 320 200) 0x82
    -- display PG13 logo
    vwbDrawPic (Point 216 110) PG13PIC

    -- wait for a while and process to the title screens
    put $ gstate { nextSteps = [DelayMs 3000, TitlePage] }


--
--
titlePage :: StateT GameState IO ()
titlePage = do
    -- get current game state
    gstate <- get

    -- draw the title screen
    vwbDrawPic (Point 0 0) TITLEPIC

    -- wait for input here and process to the title screens
    put $ gstate { nextSteps = [DelayMs 3000, FadeIn, Credits] }


--
--
creditsPage :: StateT GameState IO ()
creditsPage = do
    -- get current game state
    gstate <- get

    -- draw the intro screen
    vwbDrawPic (Point 0 0) CREDITSPIC

    -- wait for input here and process to the title screens
    put $ gstate { nextSteps = [DelayMsInt 3000, FadeOut, HighScores, MainMenu] }
