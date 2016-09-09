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


-- |An IO wrapper around surface-related operations
-- Reduces amount of 'liftIO' operations
--
pg13_drawBegin :: Surface -> GameData-> IO ()
pg13_drawBegin surf gdata = do

    let
        pic = (lumps gdata) !! getLumpNum PG13PIC

    -- @todo fade out

    -- clear screen
    vwb_Bar surf (Rect 0 0 320 200) 0x82

    -- draw PG13 logo
    vwb_DrawPic (Point 216 110) pic

    -- @todo fade in


--
--
pg13 :: StateT GameState IO ()
pg13 = do
    -- get current game state
    gstate <- get

    let
        gdata    = gameData gstate
        --intorscr = signon gdata

    -- startCPMusic INTROSONG

    -- draw the intro screen
    liftIO $ pg13_drawBegin (screen gstate) gdata

    -- wait for a while and process to the title screens
    put $ gstate { nextSteps = [DelayMs 3000, TitlePage] }


--
--
titlePage_draw :: Surface -> GameData-> IO ()
titlePage_draw surf gdata = do

    let
        pic = (lumps gdata) !! getLumpNum TITLEPIC

    -- draw the title
    vwb_DrawPic (Point 0 0) pic


--
--
titlePage :: StateT GameState IO ()
titlePage = do
    -- get current game state
    gstate <- get

    let
        gdata = gameData gstate

    -- draw the intro screen
    liftIO $ titlePage_draw (screen gstate) gdata

    -- wait for input here and process to the title screens
    put $ gstate { nextSteps = [DelayMs 3000, FadeIn, Credits] }


--
--
creditsPage_draw :: Surface -> GameData-> IO ()
creditsPage_draw surf gdata = do

    let
        pic = (lumps gdata) !! getLumpNum CREDITSPIC

    -- draw the title
    vwb_DrawPic (Point 0 0) pic


--
--
creditsPage :: StateT GameState IO ()
creditsPage = do
    -- get current game state
    gstate <- get

    let
        gdata    = gameData gstate

    -- draw the intro screen
    liftIO $ creditsPage_draw (screen gstate) gdata

    -- wait for input here and process to the title screens
    put $ gstate { nextSteps = [DelayMsInt 3000, FadeOut, HighScores, MainMenu] }
