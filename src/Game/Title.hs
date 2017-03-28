{-

-}

module Game.Title
    ( pg13
    , titleLoop
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
import              Resources


pg13BgColor :: GameColor
pg13BgColor = 0x82  -- acid blue for PG13 logo background


-- | Display PG13 logo
--
pg13 :: StateT GameState IO ()
pg13 = do
  gstate <- get

  vwFadeOut

  -- clear the screen
  vwbBar (Rect 0 0 320 200) pg13BgColor
  -- display PG13 logo
  vwbDrawPic (Point 216 110) PG13PIC

  vwFadeIn

  -- userInput TickBase * 7

  vwFadeOut


--
--
titleLoop :: StateT GameState IO ()
titleLoop = do
  gstate <- get

  -- title page
  vwbDrawPic (Point 0 0) TITLEPIC
  updateScreen

--vwFadeIn
--userInput TickBase * 15
-- if -> exit
-- vwFadeOut

  -- credits page
  vwbDrawPic (Point 0 0) CREDITSPIC
  updateScreen

  -- high scores
  --
  --
  -- demo

-- fade out on exit
