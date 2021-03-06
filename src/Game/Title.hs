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
import              Game.Input          (inUserInput)
import              Resources.Gfxv_wl6  as WL6
import              Game.State
import              Resources
import Resources.Map

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

  inUserInput 3000

  vwFadeOut


-- | Shows title pages in loop
--
titleLoop :: StateT GameState IO ()
titleLoop = do
  gstate <- get

  -- title page
  vwbDrawPic (Point 0 0) TITLEPIC

  vwFadeIn

  titleAck <- inUserInput 3000
  if titleAck
     then return ()
     else do

       vwFadeOut

       -- credits page
       vwbDrawPic (Point 0 0) CREDITSPIC

       vwFadeIn

       creditsAck <- inUserInput 3000
       if creditsAck
          then return ()
          else do

            vwFadeOut

            -- high scores
            -- TODO

            -- demo
            -- TODO
            --
            -- check playstate
            -- startCPMusic

            titleLoop
