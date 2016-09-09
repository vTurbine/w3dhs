{-
 | Intro screen module implementation

  The game shows this screen during startup. It contains some
  information about your system like available memory, inputs
  and sound devices installed.

  The base image stored into SIGNON.OBJ file and contains empty
  boxes and title "One moment.." at the bottom. When resources
  loaded it changes to "Press a key" and after input acknowlegment
  processes to title screens.

  We will fill all blanks as 'available' here.

-}

module Game.Intro
    ( introScreen_begin
    , introScreen_end
    ) where

import          Control.Monad (forM_)
import          Control.Monad.Trans.State
import          Control.Monad.Trans (liftIO)
import          Graphics.UI.SDL
import          Data.Word

-- Internal modules import
import          Game.Graphics
import          Game.State
import          Game.Text
import          Resources


introMain_color, introEMS_color, introXMS_color, introFill_color :: GameColor
introMain_color = 0x6C
introEMS_color  = 0x6C
introXMS_color  = 0x6C
introFill_color = 14


--
--
introScreen_begin :: StateT GameState IO ()
introScreen_begin = do
    -- get current game state
    gstate <- get

    -- draw the intro screen
    --

    -- Draw background image
    liftIO $ setSurfaceData (screen gstate) (signon gstate) -- @todo move signon into gamedata

    -- Fill the boxes in the signon screen

    -- Of course we have a lot of memory, especially the
    -- ..Main one,
    forM_ [0..9] (\i -> vwbBar (Rect  49 (163 - 8 * i) 6 5) introMain_color)

    -- EMS..,
    forM_ [0..9] (\i -> vwbBar (Rect  89 (163 - 8 * i) 6 5) introEMS_color)

    -- ..and XMS for sure.
    forM_ [0..9] (\i -> vwbBar (Rect 129 (163 - 8 * i) 6 5) introXMS_color)

        -- mouse present
    vwbBar (Rect 164  82 12 2) introFill_color
    -- joystick present
    vwbBar (Rect 164 105 12 2) introFill_color
    -- AdLib present
    vwbBar (Rect 164 128 12 2) introFill_color
    -- SoundBlaster present
    vwbBar (Rect 164 151 12 2) introFill_color
    -- SoundSource present
    vwbBar (Rect 164 174 12 2) introFill_color

    -- clear the "One moment.." text
    vwbBar (Rect 0 189 300 11) 41

    -- All ok. Can proceed to resource loading and schedule the IntroEnd
    put $ gstate { nextSteps = [LoadResources, IntroEnd] }

--
--
introScreen_end :: StateT GameState IO ()
introScreen_end = do
    -- get current game state
    gstate <- get

    setFontColor (14,   4)
    setTextPos   ( 0, 190)

    us_CPrint "Press a key"

    -- wait for input here and process to the title screens
    put $ gstate { nextSteps = [WaitForInput, TitlePG13] }
