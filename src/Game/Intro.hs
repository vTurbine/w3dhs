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
    ( introScreenPre
    , introScreenPost
    ) where

import          Control.Monad (forM_)
import          Control.Monad.Trans.State
import          Control.Monad.Trans (liftIO)
import          Graphics.UI.SDL

-- Internal modules import
import          Game.Graphics
import          Game.State
import          Game.Text
import          Resources


introMainColor, introEMSColor, introXMSColor, introFillColor :: GameColor
introMainColor = 0x6C
introEMSColor  = 0x6C
introXMSColor  = 0x6C
introFillColor = 14


--
--
introScreenPre :: StateT GameState IO ()
introScreenPre = do
    -- get current game state
    gstate <- get

    signon <- liftIO loadSignon

    -- draw the intro screen
    --

    -- Draw background image
    liftIO $ setSurfaceData (screen gstate) signon

    -- Fill the boxes in the signon screen

    -- Of course we have a lot of memory, especially the
    -- ..Main one,
    forM_ [0..9] (\i -> vwbBar (Rect  49 (163 - 8 * i) 6 5) introMainColor)

    -- EMS..,
    forM_ [0..9] (\i -> vwbBar (Rect  89 (163 - 8 * i) 6 5) introEMSColor)

    -- ..and XMS for sure.
    forM_ [0..9] (\i -> vwbBar (Rect 129 (163 - 8 * i) 6 5) introXMSColor)

        -- mouse present
    vwbBar (Rect 164  82 12 2) introFillColor
    -- joystick present
    vwbBar (Rect 164 105 12 2) introFillColor
    -- AdLib present
    vwbBar (Rect 164 128 12 2) introFillColor
    -- SoundBlaster present
    vwbBar (Rect 164 151 12 2) introFillColor
    -- SoundSource present
    vwbBar (Rect 164 174 12 2) introFillColor

    -- clear the "One moment.." text
    vwbBar (Rect 0 189 300 11) 41

    -- All ok. Can proceed to resource loading and schedule the IntroEnd
    put $ gstate { nextSteps = [LoadResources, IntroEnd] }

--
--
introScreenPost :: StateT GameState IO ()
introScreenPost = do
    -- get current game state
    gstate <- get

    setFontColor (14, 4)
    setTextPos   (Point 0 190)

    modify (\s -> s { windowX =   0 -- @todo setWindowRect ?
                    , windowW = 320
                    })

    usCPrint "Press a key"

    -- wait for input here and process to the title screens
    put $ gstate { nextSteps = [WaitForInput, TitlePG13] }
