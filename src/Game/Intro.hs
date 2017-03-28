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
    ( signonScreen
    , introScreen
    , finishSignon
    ) where

import          Control.Monad (forM_)
import          Control.Monad.Trans.State
import          Control.Monad.Trans (liftIO)
import          Graphics.UI.SDL

-- Internal modules import
import          Game.Graphics
import          Game.Input
import          Game.State
import          Game.Text
import          Resources


introMainColor, introEMSColor, introXMSColor, introFillColor :: GameColor
introMainColor = 0x6C
introEMSColor  = 0x6C
introXMSColor  = 0x6C
introFillColor = 14

txtBgColor :: GameColor
txtBgColor = 41  -- obtained from bg picture


-- | Draw blank signon screen
--
signonScreen :: StateT GameState IO ()
signonScreen = do
  gstate <- get

  signon <- liftIO $ loadSignon
  -- draw background image
  liftIO $ setSurfaceData (screen gstate) signon


-- | Fill signon with system information
--
introScreen :: StateT GameState IO ()
introScreen = do
  -- get current game state
  gstate <- get

  -- Fill the boxes in the signon screen

  -- Of course we have a lot of memory, especially the
  -- ..Main one,
  forM_ [0..9] (\i -> vwbBar (Rect  49 (163 - 8 * i) 6 5) introMainColor)

  -- EMS..,
  forM_ [0..9] (\i -> vwbBar (Rect  89 (163 - 8 * i) 6 5) introEMSColor)

  -- ..and XMS for sure.
  forM_ [0..9] (\i -> vwbBar (Rect 129 (163 - 8 * i) 6 5) introXMSColor)

  -- mouse is present
  vwbBar (Rect 164  82 12 2) introFillColor
  -- joystick is present
  vwbBar (Rect 164 105 12 2) introFillColor
  -- AdLib is present
  vwbBar (Rect 164 128 12 2) introFillColor
  -- SoundBlaster is present
  vwbBar (Rect 164 151 12 2) introFillColor
  -- SoundSource is present
  vwbBar (Rect 164 174 12 2) introFillColor


-- | Finish signon screen and wait for user input
--
finishSignon :: StateT GameState IO ()
finishSignon = do
  gstate <- get

  modify (\s -> s { windowX =   0 -- @todo setWindowRect ?
                  , windowW = 320
                  })

  -- clear the "One moment.." text
  vwbBar (Rect 0 189 300 11) txtBgColor

  setFontColor (14, 4)
  setTextPos   (Point 0 190)

  usCPrint "Press a key"

  -- TODO: check for `noWait`
  inAck

  -- clear text again
  vwbBar (Rect 0 189 300 11) txtBgColor

  setFontColor (10, 4)
  setTextPos   (Point 0 190)

  usCPrint "Working..."

  setFontColor ( 0, 15)
