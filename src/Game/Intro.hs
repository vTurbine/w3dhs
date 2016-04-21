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
    ( introScreen
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


introMain_color, introEMS_color, introXMS_color, introFill_color :: Word32
introMain_color = 0x6C
introEMS_color  = 0x6C
introXMS_color  = 0x6C
introFill_color = 14


-- |An IO wrapper for surface-related operations
-- Reduces amount of 'liftIO' operations
--
introScreen_drawBegin :: Surface -> [Word8] -> IO ()
introScreen_drawBegin surf bg = do
    -- Draw background image
    setSurfaceData surf bg

    -- Fill the boxes in the signon screen

    -- Of course we have a lot of memory, especially the
    -- ..Main one,
    forM_ [0..9] (\i -> vwb_Bar surf (Rect  49 (163 - 8 * i) 6 5) introMain_color)

    -- EMS..,
    forM_ [0..9] (\i -> vwb_Bar surf (Rect  89 (163 - 8 * i) 6 5) introEMS_color)

    -- ..and XMS for sure.
    forM_ [0..9] (\i -> vwb_Bar surf (Rect 129 (163 - 8 * i) 6 5) introXMS_color)

        -- mouse present
    vwb_Bar surf (Rect 164  82 12 2) introFill_color
    -- joystick present
    vwb_Bar surf (Rect 164 105 12 2) introFill_color
    -- AdLib present
    vwb_Bar surf (Rect 164 128 12 2) introFill_color
    -- SoundBlaster present
    vwb_Bar surf (Rect 164 151 12 2) introFill_color
    -- SoundSource present
    vwb_Bar surf (Rect 164 174 12 2) introFill_color

    -- clear the "One moment.." text
    -- @todo get color
    vwb_Bar surf (Rect 0 189 300 11) 41


--
--
introScreen :: StateT GameState IO ()
introScreen = do
    -- get current game state
    gstate <- get

    let
        gdata    = gameData gstate
        intorscr = signon gdata

    -- draw the intro screen
    liftIO $ introScreen_drawBegin (screen gstate) intorscr

    setFontColor (14,   4)
    setTextPos   ( 0, 190)

    us_CPrint "Press a key"

    -- wait for input here

    return ()
