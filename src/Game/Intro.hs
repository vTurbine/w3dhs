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

introScreen :: StateT GameState IO ()
introScreen = do
    -- get current game state and obtain the screen surface
    gameState <- get

    let s = screen gameState

    -- load and blit the intro screen
    signon <- liftIO $ loadSignOn

    liftIO $ setSurfaceData s signon

    -- Fill the boxes in the SignOn screen

    -- Of course we have a lot of memory, especially the
    -- ..Main one,
    liftIO $ forM_ [0..9] (\i -> vwb_Bar s  49 (163 - 8 * i) 6 5 introMain_color)

    -- EMS..,
    liftIO $ forM_ [0..9] (\i -> vwb_Bar s  89 (163 - 8 * i) 6 5 introEMS_color)

    -- ..and XMS for sure.
    liftIO $ forM_ [0..9] (\i -> vwb_Bar s 129 (163 - 8 * i) 6 5 introXMS_color)

    -- fill boxes
    -- mouse present
    liftIO $ vwb_Bar s 164  82 12 2 introFill_color
    -- joystick present
    liftIO $ vwb_Bar s 164 105 12 2 introFill_color
    -- AdLib present
    liftIO $ vwb_Bar s 164 128 12 2 introFill_color
    -- SoundBlaster present
    liftIO $ vwb_Bar s 164 151 12 2 introFill_color
    -- SoundSource present
    liftIO $ vwb_Bar s 164 174 12 2 introFill_color

    -- clear the "One moment.." text
    -- @todo get color
    liftIO $ vwb_Bar s 0 189 300 11 41

    setFontColor (14,   4)
    setTextPos   ( 0, 190)

    --us_CPrint "Press a key"

    -- wait for input here

    return ()
