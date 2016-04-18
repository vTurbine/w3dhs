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

import Control.Monad.Trans.State
import Control.Monad.Trans
import Graphics.UI.SDL
import Data.Word

import Game.Graphics
import Game.State
import Game.Text
import Resources

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
    liftIO $ mapM_ (\i -> vwb_Bar s  49 (163 - 8 * i) 6 5 introMain_color) [0..9] -- @todo forM_ ?

    -- EMS..,
    liftIO $ mapM_ (\i -> vwb_Bar s  89 (163 - 8 * i) 6 5 introEMS_color)  [0..9]

    -- ..and XMS for sure.
    liftIO $ mapM_ (\i -> vwb_Bar s 129 (163 - 8 * i) 6 5 introXMS_color)  [0..9]

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
    liftIO $ vwb_Bar s 0 189 300 11 14

    setFontColor (14, 4)
    --us_CPrint 0 190 14 4 "Press a key"

    -- wait for input here

    return ()
