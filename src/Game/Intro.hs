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

import Graphics.UI.SDL
import Data.Word

import Game
import Resources

introMain_color, introEMS_color, introXMS_color, introFill_color :: Word32
introMain_color = 0x6C
introEMS_color  = 0x6C
introXMS_color  = 0x6C
introFill_color = 14

introScreen :: Surface -> IO ()
introScreen s = do
    -- load and blit the SignOn
    signon  <- loadSignOn

    setSurfaceData s signon

    -- Fill the boxes in the SignOn screen

    -- Of course we have a lot of memory, especially the
    -- ..Main one,
    mapM_ (\i -> vwb_Bar s  49 (163 - 8 * i) 6 5 introMain_color) [0..9] -- @todo forM_ ?

    -- EMS..,
    mapM_ (\i -> vwb_Bar s  89 (163 - 8 * i) 6 5 introEMS_color)  [0..9]

    -- ..and XMS for sure.
    mapM_ (\i -> vwb_Bar s 129 (163 - 8 * i) 6 5 introXMS_color)  [0..9]

    -- fill boxes
    -- mouse present
    vwb_Bar s 164  82 12 2 introFill_color
    -- joystick present
    vwb_Bar s 164 105 12 2 introFill_color
    -- AdLib present
    vwb_Bar s 164 128 12 2 introFill_color
    -- SoundBlaster present
    vwb_Bar s 164 151 12 2 introFill_color
    -- SoundSource present
    vwb_Bar s 164 174 12 2 introFill_color

    -- clear the "One moment.." text
    -- @todo get color
    vwb_Bar s 0 189 300 11 14

    -- fontColor = 14, BG = 4
    -- @todo probably it's better to add the position, font and BG color
    -- settings into the game state
    --us_CPrint 0 190 14 4 "Press a key"

    -- wait for input here

    return ()
