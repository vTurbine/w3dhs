module Game.Menu
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
    mapM_ (\i -> vwb_Bar s  49 (163 - 8 * i) 6 5 introMain_color) [0..9]

    -- EMS..,
    mapM_ (\i -> vwb_Bar s  89 (163 - 8 * i) 6 5 introEMS_color)  [0..9]

    -- ..and XMS for sure.
    mapM_ (\i -> vwb_Bar s 129 (163 - 8 * i) 6 5 introXMS_color)  [0..9]

    -- check if mouse enabled
    let mouseEn = True -- @todo add real check or stub. Not decided yet
    if mouseEn
        then vwb_Bar s 164 82 12 2 introFill_color
        else return ()

    -- @todo add remain boxes