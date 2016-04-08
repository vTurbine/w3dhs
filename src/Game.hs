module Game
    ( introScreen
    ) where

import Graphics.UI.SDL
import Data.Word

import Resources.Configuration

-- @todo put all contents below into Game.Menu file

introMain_color, introEMS_color, introXMS_color, introFill_color :: Word32
introMain_color = 0x6C
introEMS_color  = 0x6C
introXMS_color  = 0x6C
introFill_color = 14

-- |A wrapper around the `fillRect`.
-- Similar to original Wolf's API
--
vwb_Bar :: Surface -> Int -> Int -> Int -> Int -> Word32 -> IO ()
vwb_Bar s r0 r1 r2 r3 px = do
    _ <- fillRect s (Just (Rect r0 r1 r2 r3)) (Pixel px)
    return ()

introScreen :: Surface -> GameConfig -> IO ()
introScreen s c = do
    -- Of course we have a lot of memory, especially the
    
    -- ..Main one,
    mapM_ (\i -> vwb_Bar s  49 (163 - 8 * i) 6 5 introMain_color) [0..9]

    -- EMS..,
    mapM_ (\i -> vwb_Bar s  89 (163 - 8 * i) 6 5 introEMS_color)  [0..9]

    -- ..and XMS for sure.
    mapM_ (\i -> vwb_Bar s 129 (163 - 8 * i) 6 5 introXMS_color)  [0..9]

    -- check if mouse enabled
    if mouseEn c
        then vwb_Bar s 164 82 12 2 introFill_color
        else return ()

    -- @todo add remain boxes
