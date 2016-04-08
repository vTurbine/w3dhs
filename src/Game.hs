module Game
    ( introScreen
    ) where

import Graphics.UI.SDL
import Data.Word

import Resources.Configuration

introMain_color, introEMS_color, introXMS_color :: Word32
introMain_color = 0x6C
introEMS_color  = 0x6C
introXMS_color  = 0x6C

introScreen :: Surface -> GameConfig -> IO ()
introScreen s c = do
    -- Of course we have a lot of memory, especially the..
    
    -- ..Main one,
    mapM_ (\i -> fillRect s (Just (Rect 49
                                        (163 - 8 * i)
                                        6
                                        5)) (Pixel introMain_color)) [0..9]
    -- EMS..,
    mapM_ (\i -> fillRect s (Just (Rect 89
                                        (163 - 8 * i)
                                        6
                                        5)) (Pixel introEMS_color))  [0..9]
    -- end XMS for sure.
    mapM_ (\i -> fillRect s (Just (Rect 129
                                        (163 - 8 * i)
                                        6
                                        5)) (Pixel introXMS_color))  [0..9]

    -- @todo fill boxes
