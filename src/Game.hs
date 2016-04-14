module Game
    ( vwb_Bar
    , setSurfaceData
    , GameState(..)
    , initState
    , updateState
    ) where

import Control.Exception (bracket_)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr

import Graphics.UI.SDL
import Data.Word

-- |Game state record definition
--
data GameState = GameState  { dummy :: String
                            }
                            deriving (Show)

-- |Initializes the game state
-- @todo probably it's better to run all state-related calculations
-- in environment/state monad
--
initState :: IO GameState
initState = return $ GameState "empty"


-- |Updates the game state
--
updateState :: GameState -> IO GameState
updateState _ = return $ GameState "updated"

-- |A wrapper around the `fillRect`.
-- Similar to original Wolf's API
--
vwb_Bar :: Surface -> Int -> Int -> Int -> Int -> Word32 -> IO ()
vwb_Bar s r0 r1 r2 r3 px = do
    _ <- fillRect s (Just (Rect r0 r1 r2 r3)) (Pixel px)
    return ()

-- |Copies the list of `Word8` into a surface
--
setSurfaceData :: Surface -> [Word8] -> IO ()
setSurfaceData s ids = do
    pxs <- surfaceGetPixels s
    bracket_
        (lockSurface s)
        (unlockSurface s)
        (pokeArray (castPtr pxs) ids)
