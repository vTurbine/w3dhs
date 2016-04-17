module Game
    ( vwb_Bar
    , setSurfaceData
    , GameState(..)
    , initState
    , updateState
    , us_CPrint
    ) where

import Control.Exception (bracket_)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr

import Graphics.UI.SDL
import Data.Word

-- |Game state record definition
--
data GameState = GameState  {
                              printX        :: Int
                            , printY        :: Int
                            , fontColor     :: Int
                            , backColor     :: Int
                            }
                            deriving (Show)

-- |Initializes the game state
-- @todo probably it's better to run all state-related calculations
-- in environment/state monad
--
initState :: GameState
initState = GameState   0
                        0
                        0
                        0


-- |Updates the game state
--
updateState :: GameState -> GameState
updateState = id

-- |A wrapper around the `fillRect`.
-- Similar to original Wolf's API
-- @todo get rectange as 'Rect' record
--
vwb_Bar :: Surface -> Int -> Int -> Int -> Int -> Word32 -> IO ()
vwb_Bar s r0 r1 r2 r3 px = do
    _ <- fillRect s (Just (Rect r0 r1 r2 r3)) (Pixel px)
    return ()


-- |Prints a string centered on the current line
-- @todo too many params
us_CPrint :: Int -> Int -> Int -> Int -> String -> IO ()
us_CPrint x y fc bgc str = undefined


-- |Copies the list of `Word8` into a surface
--
setSurfaceData :: Surface -> [Word8] -> IO ()
setSurfaceData s ids = do
    pxs <- surfaceGetPixels s
    bracket_
        (lockSurface s)
        (unlockSurface s)
        (pokeArray (castPtr pxs) ids)
