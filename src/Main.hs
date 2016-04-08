module Main where

import Control.Exception (bracket_)
import Data.Word
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Video

import Game as Game
import Resources
import Settings

setSurfaceData :: Surface -> [Word8] -> IO ()
setSurfaceData s ids = do
    pxs <- surfaceGetPixels s
    bracket_
        (lockSurface s)
        (unlockSurface s)
        (pokeArray (castPtr pxs) ids)

main :: IO ()
main = do
    -- Add cmdline params parsing like:
    -- fullscreen, SOD/Classic/Demo, Data path, etc.

    -- Load game resources
    palette <- loadPalette
    signon  <- loadSignOn
    config  <- loadConfig

    putStrLn $ "-- Configuration dump --"
    print $ config
    putStrLn $ "-- End of dump --"

    -- Initialize SDL system
    SDL.init [InitAudio, InitVideo]

    screen <- SDL.setVideoMode scrWidth scrHeight scrBpp [HWSurface, HWPalette]

    -- set main palette
    -- @todo Currently we have a limitation with bpp incompatibility.
    -- The VGA uses 6-6-6 scheme while SDL expected 8-8-8. Need to find
    -- a way to transpose the palette.
    _ <- SDL.setColors screen palette 0 -- @todo check for result.

    -- blit the SignOn
    setSurfaceData screen signon

    -- draw [Intro Screen]
    Game.introScreen screen config

    SDL.flip screen

    -- @todo no game loop yet. Just waiting for user input
    _ <- getChar
    return ()
