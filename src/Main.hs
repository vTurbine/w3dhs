module Main where

import Graphics.UI.SDL as SDL

import Game.Menu

import Resources
import Settings

main :: IO ()
main = do
    -- Add cmdline params parsing like:
    -- fullscreen, SOD/Classic/Demo, Data path, etc.

    -- Load game resources
    palette <- loadPalette
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

    -- draw [Intro Screen]
    Game.Menu.introScreen screen

    SDL.flip screen

    -- @todo no game loop yet. Just waiting for user input
    _ <- getChar
    return ()
