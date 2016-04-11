module Main where

import Graphics.UI.SDL as SDL
import System.Exit

import Game.Menu

import Resources
import Settings

doExit = do
    SDL.quit
    exitWith ExitSuccess

gameLoop :: IO ()
gameLoop = do
    ev <- waitEvent
    case ev of
        Quit                     -> doExit
        KeyDown (Keysym _ _ _)   -> doExit
        _                        -> return ()
    gameLoop

main :: IO ()
main = do
    -- Add cmdline params parsing like:1
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

    gameLoop
