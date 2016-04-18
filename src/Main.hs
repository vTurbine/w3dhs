module Main where

import Control.Monad.Trans.State
import Graphics.UI.SDL as SDL
import System.Exit

import Game
import Resources
import Settings

doExit = do
    SDL.quit
    exitWith ExitSuccess

gameLoop :: GameState -> IO ()
gameLoop gst = do
    ev <- waitEvent
    case ev of
        Quit                     -> doExit
        KeyDown (Keysym _ _ _)   -> doExit
        _                        -> return ()

    -- update game state
    gst' <- execStateT Game.updateState gst

    -- @todo
    -- draw surfaces, play sounds, etc..

    SDL.flip $ screen gst'
    gameLoop gst'


--
--
main :: IO ()
main = do
    -- Add cmdline params parsing like:
    -- fullscreen, SOD/Classic/Demo, Data path, etc.

    -- Load game resources
    gdata <- loadGameData

    -- Initialize SDL system
    SDL.init [InitAudio, InitVideo]

    screen <- SDL.setVideoMode scrWidth scrHeight scrBpp [HWSurface, HWPalette]

    -- set main palette
    -- @todo Currently we have a limitation with bpp incompatibility.
    -- The VGA uses 6-6-6 scheme while SDL expected 8-8-8. Need to find
    -- a way to transpose the palette.
    _ <- SDL.setColors screen (palette gdata) 0 -- @todo check for result.

    -- initialize game state and run the main loop
    gameLoop $ Game.initState {screen = screen, gameData = gdata}
