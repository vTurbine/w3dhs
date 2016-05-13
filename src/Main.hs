module Main where

import          Control.Monad.Trans.State
import          Control.Monad.Trans (liftIO)
import          Graphics.UI.SDL as SDL
import          System.Exit

import Game
import Game.State
import Resources
import Settings

doExit = do
    SDL.quit
    exitWith ExitSuccess

gameLoop :: StateT GameState IO ()
gameLoop = do
    gstate <- get

    liftIO $ print (currStep gstate)

    let
      akeys = activeKeys gstate

    ev <- liftIO $ pollEvent
    case ev of
        Quit                          -> liftIO $ doExit
        KeyDown (Keysym SDLK_q _ _)   -> liftIO $ doExit -- fast exit
        KeyDown (Keysym      k _ _)   -> modify $ (\s -> s { activeKeys = k : akeys
                                                           , inputAck = True
                                                           })
        _                             -> return ()

    -- update the state
    Game.updateState
    gstate' <- get

    -- flip the final surface
    liftIO $ SDL.flip $ screen gstate

    -- reset input
    put $ gstate' { inputAck = False
                  , activeKeys = []
                  }

    gameLoop


--
--
main :: IO ()
main = do
    -- Add cmdline params parsing like:
    -- fullscreen, SOD/Classic/Demo, Data path, etc.

    -- Load game palette & signon screen
    so  <- loadSignon
    pal <- loadPalette

    -- Initialize SDL system
    SDL.init [InitAudio, InitVideo]

    screen <- SDL.setVideoMode scrWidth scrHeight scrBpp [HWSurface, HWPalette]

    -- set main palette
    -- @todo Currently we have a limitation with bpp incompatibility.
    -- The VGA uses 6-6-6 scheme while SDL expected 8-8-8. Need to find
    -- a way to transpose the palette.
    _ <- SDL.setColors screen pal 0 -- @todo check for result.

    -- initialize game state and run the main loop
    finalState <- execStateT gameLoop $ Game.initState { nextSteps = [IntroBegin]
                                                       , screen = screen
                                                       , signon = so
                                                       , palette = pal
                                                       }
    return ()
