{-
 | Main module implementation.

-}

module Main
  ( main
  ) where

import          Control.Monad.Trans.State
import          Control.Monad.Trans (liftIO)
import          Graphics.UI.SDL as SDL
import          System.Environment (getArgs)
import          System.Exit

-- Internal modules import
import          Game
import          Game.State
import          Resources
import          Settings


-- | Application exit function
doExit = do
    SDL.quit
    exitWith ExitSuccess


-- | Main loop
--
gameLoop :: StateT GameState IO ()
gameLoop = do
    gstate <- get

    let
      akeys = activeKeys gstate

    -- Save start tick
    curr_ticks <- liftIO $ SDL.getTicks
    ev         <- liftIO $ pollEvent

    case ev of
      Quit                        -> liftIO $ doExit
      KeyDown (Keysym SDLK_q _ _) -> liftIO $ doExit -- @fixme fast exit
      -- Set the key pressed
      KeyDown (Keysym      k _ _) -> modify $ (\s -> s
                                        { activeKeys = k : akeys
                                        , inputAck   = True
                                        })
      -- Clear pressed key
      KeyUp   (Keysym      k _ _) -> modify $ (\s -> s
                                        { activeKeys = filter (/= k) akeys
                                        , inputAck   = True
                                        })
      _                           -> return ()

    -- Set current tick counter
    modify $ (\s -> s
      { ticksPrev = (ticksCurr gstate)
      , ticksCurr = curr_ticks
      })

    liftIO $ print $ "Before: " ++ show (currStep gstate)

    -- Process the game state machine
    Game.updateState


    liftIO $ print $ "After: " ++ show (currStep gstate)

    -- Flip the final surface
    liftIO $ SDL.flip $ screen gstate

    gstate' <- get

    liftIO $ print (activeKeys gstate')

    -- Reset game state
    put $ gstate' { inputAck    = False
                  }

    liftIO $ print $ "ticksPrev: " ++ show (ticksPrev gstate')
    liftIO $ print $ "ticksCurr: " ++ show (ticksCurr gstate')
    liftIO $ do { ticks <- SDL.getTicks; print ticks; SDL.delay $ 5 }

    gameLoop


-- | The entrypoint of application
--
main :: IO ()
main = do
    -- @todo add cmdline params parsing like:
    -- fullscreen, SOD/Classic/Demo, Data path, etc.
    args <- getArgs

    -- Load game palette & signon screen
    so  <- loadSignon
    pal <- loadPalette

    -- Initialize SDL system
    SDL.init [InitAudio, InitVideo]

    screen <- SDL.setVideoMode scrWidth scrHeight scrBpp [HWSurface, HWPalette]

    -- Set the main palette

    -- @todo Currently we have a limitation with bpp incompatibility.
    -- The VGA uses 6-6-6 scheme while SDL expected 8-8-8. Need to find
    -- a smart way to transpose the palette.
    _ <- SDL.setColors screen pal 0 -- @todo check for result.

    -- Initialize game state and run the main loop
    finalState <- execStateT gameLoop $
                    Game.initState { nextSteps = [IntroBegin]
                                   , screen = screen
                                   , signon = so
                                   , palette = pal
                                   }
    return ()
