{-
 - Input handing
 -}

module Game.Input
    ( inAck
    ) where


import Control.Monad.Trans          (liftIO)
import Control.Monad.Trans.State
import Graphics.UI.SDL as SDL
import System.Exit                  (exitWith, ExitCode(..))

-- Internal modules import
import Game.State


inputAckDelay = 3  -- delay between input device scans


-- | Exit to the OS
-- FIXME: it's already implemented in `Main`. Need to get
-- rid of duplication
--
doExit :: IO ()
doExit = SDL.quit >> exitWith ExitSuccess


-- | Check pressed buttons
-- TODO: mouse handling
--
inCheckAck :: StateT GameState IO Bool
inCheckAck = do
  gstate <- get

  let
    akeys = activeKeys gstate

  ev <- liftIO $ pollEvent

  case ev of
    Quit                        ->  do
                                      liftIO $ doExit
                                      return True

    KeyDown (Keysym SDLK_q _ _) ->  do
                                      liftIO $ doExit -- FIXME: fast exit (for test only)
                                      return True

    -- Set the key pressed
    KeyDown (Keysym      k _ _) ->  do
                                      modify $ (\s -> s
                                        { activeKeys = k : akeys
                                        , inputAck   = True
                                        })
                                      return True

    -- Clear pressed key
    KeyUp   (Keysym      k _ _) ->  do
                                      modify $ (\s -> s
                                        { activeKeys = filter (/= k) akeys
                                        , inputAck   = True
                                        })
                                      return True

    _                           ->  return False -- only case when nothing happened


-- | Wait for any input
--
inAck :: StateT GameState IO ()
inAck = do
  ack <- inCheckAck

  if ack
     then return ()
     else (liftIO $ SDL.delay inputAckDelay) >> inAck
