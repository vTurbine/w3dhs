{-
 - Input handing
 -}

module Game.Input
    ( inAck
    , inUserInput
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

  ev <- liftIO $ SDL.pollEvent

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


-- | Clears the event queue
--
inStartAck :: StateT GameState IO ()
inStartAck = do
  ev <- liftIO $ SDL.pollEvent

  if ev == NoEvent
     then return ()
     else inStartAck


-- | Waits for any input
--
inAck :: StateT GameState IO ()
inAck = do

  inStartAck

  inAckHlp
    where
      inAckHlp = do
        ack <- inCheckAck

        if ack
          then return ()
          else (liftIO $ SDL.delay inputAckDelay) >> inAckHlp


-- | Waits for the specified delay time (in ms) or the user
--   pressing a key or a mouse button
--
inUserInput :: Int -> StateT GameState IO Bool
inUserInput ms = do

  inStartAck

  ticksFirst <- liftIO $ SDL.getTicks

  inUserInputHlp ticksFirst
    where
      inUserInputHlp ts = do
        ticksLast <- liftIO $ SDL.getTicks

        if (fromIntegral (ticksLast - ts) >= ms)
          then return False
          else do
            ack <- inCheckAck

            if ack
               then return True
               else inUserInputHlp ts
