{-
 - Main module implementation.
 -}

module Main
  ( main
  ) where

import Control.Monad.Trans        (liftIO)
import Control.Monad.Trans.State
import Graphics.UI.SDL as SDL
import System.Environment         (getArgs)
import System.Exit                (exitWith, ExitCode(..))

-- Internal modules import
import Defs
import Game
import Game.Intro
import Game.Graphics              (vwFadeOut)
import Game.Loop                  (gameLoop)
import Game.Menu                  (usControlPanel)
import Game.Title                 (pg13, titleLoop)
import Game.State
import Resources                  (loadPalette, loadGameData)
import Resources.Configuration    (GameConfig(..), readConfig)
import Settings


-- | Returns build variant in accordance to cmdline arguments
--
getBuildVariant :: BuildVariant
getBuildVariant = BuildUpload -- TODO


-- | Exit to the OS
--
doExit :: IO ()
doExit = SDL.quit >> exitWith ExitSuccess


-- | Application exit function
--
quit :: String -> StateT GameState IO ()
quit s  = do

  -- writeConfig

  liftIO $ putStrLn s
  liftIO $ doExit


-- | Flip surfaces to show the result of drawing
--
showScr :: StateT GameState IO () -> StateT GameState IO ()
showScr f = do
  f
  gstate <- get
  liftIO $ SDL.flip $ screen gstate


-- |
--
newViewSize :: Int -> StateT GameState IO ()
newViewSize vs = do
  return ()


-- | Init resources and game state
--
initGame :: StateT GameState IO ()
initGame = do
  gstate <- get

  let
    v = buildVariant gstate

  showScr $ signonScreen

  -- read input


  -- readConfig
  cfg <- liftIO $ readConfig v
  gd  <- liftIO $ loadGameData v

  modify (\s -> s { config   = cfg
                  , gameData = gd
                  })

  -- doJukeBox
  --

  showScr $ introScreen

  -- build tables
  newViewSize (viewSize cfg)
  -- red shifts
  --
  showScr $ finishSignon


-- | Main loop
--
demoLoop :: StateT GameState IO ()
demoLoop = do
  gstate <- get

  -- check for `noWait`
  -- nonShareware
  --
  -- copyProtection
  --

  --startCpMusic INTROSONG

  -- check for `noWait`
  pg13

  titleLoop

  -- check for debug mode

  usControlPanel Nothing

--  if (startgame || loadedgame)
  gameLoop
  vwFadeOut
--  startCpMusic INTROSONG

  demoLoop


-- | `main` routine in original game
--
gameMain :: StateT GameState IO ()
gameMain = initGame >> demoLoop >> Main.quit "Demo loop exited???"


-- | The entrypoint of application
--
main :: IO ()
main = do
    -- TODO: add cmdline params parsing like:
    -- fullscreen, SOD/Classic/Demo, Data path, etc.
    args <- getArgs

    -- Load game palette
    pal <- loadPalette

    -- Initialize SDL system
    SDL.init [InitAudio, InitVideo]

    screen <- SDL.setVideoMode scrWidth scrHeight scrBpp [HWSurface, HWPalette]

    -- Set the main palette

    -- TODO: Currently we have a limitation with bpp incompatibility.
    -- The VGA uses 6-6-6 scheme while SDL expected 8-8-8. Need to find
    -- a smart way to transpose the palette.
    _ <- SDL.setColors screen pal 0 -- TODO: check for result

    -- Initialize game state and run the main loop
    finalState <- execStateT gameMain $
      Game.initState { buildVariant = getBuildVariant
                     , screen       = screen
                     , palette      = pal
                     , paletteLast  = pal
                     }
    return ()
