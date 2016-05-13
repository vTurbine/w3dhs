module Game
    ( vwb_Bar
    , setSurfaceData
    , GameState(..)
    , initState
    , updateState
    , setFontColor
    , us_CPrint
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Word
import Graphics.UI.SDL as SDL

import Game.Graphics
import Game.Intro
import Game.Title
import Game.State
import Game.Text
import Resources

-- |Initializes the game state
-- @todo probably it's better to run all state-related calculations
-- in environment/state monad
--
initState :: GameState
initState = GameState { currStep    = Empty
                      , nextSteps   = [Empty]
                      , activeKeys  = []
                      , windowX     = 0
                      , windowY     = 0
                      , printX      = 0
                      , printY      = 0
                      , fontColor   = 0
                      , backColor   = 0
                      , inputAck    = False
                      , screen      = undefined
                      , gameData    = undefined
                      , signon      = undefined
                      , palette     = undefined
                      }


-- |Updates the game state
-- Implements main game FSM
-- @todo simplify to "next step"
--
updateState :: StateT GameState IO ()
updateState = do
    gstate <- get

    case (currStep gstate) of
        -- draw [Intro Screen]
        IntroBegin   -> Game.Intro.introScreen_begin
        --
        LoadResources-> do
                          gdata <- liftIO $ loadGameData
                          put $ gstate { gameData  = gdata }
        --
        IntroEnd     -> Game.Intro.introScreen_end
        --
        WaitForInput -> if (not $ inputAck gstate)
                        then put $ gstate { nextSteps = WaitForInput : (nextSteps gstate) }
                        else return ()
        --
        DelayMs ms   -> liftIO $ SDL.delay ms;
        --
        TitlePG13    -> Game.Title.pg13
        --
        TitlePage    -> Game.Title.titlePage
        _            -> return ()

    -- update the state
    gstate <- get

    let
      nsteps = nextSteps gstate
      nstep  = head $ nsteps

    -- proceed to the next step
    put $ gstate { currStep  = nstep
                 , nextSteps = drop 1 nsteps
		 }
