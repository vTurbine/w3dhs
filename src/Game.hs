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
import Game.Loop
import Resources

-- |Initializes the game state
-- @todo probably it's better to run all state-related calculations
-- in environment/state monad
--
initState :: GameState
initState = GameState { currStep    = Empty
                      , nextSteps   = [Empty]
                      , ticksPrev   = 0
                      , ticksCurr   = 0
                      , activeKeys  = []
                      , windowX     = 0
                      , windowY     = 0
                      , printX      = 0
                      , printY      = 0
                      , fontColor   = 0
                      , backColor   = 0
                      , inputAck    = False
                      , viewWidth   = 256
                      , viewHeight  = 144
                      , screen      = undefined
                      , gameData    = undefined
                      , signon      = undefined
                      , palette     = undefined
                      }


-- | Updates the game state
-- Implements main game FSM
-- @todo simplify to "next step"
--
updateState :: StateT GameState IO ()
updateState = do
    gstate <- get

    case (currStep gstate) of
        -- draw [Intro Screen]
        IntroBegin    -> Game.Intro.introScreen_begin
        --
        LoadResources -> do
                          gdata <- liftIO $ loadGameData
                          put $ gstate { gameData  = gdata }
        --
        IntroEnd      -> Game.Intro.introScreen_end
        --
        WaitForInput  -> if (not $ inputAck gstate)
                         then put $ gstate
                                { nextSteps = WaitForInput : (nextSteps gstate)
                                }
                         else return ()
        --
        DelayMs ms    -> if delta > ms
                         then return ()
                         else put $ gstate
                                { nextSteps = DelayMs (ms - delta) : (nextSteps gstate)
                                }
                            where
                              delta = (ticksCurr gstate) - (ticksPrev gstate)

        DelayMsIntr ms-> return ()
        --
        TitlePG13     -> Game.Title.pg13
        --
        TitlePage     -> Game.Title.titlePage
        --
        Credits       -> Game.Title.creditsPage
        GameLoop      -> Game.Loop.gameLoop
        _             -> return ()

    -- update the state
    gstate <- get

    let
      nsteps = nextSteps gstate
      nstep  = head $ nsteps

    -- proceed to the next step
    put $ gstate { currStep  = nstep
                 , nextSteps = drop 1 nsteps
                 }
