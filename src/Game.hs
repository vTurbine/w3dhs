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
                      , nextStep    = Empty
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
                      }


-- |Updates the game state
-- Implements main game FSM
--
updateState :: StateT GameState IO ()
updateState = do
    gstate <- get

    let
      nstep = nextStep gstate

    case (currStep gstate) of
        -- draw [Intro Screen]
        IntroScreen  -> Game.Intro.introScreen
        WaitForInput -> if (inputAck gstate)
                        then put $ gstate { currStep = nstep
                                          , nextStep = Empty
                                          }
                        else return ()
        DelayMs ms   -> do {
                          liftIO $ SDL.delay ms; put $ gstate { currStep = nstep
                                                              , nextStep = Empty
                                                              }
                        }
        TitlePG13    -> Game.Title.pg13
        TitlePage    -> Game.Title.titlePage
        _            -> return ()
