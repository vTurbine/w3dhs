module Game
    ( vwb_Bar
    , setSurfaceData
    , GameState(..)
    , initState
    , updateState
    , setFontColor
    , us_CPrint
    ) where

import Control.Monad.Trans.State
import Data.Word
import Graphics.UI.SDL

import Game.Graphics
import Game.Intro
import Game.State
import Game.Text


-- |Initializes the game state
-- @todo probably it's better to run all state-related calculations
-- in environment/state monad
--
initState :: GameState
initState = GameState { step        = IntroScreen
                      , printX      = 0
                      , printY      = 0
                      , fontColor   = 0
                      , backColor   = 0
                      , screen      = undefined
                      }

-- |Updates the game state
-- Implements main game FSM
--
updateState :: StateT GameState IO ()
updateState = do
    gst <- get
    case (step gst) of
        -- draw [Intro Screen]
        IntroScreen -> Game.Intro.introScreen
        _           -> return ()
