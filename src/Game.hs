{-
 |
-}

module Game
    ( GameState(..)
    , initState
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.State

import Game.State
import Game.Loop
import Game.Menu
import Resources

-- |Initializes the game state
-- @todo probably it's better to run all state-related calculations
-- in environment/state monad
--
initState :: GameState
initState = GameState { buildVariant= undefined     -- should be set on init
                      , currStep    = Empty
                      , nextSteps   = [Empty]
                      , ticksPrev   = 0
                      , ticksCurr   = 0
                      , activeKeys  = []
                      , windowX     = 0
                      , windowY     = 0
                      , windowW     = 0
                      , windowH     = 0
                      , printX      = 0
                      , printY      = 0
                      , fontColor   = 0
                      , backColor   = 0
                      , inputAck    = False
                      , viewWidth   = 256
                      , viewHeight  = 144
                      , fontNumber  = 0

                      -- gamestate data
                      -- , difficulty  = 0
                      -- , oldscore    = 0
                      , score       = 0
                      -- , nextextra   = 0
                      , lives       = 0
                      , health      = 0
                      , ammo        = 0
                      , keys        = Keys  { goldenKey = False
                                            , silverKey = False
                                            }
                      , weapon      = Knife
                      -- , faceframe   = 0
                      -- , attackframe = 0
                      -- , attackcount = 0
                      -- , weaponframe = 0
                      -- , episode     = 0
                      --
                      , timeCount     = undefined -- this group should be set
                      , secretTotal   = undefined -- in `setupGameLevel`
                      , secretCount   = undefined
                      , killTotal     = undefined
                      , treasureTotal = undefined
                      , killCount     = undefined
                      , treasureCount = undefined
                      --
                      -- , killx       = 0
                      -- , killy       = 0
                      -- , victoryflag = False
                      , godMode     = False
                      , isSpear     = False
                      , startGame   = False
                      , loadedGame  = False
                      , inGame      = False
                      , died        = undefined -- should be set in `RestartGame`
                      , screen      = undefined
                      , gameData    = undefined
                      , palette     = undefined
                      , paletteLast = undefined
                      -- [Graphics] section
                      , screenFaded = False
                      }
