{-
 |
-}

module Game
    ( GameState(..)
    , initState
    , updateState
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

                      -- [Graphics] section
                      , screenFaded = False
                      }


-- | Updates the game state
-- Implements main game FSM
-- @todo simplify to "next step"
--
updateState :: StateT GameState IO ()
updateState = do
    gstate <- get

    let
      delta = (ticksCurr gstate) - (ticksPrev gstate)
      delay e t = if delta > t
                  then return ()
                  else put $ gstate
                             { nextSteps = e (t - delta) : (nextSteps gstate)
                             }

    case (currStep gstate) of
        -- cache game resources
        -- initialize game tables
        InitGame      -> Game.Loop.initGame
        --
        WaitForInput  -> if (not $ inputAck gstate)
                         then put $ gstate
                                { nextSteps = WaitForInput : (nextSteps gstate)
                                }
                         else return ()
        --
        DelayMs ms    -> delay DelayMs ms

        DelayMsInt ms -> if inputAck gstate
                         then return ()
                         else delay DelayMsInt ms
        --
        MainMenu      -> Game.Menu.mainMenu
        MainMenuLoop  -> Game.Menu.mainMenuLoop
        RestartGame   -> Game.Loop.restartGame
        GameLoop      -> Game.Loop.gameLoop
        Pause         -> put $ gstate
                                { nextSteps = [Pause]
                                }
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


-- |
--
-- runWhen :: @todo
