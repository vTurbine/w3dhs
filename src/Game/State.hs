module Game.State where

import Graphics.UI.SDL
import Data.Word

import Resources

--
--
data GameStep = IntroBegin
              | LoadResources
              | IntroEnd
              | TitlePG13
              | TitlePage
              | Credits
              | HighScores
              | MainMenu
              | MainMenuLoop
              | WaitForInput
              | DelayMs    Word32
              | DelayMsInt Word32         -- delay interruptible by user input
              | FadeIn
              | FadeOut
              | RestartGame
              | GameLoop
              | Pause                     -- for debug purpose only
              | Empty
              deriving (Show)

-- |Holds the keys collected by player
--
data Keys = Keys     { goldenKey :: Bool
                     , silverKey :: Bool
                     }

-- |Specified selected weapon
--
data Weapon = Knife
              | Gun
              | MachineGun
              | GatlingGun
              deriving (Show, Enum)


-- |
--
type GameColor = Word8

-- |Game state record definition
--
data GameState = GameState  { currStep      :: GameStep
                            , nextSteps     :: [GameStep]
                            , ticksPrev     :: Word32
                            , ticksCurr     :: Word32      -- ticks passed since last iteration
                            , activeKeys    :: [SDLKey]    -- list of active keys
                            -- ID_US_1.C globals {
                            , windowX       :: Int
                            , windowY       :: Int
                            , windowW       :: Int
                            , windowH       :: Int
                            , printX        :: Int
                            , printY        :: Int
                            -- }
                            , fontColor     :: GameColor
                            , backColor     :: GameColor
                            , inputAck      :: Bool        -- is any input event occured
                            , viewWidth     :: Int
                            , viewHeight    :: Int
                            -- gamestate
                            , score         :: Int
                            , lives         :: Int
                            , health        :: Int
                            , ammo          :: Int
                            , keys          :: Keys
                            , weapon        :: Weapon
                            , godMode       :: Bool
                            , timeCount     :: Int
                            , secretTotal   :: Int
                            , killTotal     :: Int
                            , treasureTotal :: Int
                            , secretCount   :: Int
                            , killCount     :: Int
                            , treasureCount :: Int
                            -- variant configuration
                            , isSpear       :: Bool
                            , died          :: Bool
                            , startGame     :: Bool
                            , loadedGame    :: Bool
                            , inGame        :: Bool       -- Flag set by game indicating if a game is in progress
                            --
                            , screen        :: Surface
                            , palette       :: [Color]
                            , gameData      :: GameData
                            }
