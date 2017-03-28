module Game.State where

import Graphics.UI.SDL
import Data.Word

import Defs
import Resources

--
--
data GameStep = IntroBegin
              | LoadResources             -- load and cache game resources
              | DoJukeBox
              | InitGame                  -- initialize game tables
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
data ActiveType = AcBadObject
                | AcNo
                | AcYes
                | AcAlways
                deriving (Show, Enum)

-- |
--
type GameColor = Word8

-- |Thinking actor structure
--
data ObjType = ObjType { activ      :: ActiveType
                       , angle      :: Int
                       , hitcount   :: Int
                       }

-- |Game state record definition
--
data GameState = GameState  { buildVariant  :: BuildVariant
                            , currStep      :: GameStep
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
                            -- graphics globals
                            , screenFaded   :: Bool
                            --
                            , screen        :: Surface
                            , palette       :: [Color]  -- initial palette loaded from resources
                            , paletteLast   :: [Color]  -- last palette set by `vlFillPalette`
                            , gameData      :: GameData
                            }
