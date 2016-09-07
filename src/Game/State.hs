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


-- |Game state record definition
--
data GameState = GameState  { currStep      :: GameStep
                            , nextSteps     :: [GameStep]
                            , ticksPrev     :: Word32
                            , ticksCurr     :: Word32      -- ticks passed since last iteration
                            , activeKeys    :: [SDLKey]    -- list of active keys
                            , windowX       :: Int
                            , windowY       :: Int
                            , printX        :: Int
                            , printY        :: Int
                            , fontColor     :: Int
                            , backColor     :: Int
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
                            , screen        :: Surface
                            , signon        :: [Word8]
                            , palette       :: [Color]
                            , gameData      :: GameData
                            -- variant configuration
                            , isSpear       :: Bool
                            , died          :: Bool
                            , startGame     :: Bool
                            , loadedGame    :: Bool
                            , inGame        :: Bool       -- Flag set by game indicating if a game is in progress
                            }
